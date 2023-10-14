//===--- TransformConcept.cpp ------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "ParsedAST.h"
#include "SourceCode.h"
#include "refactor/Tweak.h"
#include "support/Logger.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ExprConcepts.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Tooling/Core/Replacement.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Error.h"

namespace clang {
namespace clangd {
namespace {
/// Transforms a concept
class TransformConcept : public Tweak {
public:
  const char *id() const final;

  bool prepare(const Selection &Inputs) override;
  Expected<Effect> apply(const Selection &Inputs) override;
  std::string title() const override { return "Transform concept"; }
  llvm::StringLiteral kind() const override {
    return CodeAction::REFACTOR_KIND;
  }

private:
  // TODO: Investigate if we can reduce how much data we store
  const ConceptSpecializationExpr *ConceptSpecializationExpression = nullptr;
  const TemplateTypeParmDecl *TemplateTypeParameterDeclaration = nullptr;
  const Expr *RequiresExpr = nullptr;
  const FunctionTemplateDecl *FunctionTemplateDeclaration = nullptr;
  const syntax::Token *RequiresToken = nullptr;

  static auto getTemplateParameterIndexOfTemplateArgument(
      const TemplateArgument &TemplateArgument) -> std::optional<int>;
  auto generateRequiresReplacement(ASTContext &)
      -> std::variant<tooling::Replacement, llvm::Error>;
  auto generateRequiresTokenReplacement(const syntax::TokenBuffer &)
      -> tooling::Replacement;
  auto generateTemplateParameterReplacement(ASTContext &Context)
      -> tooling::Replacement;

  static auto findToken(const ParsedAST *, const SourceRange &,
                        const tok::TokenKind) -> const syntax::Token *;

  template <typename T, typename NodeKind>
  static auto findNode(const SelectionTree::Node &Root) -> const T *;

  template <typename T>
  static auto findExpression(const SelectionTree::Node &Root) -> const T * {
    return findNode<T, Expr>(Root);
  }

  template <typename T>
  static auto findDeclaration(const SelectionTree::Node &Root) -> const T * {
    return findNode<T, Decl>(Root);
  }
};

REGISTER_TWEAK(TransformConcept)

bool TransformConcept::prepare(const Selection &Inputs) {
  if (!Inputs.AST->getLangOpts().CPlusPlus20)
    return false;

  const auto *Root = Inputs.ASTSelection.commonAncestor();
  if (!Root)
    return false;

  if (!(ConceptSpecializationExpression =
            findExpression<ConceptSpecializationExpr>(*Root))) {
    return false;
  }

  //  TODO: Bring this logic back, it got lost with the refactoring of the find
  //  method, maybe we need to revert the commit that introduced this todo or we
  //  add proper support for logical combinations if (Expression && Node &&
  //  isa_and_nonnull<FunctionTemplateDecl>(Node->Parent->ASTNode.get<Decl>()))
  //  {
  //    return Expression;
  //  }

  if (!(FunctionTemplateDeclaration =
            findDeclaration<FunctionTemplateDecl>(*Root))) {
    return false;
  }

  auto TemplateArguments =
      ConceptSpecializationExpression->getTemplateArguments();
  if (TemplateArguments.size() != 1)
    return false;

  auto TemplateParameterIndex =
      getTemplateParameterIndexOfTemplateArgument(TemplateArguments[0]);
  if (!TemplateParameterIndex)
    return false;

  TemplateTypeParameterDeclaration = dyn_cast_or_null<TemplateTypeParmDecl>(
      FunctionTemplateDeclaration->getTemplateParameters()->getParam(
          *TemplateParameterIndex));
  if (!TemplateTypeParameterDeclaration->wasDeclaredWithTypename())
    return false;

  const auto *Function = FunctionTemplateDeclaration->getAsFunction();

  RequiresExpr = Function->getAsFunction()->getTrailingRequiresClause();

  if (!(RequiresToken = findToken(Inputs.AST, Function->getSourceRange(),
                                  tok::kw_requires))) {
    return false;
  }

  return true;
}

Expected<Tweak::Effect> TransformConcept::apply(const Selection &Inputs) {
  auto &Context = Inputs.AST->getASTContext();
  auto &TokenBuffer = Inputs.AST->getTokens();

  tooling::Replacements Replacements{};

  if (auto Err =
          Replacements.add(generateTemplateParameterReplacement(Context))) {
    return std::move(Err);
  }

  auto RequiresReplacement = generateRequiresReplacement(Context);

  if (std::holds_alternative<llvm::Error>(RequiresReplacement)) {
    return std::move(std::get<llvm::Error>(RequiresReplacement));
  }

  if (auto Err = Replacements.add(
          std::get<tooling::Replacement>(RequiresReplacement))) {
    return std::move(Err);
  }

  if (auto Err =
          Replacements.add(generateRequiresTokenReplacement(TokenBuffer))) {
    return std::move(Err);
  }

  auto Effect = Effect::mainFileEdit(Context.getSourceManager(), Replacements);
  if (auto Err = Effect.takeError())
    return Err;

  Effect->FormatEdits = false;
  return Effect;
}

auto TransformConcept::getTemplateParameterIndexOfTemplateArgument(
    const TemplateArgument &TemplateArgument) -> std::optional<int> {
  if (TemplateArgument.getKind() != TemplateArgument.Type)
    return {};

  auto TemplateArgumentType = TemplateArgument.getAsType();
  if (!TemplateArgumentType->isTemplateTypeParmType())
    return {};

  const auto *TemplateTypeParameterType =
      TemplateArgumentType->getAs<TemplateTypeParmType>();
  if (!TemplateTypeParameterType)
    return {};

  return TemplateTypeParameterType->getIndex();
}

auto TransformConcept::generateRequiresReplacement(ASTContext &Context)
    -> std::variant<tooling::Replacement, llvm::Error> {
  auto &SourceManager = Context.getSourceManager();

  auto RequiresRng = toHalfOpenFileRange(SourceManager, Context.getLangOpts(),
                                         RequiresExpr->getSourceRange());
  if (!RequiresRng) {
    return error("Could not obtain range of the 'requires' branch. Macros?");
  }

  auto RequiresCode = toSourceCode(SourceManager, *RequiresRng);

  return tooling::Replacement(SourceManager, RequiresRng->getBegin(),
                              RequiresCode.size(), "");
}

auto TransformConcept::generateRequiresTokenReplacement(
    const syntax::TokenBuffer &TokenBuffer) -> tooling::Replacement {
  auto &SourceManager = TokenBuffer.sourceManager();

  auto Spelling =
      TokenBuffer.spelledForExpanded(llvm::ArrayRef(*RequiresToken));

  auto DeletionRange =
      syntax::Token::range(SourceManager, Spelling->front(), Spelling->back())
          .toCharRange(SourceManager);

  return tooling::Replacement(SourceManager, DeletionRange, "");
}

auto TransformConcept::generateTemplateParameterReplacement(ASTContext &Context)
    -> tooling::Replacement {
  auto &SourceManager = Context.getSourceManager();

  auto ConceptName = ConceptSpecializationExpression->getNamedConcept()
                         ->getQualifiedNameAsString();

  auto TemplateParameterName =
      TemplateTypeParameterDeclaration->getQualifiedNameAsString();

  auto TemplateParameterReplacement = ConceptName + ' ' + TemplateParameterName;

  auto TemplateParameterRange =
      toHalfOpenFileRange(SourceManager, Context.getLangOpts(),
                          TemplateTypeParameterDeclaration->getSourceRange());

  auto SourceCode = toSourceCode(SourceManager, *TemplateParameterRange);

  return tooling::Replacement(Context.getSourceManager(),
                              TemplateParameterRange->getBegin(),
                              SourceCode.size(), TemplateParameterReplacement);
}

auto clang::clangd::TransformConcept::findToken(const ParsedAST *AST,
                                                const SourceRange &SourceRange,
                                                const tok::TokenKind TokenKind)
    -> const syntax::Token * {
  auto &TokenBuffer = AST->getTokens();
  const auto &Tokens = TokenBuffer.expandedTokens(SourceRange);

  const auto Predicate = [TokenKind](const auto &Token) {
    return Token.kind() == TokenKind;
  };

  const auto *const It = std::find_if(Tokens.begin(), Tokens.end(), Predicate);

  if (It == Tokens.end()) {
    return nullptr;
  }

  return It;
}

template <typename T, typename NodeKind>
auto TransformConcept::findNode(const SelectionTree::Node &Root) -> const T * {
  const T *Result = nullptr;

  const SelectionTree::Node *Node = &Root;
  for (; Node && !Result; Node = Node->Parent) {
    Result = dyn_cast_or_null<T>(Node->ASTNode.get<NodeKind>());
  }

  return Result;
}

} // namespace
} // namespace clangd
} // namespace clang
