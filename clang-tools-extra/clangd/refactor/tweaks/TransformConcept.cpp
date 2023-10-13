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
  auto generateTypeReplacement(ASTContext &) -> tooling::Replacement;

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

// TODO: Extract some helper methods
bool TransformConcept::prepare(const Selection &Inputs) {
  const auto *Root = Inputs.ASTSelection.commonAncestor();
  if (!Root)
    return false;

  ConceptSpecializationExpression =
      findExpression<ConceptSpecializationExpr>(*Root);
  if (!ConceptSpecializationExpression)
    return false;

  //  TODO: Bring this logic back, it got lost with the refactoring of the find
  //  method, maybe we need to revert the commit that introduced this todo or we
  //  add proper support for logical combinations if (Expression && Node &&
  //  isa_and_nonnull<FunctionTemplateDecl>(Node->Parent->ASTNode.get<Decl>()))
  //  {
  //    return Expression;
  //  }

  FunctionTemplateDeclaration = findDeclaration<FunctionTemplateDecl>(*Root);
  if (!FunctionTemplateDeclaration)
    return false;

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

  RequiresExpr =
      FunctionTemplateDeclaration->getAsFunction()->getTrailingRequiresClause();

  // TODO: check if this logic can be extracted to a method
  // Check if `requires` token exists
  auto &AST = Inputs.AST;
  auto &TokenBuffer = AST->getTokens();

  const auto &Tokens = TokenBuffer.expandedTokens(
      FunctionTemplateDeclaration->getAsFunction()->getSourceRange());

  const auto *const It =
      std::find_if(Tokens.begin(), Tokens.end(), [](const auto &Token) {
        return Token.kind() == tok::kw_requires;
      });

  if (It == Tokens.end()) {
    return false;
  }

  RequiresToken = It;

  return true;
}

Expected<Tweak::Effect> TransformConcept::apply(const Selection &Inputs) {
  auto &Context = Inputs.AST->getASTContext();
  auto &TokenBuffer = Inputs.AST->getTokens();

  tooling::Replacements Replacements{};

  if (auto Err =
          Replacements.add(generateTypeReplacement(Context))) {
    return std::move(Err);
  }

  auto RequiresReplacement =
      generateRequiresReplacement(Context);

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

  return Effect::mainFileEdit(Context.getSourceManager(), Replacements);
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

  // Replace requirement clause with empty string
  return tooling::Replacement(SourceManager, RequiresRng->getBegin(),
                              RequiresCode.size(), std::string{});
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

auto TransformConcept::generateTypeReplacement(ASTContext &Context)
    -> tooling::Replacement {
  auto &SourceManager = Context.getSourceManager();

  auto ConceptName = ConceptSpecializationExpression->getNamedConcept()
                         ->getQualifiedNameAsString(); // "std::integral"
  auto TypeSourceRange =
      toHalfOpenFileRange(SourceManager, Context.getLangOpts(),
                          TemplateTypeParameterDeclaration
                              ->getSourceRange()); // range of "typename T"

  auto TypeCode = toSourceCode(SourceManager, *TypeSourceRange);

  // TODO: Adjust replacement to either add `T` to `ConceptName` or only replace
  // `typename` instead of `typename T`
  return tooling::Replacement(Context.getSourceManager(),
                              TypeSourceRange->getBegin(), TypeCode.size(),
                              ConceptName);

  // TODO: Remove old logic one the new code above is working correctly
  //  auto SourceRangeSize =
  //      SourceManager.getFileOffset(TypeSourceRange.getEnd()) -
  //      SourceManager.getFileOffset(TypeSourceRange.getBegin());

  //  return  tooling::Replacement(
  //      Context.getSourceManager(),
  //      TypeSourceRange.getBegin(),
  //      SourceRangeSize,
  //      ConceptName + ' ');
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
