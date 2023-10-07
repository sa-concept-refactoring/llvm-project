//===--- TransformConcept.cpp --------------------------------------*- C++-*-===//
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
#include "clang/AST/Stmt.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
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

  auto getTemplateParameterIndexOfTemplateArgument(const TemplateArgument &TemplateArgument) -> std::optional<int>;
  auto generateRequiresReplacement(SourceManager&, ASTContext&) -> tooling::Replacement;

  template <typename T, typename NodeKind>
  static auto findNode(const SelectionTree::Node &Root) -> const T*;

  template <typename T>
  static auto findExpression(const SelectionTree::Node &Root) -> const T* {
    return findNode<T, Expr>(Root);
  }

  template <typename T>
  static auto findDeclaration(const SelectionTree::Node &Root) -> const T* {
    return findNode<T, Decl>(Root);
  }
};

REGISTER_TWEAK(TransformConcept)

// TODO: Extract some helper methods
bool TransformConcept::prepare(const Selection &Inputs) {
  const auto *Root = Inputs.ASTSelection.commonAncestor();
  if (!Root) return false;

  ConceptSpecializationExpression = findExpression<ConceptSpecializationExpr>(*Root);
  if (!ConceptSpecializationExpression) return false;

//  TODO: Bring this logic back, it got lost with the refactoring of the find method, maybe we need to revert the commit that introduced this todo or we add proper support for logical combinations
//  if (Expression && Node && isa_and_nonnull<FunctionTemplateDecl>(Node->Parent->ASTNode.get<Decl>())) {
//    return Expression;
//  }

  FunctionTemplateDeclaration = findDeclaration<FunctionTemplateDecl>(*Root);
  if (!FunctionTemplateDeclaration) return false;

  auto TemplateArguments = ConceptSpecializationExpression->getTemplateArguments();
  if (TemplateArguments.size() != 1) return false;

  auto TemplateParameterIndex = getTemplateParameterIndexOfTemplateArgument(TemplateArguments[0]);
  if (!TemplateParameterIndex) return false;

  TemplateTypeParameterDeclaration = dyn_cast_or_null<TemplateTypeParmDecl>(
    FunctionTemplateDeclaration->getTemplateParameters()->getParam(*TemplateParameterIndex));
  if (!TemplateTypeParameterDeclaration->wasDeclaredWithTypename()) return false;

  RequiresExpr = FunctionTemplateDeclaration->getAsFunction()->getTrailingRequiresClause();

  return true;
}

Expected<Tweak::Effect> TransformConcept::apply(const Selection &Inputs) {
  auto &Context = Inputs.AST->getASTContext();
  auto &SourceManager = Inputs.AST->getSourceManager();

  tooling::Replacements Replacements{};

  auto ConceptName = ConceptSpecializationExpression->getNamedConcept()->getQualifiedNameAsString();
  auto TypeSourceRange = TemplateTypeParameterDeclaration->getSourceRange();
  auto SourceRangeSize =
      SourceManager.getFileOffset(TypeSourceRange.getEnd()) -
      SourceManager.getFileOffset(TypeSourceRange.getBegin());
  auto TypeReplacement = tooling::Replacement(
      Context.getSourceManager(), TypeSourceRange.getBegin(), SourceRangeSize, ConceptName + ' ');

  if (auto Err = Replacements.add(TypeReplacement)) {
    return Err;
  }

  // Replace requirement clause with empty string
  // TODO: Only do this if there are no further require clauses
  // TODO: Remove commented code
  //  auto RequiresRng = toHalfOpenFileRange(SourceManager, Context.getLangOpts(), RequiresExpr->getSourceRange());
  //  if (!RequiresRng) {
  //    return error("Could not obtain range of the 'requires' branch. Macros?");
  //  }
  //
  //  auto RequiresCode = toSourceCode(SourceManager, *RequiresRng);
  //
  //  auto RequirementReplacement = tooling::Replacement(Context.getSourceManager(), RequiresRng->getBegin(), RequiresCode.size(), std::string{});
  if (auto Err = Replacements.add(generateRequiresReplacement(SourceManager, Context))) {
    return Err;
  }

  auto &AST = Inputs.AST;
  auto &TokenBuffer = AST->getTokens();
  auto &NewSourceManager = TokenBuffer.sourceManager();
  // Tried to get token with find instead of for loop
  //  auto Tokens = TokenBuffer.expandedTokens(FunctionTemplateDeclaration->getAsFunction()->getSourceRange());
  //  const auto *Itr = llvm::find_if(Tokens, [&](const clang::syntax::Token Symbol) {
  //    return Symbol.kind() == tok::kw_requires;
  //  });
  //
  //  if (Itr != Tokens.end()) {
  //    size_t Idx = std::distance(Tokens.begin(), Itr);
  //
  //    auto Token = Tokens[Idx];
  //    auto Spelling = TokenBuffer.spelledForExpanded(llvm::ArrayRef(Token));
  //    auto DeletionRange =
  //        syntax::Token::range(NewSourceManager, Spelling->front(),
  //                             Spelling->back())
  //            .toCharRange(NewSourceManager);
  //
  //    if (auto Err = Replacements.add(
  //            tooling::Replacement(NewSourceManager, DeletionRange, ""))) {
  //      return Err;
  //    }
  //  }

  for (const auto &Token : TokenBuffer.expandedTokens(FunctionTemplateDeclaration->getAsFunction()->getSourceRange())) {
    if (Token.kind() != tok::kw_requires) {
      continue;
    }

    auto Spelling = TokenBuffer.spelledForExpanded(llvm::ArrayRef(Token));
    auto DeletionRange = syntax::Token::range(NewSourceManager, Spelling->front(), Spelling->back()).toCharRange(NewSourceManager);

    if (auto Err = Replacements.add(tooling::Replacement(NewSourceManager, DeletionRange, ""))) {
      return Err;
    }
  }

  return Effect::mainFileEdit(SourceManager, Replacements);
}

auto TransformConcept::getTemplateParameterIndexOfTemplateArgument(const TemplateArgument &TemplateArgument) -> std::optional<int> {
  if (TemplateArgument.getKind() != TemplateArgument.Type) return {};

  auto TemplateArgumentType = TemplateArgument.getAsType();
  if (!TemplateArgumentType->isTemplateTypeParmType()) return {};

  const auto *TemplateTypeParameterType = TemplateArgumentType->getAs<TemplateTypeParmType>();
  if (!TemplateTypeParameterType) return {};

  return TemplateTypeParameterType->getIndex();
}

auto TransformConcept::generateRequiresReplacement(SourceManager& SourceManager, ASTContext& Context) -> tooling::Replacement
{
  auto RequiresRng = toHalfOpenFileRange(SourceManager, Context.getLangOpts(), RequiresExpr->getSourceRange());
  if (!RequiresRng) {
    // TODO: Manage error
    //return error("Could not obtain range of the 'requires' branch. Macros?");
  }

  auto RequiresCode = toSourceCode(SourceManager, *RequiresRng);

  return tooling::Replacement(Context.getSourceManager(), RequiresRng->getBegin(), RequiresCode.size(), std::string{});
}

template <typename T, typename NodeKind>
auto TransformConcept::findNode(const SelectionTree::Node &Root) -> const T * {
  const T* Result = nullptr;

  const SelectionTree::Node *Node = &Root;
  for (; Node && !Result; Node = Node->Parent) {
    Result = dyn_cast_or_null<T>(Node->ASTNode.get<NodeKind>());
  }

  return Result;
}

} // namespace
} // namespace clangd
} // namespace clang
