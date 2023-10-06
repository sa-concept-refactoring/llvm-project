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
  const ConceptSpecializationExpr *ConceptSpecializationExpression = nullptr;
  const TemplateTypeParmDecl *TemplateTypeParameterDeclaration = nullptr;
  const Expr *RequiresExpr = nullptr;

  // TODO: Remove this
  const FunctionTemplateDecl *FunctionTemplateDeclaration = nullptr;

  // TODO: Maybe don't name these 'findX'
  auto findConceptSpecialization(const SelectionTree::Node&) -> const ConceptSpecializationExpr*;
  auto findSingleTemplateTypeParameter(const ConceptSpecializationExpr&) -> const TemplateTypeParmType*;
  auto findFunctionTemplateDeclaration(const SelectionTree::Node&) -> const FunctionTemplateDecl*;
};

REGISTER_TWEAK(TransformConcept)

// TODO: Extract some helper methods
bool TransformConcept::prepare(const Selection &Inputs) {
  const auto *Root = Inputs.ASTSelection.commonAncestor();
  if (!Root) return false;

  ConceptSpecializationExpression = findConceptSpecialization(*Root);
  if (!ConceptSpecializationExpression) return false;

  const auto *TemplateTypeParamType = findSingleTemplateTypeParameter(*ConceptSpecializationExpression);
  if (!TemplateTypeParamType) return false;

  FunctionTemplateDeclaration = findFunctionTemplateDeclaration(*Root);
  if (!FunctionTemplateDeclaration) return false;

  auto *TemplateParameter = FunctionTemplateDeclaration->getTemplateParameters()->getParam(TemplateTypeParamType->getIndex());
  TemplateTypeParameterDeclaration = dyn_cast_or_null<TemplateTypeParmDecl>(TemplateParameter);
  if (!TemplateTypeParameterDeclaration->wasDeclaredWithTypename()) {
    return false;
  }

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
  auto RequiresRng = toHalfOpenFileRange(SourceManager, Context.getLangOpts(), RequiresExpr->getSourceRange());
  if (!RequiresRng) {
    return error("Could not obtain range of the 'requires' branch. Macros?");
  }

  auto RequiresCode = toSourceCode(SourceManager, *RequiresRng);

  auto RequirementReplacement = tooling::Replacement(Context.getSourceManager(), RequiresRng->getBegin(), RequiresCode.size(), std::string{});
  if (auto Err = Replacements.add(RequirementReplacement)) {
    return Err;
  }

  auto &AST = Inputs.AST;
  auto &TokenBuffer = AST->getTokens();
  auto &NewSourceManager = TokenBuffer.sourceManager();

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

// TODO: Make this cleaner
auto clang::clangd::TransformConcept::findConceptSpecialization(const SelectionTree::Node &Root) -> const ConceptSpecializationExpr * {
  const ConceptSpecializationExpr *Expression = nullptr;

  const SelectionTree::Node *Node = &Root;
  for (; Node && !Expression; Node = Node->Parent) {
    Expression = dyn_cast_or_null<ConceptSpecializationExpr>(Node->ASTNode.get<Expr>());
  }

  // For now we only support single concept specializations, no combined ones.
  // TODO: Improve comment, make it more clear that we mean '&&' and '||'
  if (Expression && Node && isa_and_nonnull<FunctionTemplateDecl>(Node->Parent->ASTNode.get<Decl>())) {
    return Expression;
  }

  return nullptr;
}

auto clang::clangd::TransformConcept::findSingleTemplateTypeParameter(const ConceptSpecializationExpr &ConceptSpecialization) -> const TemplateTypeParmType * {
  auto TemplateArguments = ConceptSpecialization.getSpecializationDecl()->getTemplateArguments();
  if (TemplateArguments.size() != 1) {
    return nullptr;
  }

  const auto &TemplateArgument = &TemplateArguments[0];
  if (TemplateArgument->getKind() != TemplateArgument->Type) {
    return nullptr;
  }

  auto TemplateArgumentType = TemplateArgument->getAsType();
  if (!TemplateArgumentType->isTemplateTypeParmType()) {
    return nullptr;
  }

  return TemplateArgumentType->getAs<TemplateTypeParmType>();
}

auto clang::clangd::TransformConcept::findFunctionTemplateDeclaration(const SelectionTree::Node &Root) -> const FunctionTemplateDecl * {
  const FunctionTemplateDecl *FunctionTemplateDeclaration = nullptr;

  for (const SelectionTree::Node *N = Root.Parent; N && !FunctionTemplateDeclaration; N = N->Parent) {
    FunctionTemplateDeclaration = dyn_cast_or_null<FunctionTemplateDecl>(N->ASTNode.get<Decl>());
  }

  return FunctionTemplateDeclaration;
}

} // namespace
} // namespace clangd
} // namespace clang
