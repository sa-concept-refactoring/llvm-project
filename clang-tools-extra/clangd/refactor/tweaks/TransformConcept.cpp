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
  const TemplateTypeParmDecl *TemplateTypeParameterDeclaration;
  const Expr *RequiresExpr = nullptr;
};

REGISTER_TWEAK(TransformConcept)

// TODO: Extract some helper methods
bool TransformConcept::prepare(const Selection &Inputs) {
  const auto &Node = Inputs.ASTSelection.commonAncestor()->Parent; // TODO: Check why we need the parent
  const auto &Expression = Node->ASTNode.get<Expr>(); // TODO: Check if we should do error handling here

  ConceptSpecializationExpression = dyn_cast_or_null<ConceptSpecializationExpr>(Expression);
  if (ConceptSpecializationExpression == nullptr) {
    return false;
  }

  // TODO: Check if the concept specialization is the combination of two others
  // For example 'integral<T> && foo<t>' or 'integral<T> || foo<T>'

  auto TemplateArguments = ConceptSpecializationExpression->getSpecializationDecl()->getTemplateArguments();
  if (TemplateArguments.size() != 1) {
    return false;
  }

  const auto &TemplateArgument = &TemplateArguments[0];
  if (TemplateArgument->getKind() != TemplateArgument->Type) {
    return false;
  }

  auto TemplateArgumentType = TemplateArgument->getAsType();
  if (!TemplateArgumentType->isTemplateTypeParmType()) {
    return false;
  }

  const auto &TemplateTypeParamType = TemplateArgumentType->getAs<TemplateTypeParmType>();

  const FunctionTemplateDecl *FunctionTemplateDeclaration = nullptr;
  for (const SelectionTree::Node *N = Node->Parent; N && !FunctionTemplateDeclaration; N = N->Parent) {
    FunctionTemplateDeclaration = dyn_cast_or_null<FunctionTemplateDecl>(N->ASTNode.get<Decl>());
  }

  if (FunctionTemplateDeclaration == nullptr) {
    return false;
  }

  auto *TemplateParameter = FunctionTemplateDeclaration->getTemplateParameters()->getParam(TemplateTypeParamType->getIndex());
  TemplateTypeParameterDeclaration = dyn_cast_or_null<TemplateTypeParmDecl>(TemplateParameter);
  if (!TemplateTypeParameterDeclaration->wasDeclaredWithTypename()) {
    return false;
  }

  RequiresExpr = FunctionTemplateDeclaration->getAsFunction()->getTrailingRequiresClause();

  return true;
}

Expected<Tweak::Effect> TransformConcept::apply(const Selection &Inputs) {
  auto &Ctx = Inputs.AST->getASTContext();
  auto &SrcMgr = Inputs.AST->getSourceManager();

  tooling::Replacements Replacements{};

  auto ConceptName = ConceptSpecializationExpression->getNamedConcept()->getQualifiedNameAsString();
  auto TypeSourceRange = TemplateTypeParameterDeclaration->getSourceRange();
  auto SourceRangeSize = SrcMgr.getFileOffset(TypeSourceRange.getEnd()) - SrcMgr.getFileOffset(TypeSourceRange.getBegin());
  auto TypeReplacement = tooling::Replacement(Ctx.getSourceManager(), TypeSourceRange.getBegin(), SourceRangeSize, ConceptName + ' ');

  if (auto Err = Replacements.add(TypeReplacement)) {
    return Err;
  }

  // Replace requirement clause with empty string
  // TODO: Only do this if there are no further require clauses
  auto RequiresRng = toHalfOpenFileRange(SrcMgr, Ctx.getLangOpts(), RequiresExpr->getSourceRange());
  if (!RequiresRng) {
    return error("Could not obtain range of the 'requires' branch. Macros?");
  }

  auto RequiresCode = toSourceCode(SrcMgr, *RequiresRng);

  auto RequirementReplacement = tooling::Replacement(Ctx.getSourceManager(), RequiresRng->getBegin(), RequiresCode.size(), std::string{});
  if (auto Err = Replacements.add(RequirementReplacement)) {
    return Err;
  }

  return Effect::mainFileEdit(SrcMgr, Replacements);
}

} // namespace
} // namespace clangd
} // namespace clang
