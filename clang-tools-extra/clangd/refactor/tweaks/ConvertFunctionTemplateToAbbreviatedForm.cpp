//===--- ConvertFunctionTemplateToAbbreviatedForm.cpp --------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "AST.h"
#include "llvm/Support/Error.h"
#include "SourceCode.h"
#include "FindTarget.h"
#include <numeric>
#include "refactor/Tweak.h"
#include "llvm/ADT/StringRef.h"
#include "ParsedAST.h"
#include "support/Logger.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ExprConcepts.h"
#include "clang/Tooling/Core/Replacement.h"
#include "llvm/Support/Casting.h"
#include "XRefs.h"

namespace clang {
namespace clangd {
namespace {
/// Replaces type of declaration type with `auto`
/// Before:
///     template <std::integral T>
///     auto foo(T param) { }
///          ^^^^^^^^^^^
/// After:
///     auto foo(std::integral auto param) { }
class ConvertFunctionTemplateToAbbreviatedForm : public Tweak {
public:
  const char *id() const final;

  bool prepare(const Selection &Inputs) override;
  Expected<Effect> apply(const Selection &Inputs) override;
  std::string title() const override { return "Convert to auto parameter"; }
  llvm::StringLiteral kind() const override {
    return CodeAction::REFACTOR_KIND;
  }

private:
  const FunctionTemplateDecl *FunctionTemplateDeclaration;

  std::vector<const TypeConstraint*> TypeConstraints;
  std::vector<unsigned int> ParameterIndices;
  std::vector<std::vector<tok::TokenKind>> Qualifiers;

  auto generateFunctionParameterReplacement(unsigned int ParameterIndex,
                                            ASTContext &Context)
      -> llvm::Expected<tooling::Replacement>;

  auto generateTemplateDeclarationReplacement(ASTContext &Context)
      -> llvm::Expected<tooling::Replacement>;

  template <typename T, typename NodeKind>
  static auto findNode(const SelectionTree::Node &Root) -> const T *;

  template <typename T>
  static auto findDeclaration(const SelectionTree::Node &Root) -> const T * {
    return findNode<T, Decl>(Root);
  }
};

REGISTER_TWEAK(ConvertFunctionTemplateToAbbreviatedForm)

bool ConvertFunctionTemplateToAbbreviatedForm::prepare(const Selection &Inputs) {
  const auto *Root = Inputs.ASTSelection.commonAncestor();
  if (!Root)
    return false;

  FunctionTemplateDeclaration = findDeclaration<FunctionTemplateDecl>(*Root);
  if (!FunctionTemplateDeclaration)
    return false;

  auto *TemplateParameters = FunctionTemplateDeclaration->getTemplateParameters();
  if (TemplateParameters->size() == 0)
    // TODO: Investigate if we could handle empty templates easily.
    return false;

  auto Size = TemplateParameters->size();
  TypeConstraints.reserve(Size);
  ParameterIndices.reserve(Size);
  Qualifiers.reserve(Size);

  for (auto *TemplateParameter : *TemplateParameters) {
    // TODO: Make this comment more clear
    // For each template parameter we check how many times it is referenced.
    // Depending on the number of references we know a few more things.
    // If there is
    // - exactly one: The template parameter was declared but never used, which
    //                means we know for sure it doesn't appear as a parameter.
    // - exactly two: The template parameter was used exactly once, either as a
    //                parameter or somewhere else. This is the case we are
    //                interested in.
    // - more than two: The template parameter was either used for multiple
    //                  parameters or somewhere else in the function.

    auto *TemplateParameterDeclaration = dyn_cast_or_null<TemplateTypeParmDecl>(TemplateParameter);
    TypeConstraints.push_back(TemplateParameterDeclaration->getTypeConstraint());

    auto TemplateParameterPosition = sourceLocToPosition(Inputs.AST->getSourceManager(), TemplateParameter->getEndLoc());
    auto ReferencesResult = findReferences(*Inputs.AST, TemplateParameterPosition, 3, Inputs.Index);

    if (ReferencesResult.References.size() != 2)
      return false;
  }

  auto CurrentTemplateParameterBeingChecked = 0u;

  auto Parameters = FunctionTemplateDeclaration->getAsFunction()->parameters();
  for (auto ParameterIndex = 0u; ParameterIndex < Parameters.size(); ParameterIndex++) {
    auto Type = Parameters[ParameterIndex]->getType();
    std::vector<tok::TokenKind> QualifiersForParameter{};

    if (Type->isRValueReferenceType()) {
      QualifiersForParameter.push_back(tok::ampamp);
      Type = Type.getNonReferenceType();
    }

    if (Type->isLValueReferenceType()) {
      QualifiersForParameter.push_back(tok::amp);
      Type = Type.getNonReferenceType();
    }

    if (Type.isConstQualified()) {
      QualifiersForParameter.push_back(tok::kw_const);
    }

    while (Type->isPointerType()) {
      QualifiersForParameter.push_back(tok::star);
      Type = Type->getPointeeType();

      if (Type.isConstQualified()) {
        QualifiersForParameter.push_back(tok::kw_const);
      }
    }

    std::reverse(QualifiersForParameter.begin(), QualifiersForParameter.end());
    Qualifiers.push_back(QualifiersForParameter);

    if (!Type->isTemplateTypeParmType())
      continue;

    const auto *TemplateTypeParameterType = dyn_cast_or_null<TemplateTypeParmType>(Type);
    if (TemplateTypeParameterType->getIndex() == CurrentTemplateParameterBeingChecked) {
      CurrentTemplateParameterBeingChecked += 1;
      ParameterIndices.push_back(ParameterIndex);
    } else {
      return false;
    }
  }

  if (CurrentTemplateParameterBeingChecked != TemplateParameters->size())
    // TODO: Make this comment more clear
    // At least one template parameter was not used as a parameter.
    return false;

  return true;
}

#define AddReplacement(Replacement) \
if (auto Err = Replacement.takeError()) return Err; \
if (auto Err = Replacements.add(*Replacement)) return Err;

Expected<Tweak::Effect> ConvertFunctionTemplateToAbbreviatedForm::apply(const Selection &Inputs) {
  auto &Context = Inputs.AST->getASTContext();

  tooling::Replacements Replacements{};

  // Replace parameter types
  auto TemplateParameterCount = ParameterIndices.size();
  for (auto TemplateParameterIndex = 0u; TemplateParameterIndex < TemplateParameterCount; TemplateParameterIndex++) {
    auto FunctionParameterIndex = ParameterIndices[TemplateParameterIndex];
    auto FunctionParameterReplacement = generateFunctionParameterReplacement(FunctionParameterIndex, Context);

    AddReplacement(FunctionParameterReplacement);
  }

  // Remove template declaration
  auto TemplateDeclarationReplacement =
      generateTemplateDeclarationReplacement(Context);

  AddReplacement(TemplateDeclarationReplacement);

  return Effect::mainFileEdit(Context.getSourceManager(), Replacements);
}

template <typename T, typename NodeKind>
auto ConvertFunctionTemplateToAbbreviatedForm::findNode(const SelectionTree::Node &Root) -> const T * {
  for (const auto *Node = &Root; Node; Node = Node->Parent) {
    if (const T *Result = dyn_cast_or_null<T>(Node->ASTNode.get<NodeKind>()))
      return Result;
  }

  return nullptr;
}

auto ConvertFunctionTemplateToAbbreviatedForm::generateFunctionParameterReplacement(
    unsigned int ParameterIndex,
    ASTContext &Context) -> llvm::Expected<tooling::Replacement> {
  auto &SourceManager = Context.getSourceManager();

  auto FunctionParameterIndex = ParameterIndices[ParameterIndex];
  auto *TypeConstraint = TypeConstraints[ParameterIndex];

  auto *Parameter = FunctionTemplateDeclaration->getAsFunction()->getParamDecl(FunctionParameterIndex);
  auto ParameterName = Parameter->getDeclName().getAsString();

  // TODO: Maybe find a better name for this
  std::vector<std::string> ParameterTokens{};

  if (TypeConstraint != nullptr) {
    auto ConceptName = TypeConstraint->getNamedConcept()->getQualifiedNameAsString();
    ParameterTokens.push_back(ConceptName);
  }

  ParameterTokens.push_back(getKeywordSpelling(tok::kw_auto));

  // TODO: Make this more efficient :)
  for (const auto &Qualifier : Qualifiers[ParameterIndex]) {
    if (const auto *KeywordSpelling = getKeywordSpelling(Qualifier)) {
      ParameterTokens.push_back(KeywordSpelling);
    } else if (const auto *PunctuatorSpelling = getPunctuatorSpelling(Qualifier)) {
      ParameterTokens.push_back(PunctuatorSpelling);
    }

    // TODO: Handle unknown tokens
  }

  ParameterTokens.push_back(ParameterName);

  auto FunctionTypeReplacementText = std::accumulate(ParameterTokens.begin(), ParameterTokens.end(), std::string{}, [](auto Result, auto Token) {
    return Result + " " + Token;
  });

  auto FunctionParameterRange =
      toHalfOpenFileRange(SourceManager, Context.getLangOpts(), Parameter->getSourceRange());

  if (!FunctionParameterRange)
    return error("Could not obtain range of the template parameter. Macros?");

  // Replaces `typename T` with `auto`
  return tooling::Replacement(
      SourceManager,
      CharSourceRange::getCharRange(*FunctionParameterRange),
      FunctionTypeReplacementText);
}

auto ConvertFunctionTemplateToAbbreviatedForm::generateTemplateDeclarationReplacement(
    ASTContext &Context) -> llvm::Expected<tooling::Replacement> {
  auto &SourceManager = Context.getSourceManager();
  auto *TemplateParameters = FunctionTemplateDeclaration->getTemplateParameters();

  auto TemplateDeclarationRange =
      toHalfOpenFileRange(SourceManager, Context.getLangOpts(),
                          TemplateParameters->getSourceRange());

  if (!TemplateDeclarationRange)
    return error("Could not obtain range of the template parameter. Macros?");

  // TODO: delete empty line (\10 is backslash but doesn't work)
  // Comment from Jeremy: I'm not sure what is meant by '\10'
  return tooling::Replacement(
      SourceManager,
      CharSourceRange::getCharRange(*TemplateDeclarationRange),
      "");
}

} // namespace
} // namespace clangd
} // namespace clang
