//===-- ConvertToAutoParameterTests.cpp ---------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TweakTesting.h"
#include "gtest/gtest.h"

namespace clang {
namespace clangd {
namespace {

TWEAK_TEST(ConvertFunctionTemplateToAbbreviatedForm);

TEST_F(ConvertFunctionTemplateToAbbreviatedFormTest, Test) {
  Header = R"cpp(
      template <typename T>
      concept foo = true;

      template <typename T>
      concept bar = true;

      template <typename T, typename U>
      concept baz = true;

      template <typename T>
      class list<T>;
  )cpp";

  ExtraArgs = {"-std=c++20"};

  EXPECT_EQ(apply("template <typename T> auto ^fun(T param) {}"),
                  " auto fun(auto param) {}");
//  EXPECT_EQ(apply("template <typename...ArgTypes> auto f^o^o(ArgTypes...params) -> void{}"),
//                  "auto foo(auto...params) -> void{}");

  EXPECT_AVAILABLE("tem^plate <type^name ^T> auto f^un(^T pa^ram) {}");
  EXPECT_AVAILABLE("tem^plate <f^oo ^T> auto fu^n(^T pa^ram) -> void {}");
//  EXPECT_AVAILABLE("tem^plate <fo^o T> auto fu^n(^T const ** pa^ram) -> void {}");
//  EXPECT_AVAILABLE("template <typename...ArgTypes> auto f^o^o(ArgTypes...params) -> void{}");

  // Not possible to have `auto` within collections
  EXPECT_UNAVAILABLE("template<typename T> auto f^u^n(list<T> param) -> void {}");
  EXPECT_UNAVAILABLE("template<typename T> auto f^u^n(list<T> param) -> void {}");

  EXPECT_UNAVAILABLE("tem^plate<type^name ^T, typename ^U> auto f^un(^U, ^T) -> void {}");
}

} // namespace
} // namespace clangd
} // namespace clang
