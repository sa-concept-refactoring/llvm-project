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

TWEAK_TEST(ConvertToAutoParameter);

TEST_F(ConvertToAutoParameterTest, Test) {
  Context = Function;

  Header = R"cpp(
  )cpp";

  EXPECT_EQ(apply("template <typename T> auto f^o^o(T param) {}"),
                  "auto foo(auto param) {}");
  EXPECT_EQ(apply("template <typename...ArgTypes> auto f^o^o(ArgTypes...params) -> void{}"),
                  "auto foo(auto...params) -> void{}");

  EXPECT_AVAILABLE("template <typename T> auto f^o^o(T param) {}");
  EXPECT_AVAILABLE("template<std::integral T> auto f^o^o(T Tpl) -> void {}");
  EXPECT_AVAILABLE("template<std::integral T> auto f^o^o(T const ** Tpl) -> void {}");
  EXPECT_AVAILABLE("template <typename...ArgTypes> auto foo(ArgTypes...params) -> void{}");

  // Not possible to have `auto` within collections
  EXPECT_UNAVAILABLE("template<typename T> auto f^o^o(vector<T> param) -> void {}");
  EXPECT_UNAVAILABLE("template<typename T> auto f^o^o(list<T> param) -> void {}");
  EXPECT_UNAVAILABLE("template<class T, size_t N> auto foo(T (&a)[N], int size) -> void {}");
}

} // namespace
} // namespace clangd
} // namespace clang
