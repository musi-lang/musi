---
title: "quote と構文値"
description: "再利用できるコードひな形と構文主導の作業に quote とスプライスを使います。"
group: "抽象化"
section: "抽象化"
order: 14
slug: "quote-and-syntax"
summary: "quote した式、スプライス形式、実用的なメタプログラミングの型。"
---

quote 構文を使うと、コードを data として扱い、その形の中に値や部分式をスプライスできます。

## quote を使わない場合

{{snippet:quote-without-meta}}

## quote とスプライスを使う場合

{{snippet:quote-with-meta}}

コードの形そのものを組み立てたり変換したりしたいときは `quote` を使います。

## 小さい形

{{snippet:quote-expr}}

{{snippet:quote-block}}

## 比較

{{example:quote-metaprogramming}}

`#name` や `#(expr)` のようなスプライス形式は quote の文脈の中でだけ有効です。

## 試す

{{try:quote-and-syntax}}

## 次の一歩

重複した補助関数の組を 1 つ選び、1 つの quote 用ひな形に置き換えてから [基盤ライブラリと標準ライブラリ](/docs/foundation-and-standard-library) へ進んでください。
