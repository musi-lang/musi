---
title: "式と束縛"
description: "<code>let</code>、連続した式、<code>case</code> で Musi を読みます。"
group: "言語の基本"
section: "言語の基本"
order: 5
slug: "expressions-and-bindings"
summary: "名前、連続した式、分岐を読むための基本モデル。"
---

式と束縛は Musi を読む中心です。名前を束縛し、そのまま下へ読み進めます。

{{snippet:let-binding}}

## 連続した式

{{snippet:sequence}}

`;` は式を区切ります。括弧で 1 つの大きな式としてまとめることもできます。

## 分岐

{{snippet:case-port}}

`case ... of` は分岐を扱います。形に応じて match し、各 branch から値を返します。

## 試す

{{try:expressions-and-bindings}}

## 次の一歩

2 つのコード例を読み、次に [演算子とリテラル](/docs/operators-and-literals) へ進んでください。
