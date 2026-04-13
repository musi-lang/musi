---
title: "data とパターンマッチ"
description: "<code>data</code> で和型を定義し、バリアントを作り、<code>case</code> で読みます。"
group: "言語の基本"
section: "言語の基本"
order: 8
slug: "data-and-pattern-matching"
summary: "data 定義、コンストラクタ、パターンマッチ。"
---

`data` を使うと、範囲が限られた領域をコードの中で直接表せます。
`case` は形ごとに読み進め、分岐を明示したまま保ちます。

## まず match する

{{snippet:data-port-case}}

値が既知のいくつかの形のどれかになるなら `data` を使います。

## 定義して組み立てる

{{snippet:data-port}}

{{snippet:data-port-value}}

## 比較

{{example:data-named-record}}

## 試す

{{try:data-and-pattern-matching}}

## 次の一歩

3 つのコード例を順に読んでから [レコードと配列](/docs/records-arrays-and-mutation) へ進んでください。
