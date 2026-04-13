---
title: "レコードと配列"
description: "レコードリテラル、配列、明示的な spread 形式を使います。"
group: "言語の基本"
section: "言語の基本"
order: 9
slug: "records-arrays-and-mutation"
summary: "構造化された値と、現在の書き換え可能な data の扱い方。"
---

レコードと配列は、更新の仕方が予測しやすい普通の値です。

{{snippet:record-array}}

## copy して更新する

{{snippet:spread-record-array}}

## 比較

{{example:record-array-spread}}

## Musi の補足
Musi では `let r := p.{ x := 3 };` や `let r := p.{ ...q, y := 9 };` のような、入れ子になったレコード更新構文も使えます。
この形は上の比較例とは別で、F# や OCaml と同じ系統のレコード更新構文です。

## 試す

{{try:records-arrays-and-mutation}}

## 次の一歩

元の値を 1 つ作り、spread を使った別形を 1 つ作ってから [型とジェネリック](/docs/types) へ進んでください。
