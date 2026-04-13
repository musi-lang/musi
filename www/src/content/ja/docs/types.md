---
title: "型とジェネリック"
description: "値と同じ流れで型注釈とジェネリック引数を読みます。"
group: "型"
section: "型"
order: 10
slug: "types"
summary: "型注釈、ジェネリック引数、直接の型適用。"
---

Musi の型は値や関数の近くに現れます。
別の宣言節へ切り替えずに読めます。

{{snippet:types-basic}}

## ジェネリック

まず値や関数に注釈を足し、再利用が必要になったらジェネリックを使います。
最初は具体的な型から始めます。1 つの型で意味が通ってからジェネリックに進みます。

{{snippet:types-apply}}

## 比較

{{example:generic-constraint}}

## 試す

{{try:types}}

## 次の一歩

2 つのコード例を試してから [クラスとインスタンス](/docs/classes-instances-and-laws) へ進んでください。
