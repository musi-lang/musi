---
title: "テストと実行"
description: "パッケージを実行し、テストを走らせ、必要なときは直接実行の CLI を使います。"
group: "ツール"
section: "ツール"
order: 16
slug: "testing-and-running"
summary: "パッケージ作業と単体ファイル作業に使う主要なコマンド。"
---

テストと実行は対象ごとに分かれています。
- パッケージ単位の作業は `musi`
- ソースや成果物を直接扱う作業は `music`

## テストの形

- テスト は `*.test.ms` に置く
- 各 テスト は export した `test` で公開する

{{snippet:stdlib-testing-import}}

## パッケージコマンド

{{snippet:package-commands}}

## 直接実行コマンド

{{snippet:music-direct}}

## 比較

{{example:testing-entry}}

通常のプロジェクト作業には `musi` を使います。1 つのソースファイルやビルド済みの成果物を直接実行・確認したいときは `music` を使います。

## 試す

{{try:testing-and-running}}

## 次の一歩

パッケージコマンドを 1 つ、直接実行コマンドを 1 つ実行し、分かりにくい挙動があれば該当する章へ戻ってください。

[コミュニティ](/community) にはプロジェクトへの入口と公開ゲストブックがあります。
