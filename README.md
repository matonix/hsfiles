# hsfiles

Convert `*.hsfiles` from/to actual directory structure using **tar**-like commands

## Usage

### Hsfiles > directory structure

```shell
$ hsfiles -x YOUR_TEMPLATE.hsfiles YOUR_TEMPLATE_DIR
```

and you get:

```shell
$ tree

```

### Directory structure > hsfiles

```shell
$ hsfiles -c YOUR_TEMPLATE.hsfiles YOUR_TEMPLATE_DIR
```

## Install

1. clone this repository
2. `stack install`

## Motivation

GHC Haskell のビルドツール stack を使って新しいプロジェクトを作るとき、 stack-template はよく使われるディレクトリ構造の構築を支援する。stack-template はあなた自身で作ることもでき、その内容は `*.hsfiles` というファイル形式で管理する。

#### Problem

`*.hsfiles` の中身は、`stack new` したときに作られるディレクトリ構造の中身が連結されている。そのため、構築したいディレクトリ構造が複雑になると、`*.hsfiles` が肥大化し、管理が困難になる。

#### Solution

- Hsfiles > directory structure
  - `stack new` したときのようにディレクトリに展開(x)される。
    - ただしプレースホルダー部分（`{{HERE}}`）は変化しないようにしている。
      - プレースホルダーの埋め方はあなたの `~/.stack/config.yaml` に依存するから。
  - 通常のファイル操作と同様の方法でテンプレートを編集できる。
- Directory structure > hsfiles
  - ディレクトリ構造から `*.hsfiles` を作成（c）する。
    - これによって、実際に `stack new` するときに使ったり、再配布したりできる。
  - また、あなたが実際に作成しているプロジェクトからテンプレートを抽出する作業にも使えるかもしれない。
    - Limitation: まだ、展開された文字列からプレースホルダーを構築する方法はわからない。