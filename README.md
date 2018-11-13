# hsfiles

Convert `*.hsfiles` from/to actual directory structures using **tar**-like commands

## Usage

### Hsfiles > directory structures

```shell
$ hsfiles -x YOUR_TEMPLATE.hsfiles YOUR_TEMPLATE_DIR
```

For example, if you extract `new-template.hsfile`, type:

```shell
$ hsfiles -x new-template.hsfiles new-template
```

and you can get:

```shell
$ tree
.
├── new-template
│   ├── ChangeLog.md
│   ├── LICENSE
│   ├── README.md
│   ├── Setup.hs
│   ├── app
│   │   └── Main.hs
│   ├── package.yaml
│   ├── src
│   │   └── Lib.hs
│   └── test
│       └── Spec.hs
└── new-template.hsfiles

4 directories, 9 files
```

### Directory structures > hsfiles

```shell
$ hsfiles -c YOUR_TEMPLATE.hsfiles YOUR_TEMPLATE_DIR
```

## Install

1. clone this repository
2. `stack install`

## Motivation

When you start new GHC Haskell project with stack build tool, stack-template is useful to generate frequently used directory structures. You can write your own stack-template and save it as `*.hsfiles`.

#### Problem

A `*.hsfiles` consists of the concatenation of the directory structures when you have `stack new`. So, if you want to get more complex directory structures, your `*.hsfiles` will get large and hard to edit.

#### Solution

- Hsfiles > directory structures
  - `hsfiles` extract (x) your `*.hsfiles` into the directory structures as in `stack new`
    - however, `hsfiles` does not change `{{placeholder}}` because the way to fill it depends on your `~/.stack/config.yaml`
  - and you can edit the extracted template files as usual file editing

- Directory structures > hsfiles
  - `hsfiles` create (c) `*.hsfiles` from given directory structures
    - you can use this when you actually `stack new` or you can share it
  - also, `hsfiles` may be useful when you create new templates from your actual Haskell project
    - LIMITATION: `hsfiles` has not be able to generate `{{placeholder}}` from extracted strings yet