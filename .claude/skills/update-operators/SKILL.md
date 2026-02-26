---
name: update-operators
description: OPERATORS.md を更新する。ソースコードの関数・変数を解析し、Common Lisp HyperSpec スタイルのドキュメントを生成・追記・更新する。
argument-hint: "[ファイルパス or 関数名 (省略可)]"
allowed-tools: Read, Write, Edit, Glob, Grep
---

# OPERATORS.md 更新スキル

OPERATORS.md はこのプロジェクトのオペレータ (関数・変数) マニュアルである。
Common Lisp HyperSpec のスタイルに従い、すべてのオペレータをドキュメント化する。

## タスク

1. 引数が指定された場合、そのファイルまたは関数のみを対象とする
2. 引数が省略された場合、`src/` ディレクトリ内の全 `.el` ファイルを走査する
3. ソースコード内の `defun`, `defcustom`, `defvar`, `defconst`, `cl-defstruct` を解析する
4. OPERATORS.md に記載がない関数は追記し、既存の関数は内容を更新する
5. ソースコードから削除された関数は OPERATORS.md からも削除する

## OPERATORS.md の構造

```markdown
# OPERATORS.md

説明文...

---

## ファイルパス (例: src/github-api.el)

モジュールの説明文。

### 関数名 or 変数名

#### Syntax:

(関数の呼び出し形式を elisp コードブロックで記載)

#### Arguments and Values:

- **引数名** — 説明

#### Description:

関数の動作を説明する。

#### Examples:

elisp コードブロックで使用例を記載する。

#### Affected By:

動作に影響を与える変数を列挙する。

#### Exceptional Situations:

エラーが発生する条件を記載する。

#### See Also:

関連する関数・変数を列挙する。

#### Notes:

補足情報 (内部関数/公開関数の区別、副作用の有無など)。
```

## ルール

- 日本語で記述する
- `--` 付き関数は内部用、`--` なし関数は公開 API と明記する
- `defcustom` はテーブル形式で簡潔に記載する (HyperSpec セクション不要)
- `defvar` は簡潔に記載する (HyperSpec セクション不要)
- `defconst` は簡潔に記載する (HyperSpec セクション不要)
- `defun` と `cl-defstruct` は HyperSpec 形式のフルセクションで記載する
- Examples セクションでは実行可能な elisp コードを記載する
- See Also には同一モジュール内外を問わず関連するシンボルを記載する
- ファイル単位のセクション (`## src/xxx.el`) の順序はファイルのアルファベット順とする
- 各ファイル内の関数の順序はソースコード内の定義順に従う

## 手順

1. `$ARGUMENTS` が指定されている場合:
   - ファイルパスなら、そのファイルのみ読み込んで対象とする
   - 関数名なら、その関数を含むファイルを Grep で特定し、対象とする
2. `$ARGUMENTS` が省略されている場合:
   - `src/**/*.el` を Glob で走査し、全ファイルを対象とする
3. 対象ファイルのソースコードを Read で読み込む
4. 既存の OPERATORS.md を Read で読み込む
5. 差分を検出し、追加・更新・削除を行う
6. OPERATORS.md を Edit または Write で更新する
