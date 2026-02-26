# textGL

テキストベースのボックス図エディタ for Emacs。

Unicode罫線を使用してボックスと接続線を描画し、マウスで直感的に操作できます。

## 特徴

- Unicode罫線（┌─┐│└─┘）またはASCII罫線を使用した描画
- マウスドラッグによるボックスの移動・リサイズ
- ボックス間の接続線（自動ルーティング）
- インスペクタによるプロパティ編集
- undo/redo サポート
- ファイル保存・読み込み（.ew形式）

## 必要環境

- Emacs 29.1 以上

## インストール

```elisp
(add-to-list 'load-path "/path/to/textgl")
(require 'textgl)
```

## 使い方

```
M-x textgl
```

### キーバインド

| キー | 操作 |
|------|------|
| クリック | ボックス選択 / インスペクタ表示 |
| ドラッグ | ビューポートパン |
| C-ドラッグ | ボックス移動 |
| C-右下ドラッグ | ボックスリサイズ |
| 1-6 | プロパティ編集 |
| ESC | 選択解除 |
| a | ボックス追加 |
| d | ボックス削除 |
| e | ラベル編集 |
| c | 接続線追加 |
| x | 接続線削除 |
| u | undo |
| r | redo |
| C-x C-s | 保存 |
| C-x C-f | 読み込み |
| s | スタイル切替 (Unicode/ASCII) |
| ? | ヘルプ |
| q | 終了 |

## ファイル構成

```
textgl/
├── textgl.el          # メインファイル（エントリポイント、パブリックAPI）
├── textgl-core.el     # コアモジュール（データモデル、スタイル、グリッド操作）
├── textgl-draw.el     # 描画モジュール（ボックス、接続線、インスペクタ描画）
├── textgl-ui.el       # UIモジュール（マウス操作、インスペクタ操作）
└── textgl-io.el       # IOモジュール（undo/redo、ファイル保存・読み込み）
```

### モジュール詳細

| ファイル | 行数 | 内容 |
|---------|------|------|
| textgl-core.el | ~200行 | データモデル（box, connection構造体）、描画スタイル変数、バッファローカル変数、全角幅対応、グリッド操作 |
| textgl-draw.el | ~600行 | ボックス描画、接続線描画（ルーティング、ウェイポイント、矢印）、インスペクタ描画、レンダリング |
| textgl-ui.el | ~300行 | ボックス判定、Z-order管理、位置クランプ、マウスドラッグ（移動・リサイズ・パン）、インスペクタ操作 |
| textgl-io.el | ~150行 | スナップショットベースundo/redo、シリアライズ/デシリアライズ、ファイル保存・読み込み |
| textgl.el | ~300行 | require文、パブリックAPI、インタラクティブコマンド、メジャーモード定義、エントリポイント |

### 依存関係

```
textgl-core.el  (基底モジュール - 依存なし)
      ↑
textgl-draw.el  (textgl-core に依存)
      ↑
textgl-ui.el    (textgl-core, textgl-draw に依存)

textgl-io.el    (textgl-core, textgl-draw に依存)

textgl.el       (全モジュールに依存 - トップレベル)
```

## API

### ボックス操作

```elisp
;; ボックス追加
(textgl-add-box 'my-box "ラベル" row col width height)

;; ボックス削除
(textgl-remove-box 'my-box)
```

### 接続線操作

```elisp
;; 接続線追加
(textgl-add-connection 'my-conn 'from-box 'to-box 'right 'left)

;; 接続線削除
(textgl-remove-connection 'my-conn)
```

### スタイル切替

```elisp
;; Unicodeスタイル
(textgl-set-style textgl-style-unicode)

;; ASCIIスタイル
(textgl-set-style textgl-style-ascii)
```

## ライセンス

MIT License
