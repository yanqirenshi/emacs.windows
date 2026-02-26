;;; textgl-core.el --- textGL コアモジュール -*- lexical-binding: t; -*-

;;; Commentary:
;; textGL の基底モジュール。
;; データモデル、描画スタイル、バッファローカル変数、グリッド操作を提供。

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; ============================================================
;;;; データモデル
;;;; ============================================================

(cl-defstruct (textgl--box (:constructor textgl--box-create))
  "ボックスオブジェクト。"
  (id       nil :documentation "一意識別子(シンボル)")
  (row      0   :documentation "行位置(0始まり)")
  (col      0   :documentation "列位置(0始まり)")
  (width   16   :documentation "内側の幅")
  (height   3   :documentation "内側の高さ")
  (label   ""   :documentation "ラベル文字列")
  (z-order  0   :documentation "重なり順(大きいほど前面)"))

(cl-defstruct (textgl--connection (:constructor textgl--connection-create))
  "接続線オブジェクト。"
  (id       nil   :documentation "一意識別子(シンボル)")
  (from-id  nil   :documentation "接続元ボックスID")
  (to-id    nil   :documentation "接続先ボックスID")
  (from-side 'right :documentation "接続元の辺 (top/bottom/left/right)")
  (to-side   'left  :documentation "接続先の辺 (top/bottom/left/right)"))

;;;; ============================================================
;;;; 描画スタイル
;;;; ============================================================

(defvar textgl-style-unicode
  '(:tl ?┌ :tr ?┐ :bl ?└ :br ?┘ :h ?─ :v ?│)
  "Unicode罫線スタイル。")

(defvar textgl-style-ascii
  '(:tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?- :v ?|)
  "ASCII罫線スタイル。")

;; 接続線用の文字
(defvar textgl-line-chars-unicode
  '(:h ?─ :v ?│ :corner-tl ?┌ :corner-tr ?┐ :corner-bl ?└ :corner-br ?┘
    :arrow-right ?→ :arrow-left ?← :arrow-down ?↓ :arrow-up ?↑)
  "Unicode接続線文字。
矢印文字は全角幅(char-width=2)の文字を使用し、罫線文字と幅を統一する。")

(defvar textgl-line-chars-ascii
  '(:h ?- :v ?| :corner-tl ?+ :corner-tr ?+ :corner-bl ?+ :corner-br ?+
    :arrow-right ?> :arrow-left ?< :arrow-down ?v :arrow-up ?^)
  "ASCII接続線文字。")

;;;; ============================================================
;;;; バッファローカル変数
;;;; ============================================================

(defvar-local textgl--canvas-rows 30
  "キャンバスの行数。")

(defvar-local textgl--canvas-cols 80
  "キャンバスの列数。")

(defvar-local textgl--grid nil
  "描画用グリッド(ベクターのベクター、各行は文字のベクター)。")

(defvar-local textgl--boxes nil
  "ボックスのリスト。")

(defvar-local textgl--style nil
  "現在の描画スタイル。")

(defvar-local textgl--connections nil
  "接続線のリスト。")

(defvar-local textgl--next-z 0
  "次に割り当てるz-order値。")

(defvar-local textgl--undo-stack nil
  "undoスタック。状態のスナップショットのリスト。")

(defvar-local textgl--redo-stack nil
  "redoスタック。状態のスナップショットのリスト。")

(defvar-local textgl--file-name nil
  "現在開いているファイル名。")

(defvar-local textgl--selected-box nil
  "選択中のボックスID。nilなら非選択。")

(defvar-local textgl--inspector-row nil
  "インスペクタを表示する行位置。")

(defvar-local textgl--inspector-col nil
  "インスペクタを表示する列位置。")

;;;; ============================================================
;;;; 全角幅対応
;;;; ============================================================

(defconst textgl--padding 'padding
  "全角文字の2列目を示すパディングマーカー。
グリッド上で全角文字(char-width=2)が占める2番目のセルに配置する。")

(defcustom textgl-border-char-width 'auto
  "罫線文字の表示幅。
`auto' の場合はフォントのピクセル幅から自動判定する。
数値(1または2)を指定すると自動判定をスキップしてその値を使う。
自動判定が正しくない環境ではこの値を明示的に設定する。"
  :type '(choice (const :tag "自動判定" auto)
                 (const :tag "半角(1)" 1)
                 (const :tag "全角(2)" 2))
  :group 'textgl)

(defvar-local textgl--actual-border-width nil
  "罫線文字の実際の表示幅キャッシュ。
nilの場合は `textgl--measure-border-width' で自動判定する。")

(defun textgl--measure-border-width ()
  "罫線文字の実際の表示幅を測定して返す。
優先順位:
  1. `textgl-border-char-width' が数値→その値を使用
  2. GUIフレーム→ `string-pixel-width' で半角スペースと罫線のピクセル幅を比較
  3. ターミナル/batch→ `char-width' にフォールバック"
  (cond
   ;; ユーザー明示指定
   ((numberp textgl-border-char-width)
    textgl-border-char-width)
   ;; GUIフレーム: string-pixel-width で実測
   ((display-graphic-p)
    (condition-case nil
        (let ((space-px (string-pixel-width " "))
              (line-px  (string-pixel-width "─")))
          (if (and space-px line-px (> space-px 0))
              (if (> line-px (* space-px 1.5)) 2 1)
            (char-width ?─)))
      (error (char-width ?─))))
   ;; ターミナル/batch: char-widthを使用
   (t (char-width ?─))))

(defun textgl--border-h-width ()
  "現在スタイルの水平罫線文字の表示幅を返す。
Unicodeスタイルではフォント実測値を使用する。"
  (let ((st (or textgl--style textgl-style-unicode)))
    (if (eq (plist-get st :h) ?-)
        1  ;; ASCIIは常に1
      (or textgl--actual-border-width
          (setq textgl--actual-border-width (textgl--measure-border-width))))))

(defun textgl--border-v-width ()
  "現在スタイルの垂直罫線文字の表示幅を返す。"
  (textgl--border-h-width))  ;; 罫線文字は全て同じ幅

(defun textgl--border-corner-width ()
  "現在スタイルの角文字の表示幅を返す。"
  (textgl--border-h-width))  ;; 罫線文字は全て同じ幅

(defun textgl--box-inner-physical-width (box)
  "BOXの内部空間の物理列幅を返す。
上辺/下辺の水平罫線部分と同じ物理幅にする。"
  (* (textgl--box-width box) (textgl--border-h-width)))

(defun textgl--box-total-physical-width (box)
  "BOXの外枠を含む総物理列幅を返す。"
  (+ (textgl--border-corner-width)
     (textgl--box-inner-physical-width box)
     (textgl--border-corner-width)))

;;;; ============================================================
;;;; グリッド操作
;;;; ============================================================

(defun textgl--init-grid ()
  "グリッドを空白で初期化する。
各行は文字のベクターとして管理し、Unicode文字を安全に格納する。"
  (setq textgl--grid (make-vector textgl--canvas-rows nil))
  (dotimes (i textgl--canvas-rows)
    (aset textgl--grid i (make-vector textgl--canvas-cols ?\s))))

(defun textgl--grid-set (row col char)
  "グリッドの物理列(ROW, COL)にCHARを書き込む。
罫線の実表示幅が2の場合のみ、全角文字の次セルにパディングを入れる。
フォントのグリフ幅が半角の場合はパディングを入れない。
範囲外は無視。"
  (when (and (>= row 0) (< row textgl--canvas-rows)
             (>= col 0) (< col textgl--canvas-cols))
    (aset (aref textgl--grid row) col char)
    ;; 実表示幅が2のときのみパディングを入れる
    (when (and (characterp char)
               (> (char-width char) 1)
               (> (textgl--border-h-width) 1))
      (let ((next-col (1+ col)))
        (when (< next-col textgl--canvas-cols)
          (aset (aref textgl--grid row) next-col textgl--padding))))))

;;;; ============================================================
;;;; ユーティリティ関数
;;;; ============================================================

(defun textgl--find-box (id)
  "IDでボックスを検索して返す。"
  (cl-find-if (lambda (b) (eq (textgl--box-id b) id)) textgl--boxes))

(provide 'textgl-core)
;;; textgl-core.el ends here
