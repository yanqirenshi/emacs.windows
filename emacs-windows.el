;;; emacs-windows.el --- テキストベースボックス図エディタ -*- lexical-binding: t; -*-

;; Author: yanqi
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: diagrams, tools

;;; Commentary:

;; テキストベースのボックス図エディタ。
;; 複数のボックスをマウスドラッグで移動できる。
;; Unicode罫線(┌─┐│└─┘)をデフォルトで使用。
;;
;; 使い方:
;;   M-x emacs-windows

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; ============================================================
;;;; データモデル
;;;; ============================================================

(cl-defstruct (ew--box (:constructor ew--box-create))
  "ボックスオブジェクト。"
  (id       nil :documentation "一意識別子(シンボル)")
  (row      0   :documentation "行位置(0始まり)")
  (col      0   :documentation "列位置(0始まり)")
  (width   16   :documentation "内側の幅")
  (height   3   :documentation "内側の高さ")
  (label   ""   :documentation "ラベル文字列")
  (z-order  0   :documentation "重なり順(大きいほど前面)"))

(cl-defstruct (ew--connection (:constructor ew--connection-create))
  "接続線オブジェクト。"
  (id       nil   :documentation "一意識別子(シンボル)")
  (from-id  nil   :documentation "接続元ボックスID")
  (to-id    nil   :documentation "接続先ボックスID")
  (from-side 'right :documentation "接続元の辺 (top/bottom/left/right)")
  (to-side   'left  :documentation "接続先の辺 (top/bottom/left/right)"))

;;;; ============================================================
;;;; 描画スタイル
;;;; ============================================================

(defvar ew-style-unicode
  '(:tl ?┌ :tr ?┐ :bl ?└ :br ?┘ :h ?─ :v ?│)
  "Unicode罫線スタイル。")

(defvar ew-style-ascii
  '(:tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?- :v ?|)
  "ASCII罫線スタイル。")

;; 接続線用の文字
(defvar ew-line-chars-unicode
  '(:h ?─ :v ?│ :corner-tl ?┌ :corner-tr ?┐ :corner-bl ?└ :corner-br ?┘
    :arrow-right ?→ :arrow-left ?← :arrow-down ?↓ :arrow-up ?↑)
  "Unicode接続線文字。
矢印文字は全角幅(char-width=2)の文字を使用し、罫線文字と幅を統一する。")

(defvar ew-line-chars-ascii
  '(:h ?- :v ?| :corner-tl ?+ :corner-tr ?+ :corner-bl ?+ :corner-br ?+
    :arrow-right ?> :arrow-left ?< :arrow-down ?v :arrow-up ?^)
  "ASCII接続線文字。")

;;;; ============================================================
;;;; バッファローカル変数
;;;; ============================================================

(defvar-local ew--canvas-rows 30
  "キャンバスの行数。")

(defvar-local ew--canvas-cols 80
  "キャンバスの列数。")

(defvar-local ew--grid nil
  "描画用グリッド(ベクターのベクター、各行は文字のベクター)。")

(defvar-local ew--boxes nil
  "ボックスのリスト。")

(defvar-local ew--style nil
  "現在の描画スタイル。")

(defvar-local ew--connections nil
  "接続線のリスト。")

(defvar-local ew--next-z 0
  "次に割り当てるz-order値。")

(defvar-local ew--undo-stack nil
  "undoスタック。状態のスナップショットのリスト。")

(defvar-local ew--redo-stack nil
  "redoスタック。状態のスナップショットのリスト。")

(defvar-local ew--file-name nil
  "現在開いているファイル名。")

;;;; ============================================================
;;;; 全角幅対応
;;;; ============================================================

(defconst ew--padding 'padding
  "全角文字の2列目を示すパディングマーカー。
グリッド上で全角文字(char-width=2)が占める2番目のセルに配置する。")

(defcustom ew-border-char-width 'auto
  "罫線文字の表示幅。
`auto' の場合はフォントのピクセル幅から自動判定する。
数値(1または2)を指定すると自動判定をスキップしてその値を使う。
自動判定が正しくない環境ではこの値を明示的に設定する。"
  :type '(choice (const :tag "自動判定" auto)
                 (const :tag "半角(1)" 1)
                 (const :tag "全角(2)" 2))
  :group 'emacs-windows)

(defvar-local ew--actual-border-width nil
  "罫線文字の実際の表示幅キャッシュ。
nilの場合は `ew--measure-border-width' で自動判定する。")

(defun ew--measure-border-width ()
  "罫線文字の実際の表示幅を測定して返す。
優先順位:
  1. `ew-border-char-width' が数値→その値を使用
  2. GUIフレーム→ `string-pixel-width' で半角スペースと罫線のピクセル幅を比較
  3. ターミナル/batch→ `char-width' にフォールバック"
  (cond
   ;; ユーザー明示指定
   ((numberp ew-border-char-width)
    ew-border-char-width)
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

(defun ew--border-h-width ()
  "現在スタイルの水平罫線文字の表示幅を返す。
Unicodeスタイルではフォント実測値を使用する。"
  (let ((st (or ew--style ew-style-unicode)))
    (if (eq (plist-get st :h) ?-)
        1  ;; ASCIIは常に1
      (or ew--actual-border-width
          (setq ew--actual-border-width (ew--measure-border-width))))))

(defun ew--border-v-width ()
  "現在スタイルの垂直罫線文字の表示幅を返す。"
  (ew--border-h-width))  ;; 罫線文字は全て同じ幅

(defun ew--border-corner-width ()
  "現在スタイルの角文字の表示幅を返す。"
  (ew--border-h-width))  ;; 罫線文字は全て同じ幅

(defun ew--box-inner-physical-width (box)
  "BOXの内部空間の物理列幅を返す。
上辺/下辺の水平罫線部分と同じ物理幅にする。"
  (* (ew--box-width box) (ew--border-h-width)))

(defun ew--box-total-physical-width (box)
  "BOXの外枠を含む総物理列幅を返す。"
  (+ (ew--border-corner-width)
     (ew--box-inner-physical-width box)
     (ew--border-corner-width)))

;;;; ============================================================
;;;; グリッド操作
;;;; ============================================================

(defun ew--init-grid ()
  "グリッドを空白で初期化する。
各行は文字のベクターとして管理し、Unicode文字を安全に格納する。"
  (setq ew--grid (make-vector ew--canvas-rows nil))
  (dotimes (i ew--canvas-rows)
    (aset ew--grid i (make-vector ew--canvas-cols ?\s))))

(defun ew--grid-set (row col char)
  "グリッドの物理列(ROW, COL)にCHARを書き込む。
罫線の実表示幅が2の場合のみ、全角文字の次セルにパディングを入れる。
フォントのグリフ幅が半角の場合はパディングを入れない。
範囲外は無視。"
  (when (and (>= row 0) (< row ew--canvas-rows)
             (>= col 0) (< col ew--canvas-cols))
    (aset (aref ew--grid row) col char)
    ;; 実表示幅が2のときのみパディングを入れる
    (when (and (characterp char)
               (> (char-width char) 1)
               (> (ew--border-h-width) 1))
      (let ((next-col (1+ col)))
        (when (< next-col ew--canvas-cols)
          (aset (aref ew--grid row) next-col ew--padding))))))

;;;; ============================================================
;;;; ボックス描画
;;;; ============================================================

(defun ew--draw-box (box)
  "BOXをグリッドに物理列座標で描画する。
罫線文字の実表示幅(ew--border-*-width)を考慮して座標を計算する。"
  (let* ((r  (ew--box-row box))
         (c  (ew--box-col box))        ;; 物理列
         (iw (ew--box-width box))      ;; 論理内部幅
         (ih (ew--box-height box))
         (st (or ew--style ew-style-unicode))
         (tl (plist-get st :tl))
         (tr (plist-get st :tr))
         (bl (plist-get st :bl))
         (br (plist-get st :br))
         (h  (plist-get st :h))
         (v  (plist-get st :v))
         (cw (ew--border-corner-width)) ;; 角文字の実表示幅
         (hw (ew--border-h-width))     ;; 水平罫線の実表示幅
         (vw (ew--border-v-width))     ;; 垂直罫線の実表示幅
         (inner-phys (* iw hw)))       ;; 内部の物理列幅
    ;; 上ボーダー: 角 + 水平罫線×iw + 角
    (let ((pc c))
      (ew--grid-set r pc tl)
      (setq pc (+ pc cw))
      (dotimes (_i iw)
        (ew--grid-set r pc h)
        (setq pc (+ pc hw)))
      (ew--grid-set r pc tr))
    ;; 左右の縦線 + 内部を空白で埋める
    (dotimes (i ih)
      (let ((row (+ r 1 i))
            (pc c))
        (ew--grid-set row pc v)
        (setq pc (+ pc vw))
        ;; 内部スペース: inner-phys個（上辺と同じ物理幅）
        (dotimes (_j inner-phys)
          (ew--grid-set row pc ?\s)
          (setq pc (1+ pc)))
        (ew--grid-set row pc v)))
    ;; 下ボーダー: 角 + 水平罫線×iw + 角
    (let ((br-row (+ r ih 1))
          (pc c))
      (ew--grid-set br-row pc bl)
      (setq pc (+ pc cw))
      (dotimes (_i iw)
        (ew--grid-set br-row pc h)
        (setq pc (+ pc hw)))
      (ew--grid-set br-row pc br))
    ;; ラベルを内部中央に配置
    (let* ((label (ew--box-label box))
           (label-w (string-width label))  ;; ラベルの表示幅
           (mid-row (+ r 1 (/ ih 2)))
           (inner-start (+ c vw))
           (lbl-start (+ inner-start (/ (- inner-phys label-w) 2))))
      (dotimes (i (length label))
        (let ((ch (aref label i)))
          (ew--grid-set mid-row lbl-start ch)
          (setq lbl-start (+ lbl-start (char-width ch))))))))

(defun ew--draw-all ()
  "全ボックスをz-order昇順でグリッドに描画する。"
  (let ((sorted (sort (copy-sequence ew--boxes)
                      (lambda (a b)
                        (< (ew--box-z-order a) (ew--box-z-order b))))))
    (dolist (box sorted)
      (ew--draw-box box))))

;;;; ============================================================
;;;; 接続線描画
;;;; ============================================================

(defun ew--find-box (id)
  "IDでボックスを検索して返す。"
  (cl-find-if (lambda (b) (eq (ew--box-id b) id)) ew--boxes))

(defun ew--box-anchor (box side)
  "BOXのSIDE辺の中点座標を(row . col)で物理列座標で返す。
水平座標はhwの倍数にアラインして全角罫線との整合性を保つ。"
  (let* ((r (ew--box-row box))
         (c (ew--box-col box))
         (h (ew--box-height box))
         (hw (ew--border-h-width))
         (total-w (ew--box-total-physical-width box))
         (total-h (+ h 2))
         (mid-col (* (/ (+ c (/ total-w 2)) hw) hw))    ;; hwの倍数にアライン
         (right-col (* (/ (+ c total-w -1) hw) hw)))    ;; 右辺もアライン
    (cl-case side
      (top    (cons r                    mid-col))
      (bottom (cons (+ r total-h -1)     mid-col))
      (left   (cons (+ r (/ total-h 2)) c))
      (right  (cons (+ r (/ total-h 2)) right-col)))))

(defun ew--line-chars ()
  "現在のスタイルに対応する接続線文字を返す。"
  (if (eq (plist-get ew--style :tl) ?┌)
      ew-line-chars-unicode
    ew-line-chars-ascii))

(defun ew--draw-connection (conn)
  "接続線CONNをグリッドに描画する。
接続元辺の中点から接続先辺の中点へ、3セグメントの折れ線で描画。
描画順序: 接続線→ボックスの順なので、ボックスと重なる部分はボックスが上書きする。"
  (let* ((from-box (ew--find-box (ew--connection-from-id conn)))
         (to-box   (ew--find-box (ew--connection-to-id conn))))
    (when (and from-box to-box)
      (let* ((from-side (ew--connection-from-side conn))
             (to-side   (ew--connection-to-side conn))
             (from-pt (ew--box-anchor from-box from-side))
             (to-pt   (ew--box-anchor to-box   to-side))
             (r1 (car from-pt)) (c1 (cdr from-pt))
             (r2 (car to-pt))   (c2 (cdr to-pt))
             (lc (ew--line-chars))
             (ch-h (plist-get lc :h))
             (ch-v (plist-get lc :v)))
        ;; ボックスの辺から外側にオフセット（罫線の実表示幅分）
        ;; 水平座標はhwの倍数にアラインして全角罫線の隙間を防ぐ
        (let ((hw (ew--border-h-width)))
          (cl-case from-side
            (right  (cl-incf c1 hw))
            (left   (cl-decf c1 hw))
            (top    (cl-decf r1))
            (bottom (cl-incf r1)))
          (cl-case to-side
            (right  (cl-incf c2 hw))
            (left   (cl-decf c2 hw))
            (top    (cl-decf r2))
            (bottom (cl-incf r2)))
          ;; 水平座標をhwの倍数にアライン
          (when (> hw 1)
            (setq c1 (* (/ c1 hw) hw))
            (setq c2 (* (/ c2 hw) hw)))
          ;; ルーティング戦略:
          ;; 接続元がleft/right → 水平→垂直→水平 (中間col)
          ;; 接続元がtop/bottom → 垂直→水平→垂直 (中間row)
          (cond
           ;; 完全に水平
           ((= r1 r2)
            (ew--draw-h-line r1 c1 c2 ch-h))
           ;; 完全に垂直
           ((= c1 c2)
            (ew--draw-v-line c1 r1 r2 ch-v))
           ;; 水平出発 (left/right): from側の列で即座に垂直→水平
           ;; from側の列c1で縦に曲がり、to側の行r2で水平にc2まで進む
           ((memq from-side '(left right))
            (ew--draw-v-line c1 r1 r2 ch-v)
            (ew--draw-h-line r2 c1 c2 ch-h)
            ;; 角1: (r1, c1) — 垂直線のfrom端（直角に曲がる点）
            (ew--grid-set r1 c1
              (ew--corner-char lc
                (if (> c1 (cdr from-pt)) 'from-left 'from-right)
                (if (> r2 r1) 'to-down   'to-up)))
            ;; 角2: (r2, c1) — 垂直線のto端（水平に折れる点）
            (ew--grid-set r2 c1
              (ew--corner-char lc
                (if (> r2 r1) 'from-up   'from-down)
                (if (> c2 c1) 'to-right  'to-left))))
           ;; 垂直出発 (top/bottom): from側で即座に水平→垂直
           ;; from側の行で水平に曲がり、to側に縦線を降ろす
           (t
            (ew--draw-h-line r1 c1 c2 ch-h)
            (ew--draw-v-line c2 r1 r2 ch-v)
            ;; 角1: (r1, c1) — 水平線のfrom端（直角に曲がる点）
            (ew--grid-set r1 c1
              (ew--corner-char lc
                (if (> r1 (car from-pt)) 'from-up 'from-down)
                (if (> c2 c1) 'to-right  'to-left)))
            ;; 角2: (r1, c2) — 水平線のto端（垂直に折れる点）
            (ew--grid-set r1 c2
              (ew--corner-char lc
                (if (> c2 c1) 'from-left 'from-right)
                (if (> r2 r1) 'to-down   'to-up))))))
        ))))

(defun ew--draw-connection-arrow (conn)
  "接続線CONNの矢印を描画する（ボックス描画後に呼ぶ）。
矢印はボックスの枠線の手前（1セル外側）に配置する。"
  (let* ((to-box (ew--find-box (ew--connection-to-id conn)))
         (to-side (ew--connection-to-side conn)))
    (when to-box
      (let* ((to-pt (ew--box-anchor to-box to-side))
             (lc (ew--line-chars))
             (hw (ew--border-h-width))
             ;; ボックスの辺から1セル外側にオフセット
             (ar (car to-pt))
             (ac (cdr to-pt)))
        (cl-case to-side
          (left   (setq ac (- ac hw)))   ;; 左辺の手前（さらに左）
          (right  (setq ac (+ ac hw)))   ;; 右辺の手前（さらに右）
          (top    (setq ar (- ar 1)))    ;; 上辺の手前（さらに上）
          (bottom (setq ar (+ ar 1))))   ;; 下辺の手前（さらに下）
        ;; 水平座標をhwの倍数にアライン（全角罫線の隙間防止）
        (when (> hw 1)
          (setq ac (* (/ ac hw) hw)))
        (ew--grid-set ar ac
          (plist-get lc (cl-case to-side
                          (left  :arrow-right)
                          (right :arrow-left)
                          (top   :arrow-down)
                          (bottom :arrow-up))))))))

(defun ew--corner-char (lc from-dir to-dir)
  "FROM-DIRからTO-DIRへの角の文字をLCから返す。
FROM-DIR: from-left, from-right, from-up, from-down
  （角から見て、どの方向から線が来るか）
TO-DIR: to-right, to-left, to-down, to-up
  （角から見て、どの方向へ線が出るか）

罫線文字の接続方向:
  ┌ (corner-tl): 右と下に接続
  ┐ (corner-tr): 左と下に接続
  └ (corner-bl): 右と上に接続
  ┘ (corner-br): 左と上に接続"
  (let ((dirs (sort (list (cl-case from-dir
                            (from-left  'left)
                            (from-right 'right)
                            (from-up    'up)
                            (from-down  'down))
                          (cl-case to-dir
                            (to-left  'left)
                            (to-right 'right)
                            (to-up    'up)
                            (to-down  'down)))
                    #'string<)))
    (plist-get lc
      (cond
       ;; ┌: 右+下
       ((equal dirs '(down right)) :corner-tl)
       ;; ┐: 左+下
       ((equal dirs '(down left))  :corner-tr)
       ;; └: 右+上
       ((equal dirs '(right up))   :corner-bl)
       ;; ┘: 左+上
       ((equal dirs '(left up))    :corner-br)
       (t :h)))))

(defun ew--draw-h-line (row c1 c2 char)
  "水平線をROW行にC1からC2まで物理列で描画する。
開始位置をhwの倍数にアラインし、全角罫線の隙間を防ぐ。"
  (let* ((start (min c1 c2))
         (end   (max c1 c2))
         (w     (ew--border-h-width))
         ;; 開始位置をwの倍数に切り下げてアライン（隙間防止）
         (aligned-start (* (/ start w) w)))
    (cl-loop for c from aligned-start to end by w
             do (ew--grid-set row c char))))

(defun ew--draw-v-line (col r1 r2 char)
  "垂直線をCOL列にR1からR2まで描画する。行方向は幅に無関係。"
  (let ((start (min r1 r2))
        (end   (max r1 r2)))
    (cl-loop for r from start to end
             do (ew--grid-set r col char))))

(defun ew--draw-all-connections ()
  "全接続線の線をグリッドに描画する。"
  (dolist (conn ew--connections)
    (ew--draw-connection conn)))

(defun ew--draw-all-connection-arrows ()
  "全接続線の矢印をグリッドに描画する（ボックス描画後に呼ぶ）。"
  (dolist (conn ew--connections)
    (ew--draw-connection-arrow conn)))

;;;; ============================================================
;;;; レンダリング
;;;; ============================================================

(defun ew--render ()
  "グリッド内容をバッファに反映する。
パディングマーカーはスキップし、全角文字の表示幅を正しく処理する。"
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (dotimes (i ew--canvas-rows)
      (let ((row-vec (aref ew--grid i)))
        (dotimes (j ew--canvas-cols)
          (let ((cell (aref row-vec j)))
            (unless (eq cell ew--padding)
              (insert (if (characterp cell) cell ?\s))))))
      (insert "\n"))
    (goto-char (min pos (point-max)))))

(defun ew--redraw ()
  "グリッド初期化→接続線→ボックス→矢印→バッファ反映の一括実行。"
  (ew--init-grid)
  (ew--draw-all-connections)       ;; 1. 接続線（線のみ）
  (ew--draw-all)                   ;; 2. ボックス（接続線の上に描画）
  (ew--draw-all-connection-arrows) ;; 3. 矢印（ボックスの辺の上に描画）
  (ew--render))

;;;; ============================================================
;;;; ボックス判定
;;;; ============================================================

(defun ew--box-hit-p (box row col)
  "BOXの物理範囲内に(ROW, COL)があればtを返す。"
  (let ((total-w (ew--box-total-physical-width box)))
    (and (>= row (ew--box-row box))
         (<= row (+ (ew--box-row box) (ew--box-height box) 1))
         (>= col (ew--box-col box))
         (< col (+ (ew--box-col box) total-w)))))

(defun ew--box-at (row col)
  "指定座標にあるボックスのうちz-order最前面のものを返す。"
  (let ((hit nil))
    (dolist (box ew--boxes)
      (when (ew--box-hit-p box row col)
        (if (null hit)
            (setq hit box)
          (when (> (ew--box-z-order box) (ew--box-z-order hit))
            (setq hit box)))))
    hit))

;;;; ============================================================
;;;; Z-order管理
;;;; ============================================================

(defun ew--bring-to-front (box)
  "BOXを最前面に移動する。"
  (setf (ew--box-z-order box) ew--next-z)
  (cl-incf ew--next-z))

;;;; ============================================================
;;;; 位置クランプ
;;;; ============================================================

(defun ew--clamp-box-position (box)
  "BOXの位置をキャンバス範囲内にクランプする。物理幅を考慮。
列位置をhwの倍数にスナップして全角罫線のアライメントを保つ。"
  (let* ((hw (ew--border-h-width))
         (max-row (- ew--canvas-rows (ew--box-height box) 2))
         (max-col (- ew--canvas-cols (ew--box-total-physical-width box))))
    (setf (ew--box-row box) (max 0 (min max-row (ew--box-row box))))
    (setf (ew--box-col box) (max 0 (min max-col (ew--box-col box))))
    ;; 列位置をhwの倍数にスナップ（全角罫線のアライメント）
    (when (> hw 1)
      (setf (ew--box-col box) (* (/ (ew--box-col box) hw) hw)))))

;;;; ============================================================
;;;; マウスドラッグ（移動・リサイズ統合）
;;;; ============================================================

(defun ew--resize-region-p (box row col)
  "BOXの右下角付近に(ROW, COL)があればtを返す。物理幅を考慮。"
  (let* ((total-w (ew--box-total-physical-width box))
         (br-row (+ (ew--box-row box) (ew--box-height box) 1))
         (br-col (+ (ew--box-col box) total-w -1)))
    (and (>= row (- br-row 1)) (<= row br-row)
         (>= col (- br-col 2)) (<= col br-col))))

(defun ew--mouse-drag (event)
  "マウスドラッグでボックスを移動またはリサイズする。
右下角付近をドラッグするとリサイズ、それ以外は移動。"
  (interactive "e")
  (let* ((posn (event-start event))
         (rc   (posn-col-row posn))
         (start-col (car rc))
         (start-row (cdr rc))
         (box  (ew--box-at start-row start-col)))
    (when box
      (ew--push-undo)
      (ew--bring-to-front box)
      (if (ew--resize-region-p box start-row start-col)
          (ew--do-resize box start-row start-col)
        (ew--do-move box start-row start-col)))))

(defun ew--do-move (box start-row start-col)
  "BOXをドラッグ移動する。START-ROW/START-COLはドラッグ開始位置。"
  (let ((off-col (- start-col (ew--box-col box)))
        (off-row (- start-row (ew--box-row box))))
    (track-mouse
      (catch 'done
        (while t
          (let ((ev (read-event)))
            (cond
             ((mouse-movement-p ev)
              (let* ((p   (event-start ev))
                     (rc2 (posn-col-row p)))
                (when rc2
                  (setf (ew--box-col box) (- (car rc2) off-col))
                  (setf (ew--box-row box) (- (cdr rc2) off-row))
                  (ew--clamp-box-position box)
                  (ew--redraw))))
             (t (throw 'done nil)))))))))

(defun ew--do-resize (box _start-row _start-col)
  "BOXをドラッグリサイズする。右下角をドラッグで幅・高さを変更。
物理列幅を考慮して論理内部幅を逆算する。"
  (let ((box-r (ew--box-row box))
        (box-c (ew--box-col box))
        (cw (ew--border-corner-width))
        (hw (ew--border-h-width))
        (min-w 4)   ;; 最小内側幅(論理)
        (min-h 1))  ;; 最小内側高さ
    (track-mouse
      (catch 'done
        (while t
          (let ((ev (read-event)))
            (cond
             ((mouse-movement-p ev)
              (let* ((p   (event-start ev))
                     (rc2 (posn-col-row p)))
                (when rc2
                  (let* ((new-br-col (car rc2))
                         (new-br-row (cdr rc2))
                         ;; 物理列差分から論理内部幅を逆算
                         ;; total-w = corner + iw*hw + corner
                         ;; iw = (total-w - 2*corner) / hw
                         (phys-inner (- new-br-col box-c cw))
                         (new-w (max min-w (/ phys-inner hw)))
                         (new-h (max min-h (- new-br-row box-r 1))))
                    ;; キャンバス範囲内にクランプ
                    (let ((max-w (/ (- ew--canvas-cols box-c (* 2 cw)) hw)))
                      (setq new-w (min new-w max-w)))
                    (setq new-h (min new-h (- ew--canvas-rows box-r 2)))
                    (setf (ew--box-width box) new-w)
                    (setf (ew--box-height box) new-h)
                    (ew--redraw)))))
             (t (throw 'done nil)))))))))


;;;; ============================================================
;;;; undo/redo（スナップショットベース）
;;;; ============================================================

(defun ew--snapshot ()
  "現在の状態のスナップショットを返す。"
  (list :boxes (mapcar #'copy-ew--box ew--boxes)
        :connections (mapcar #'copy-ew--connection ew--connections)
        :next-z ew--next-z))

(defun ew--restore-snapshot (snap)
  "スナップショットSNAPから状態を復元する。"
  (setq ew--boxes (plist-get snap :boxes))
  (setq ew--connections (plist-get snap :connections))
  (setq ew--next-z (plist-get snap :next-z)))

(defun ew--push-undo ()
  "現在の状態をundoスタックにプッシュし、redoスタックをクリアする。"
  (push (ew--snapshot) ew--undo-stack)
  (setq ew--redo-stack nil)
  ;; undoスタックの最大サイズ制限（50件）
  (when (> (length ew--undo-stack) 50)
    (setq ew--undo-stack (cl-subseq ew--undo-stack 0 50))))

(defun ew-undo ()
  "直前の操作を元に戻す。"
  (interactive)
  (if (null ew--undo-stack)
      (message "元に戻す操作がありません")
    (push (ew--snapshot) ew--redo-stack)
    (ew--restore-snapshot (pop ew--undo-stack))
    (ew--redraw)
    (message "元に戻しました")))

(defun ew-redo ()
  "元に戻した操作をやり直す。"
  (interactive)
  (if (null ew--redo-stack)
      (message "やり直す操作がありません")
    (push (ew--snapshot) ew--undo-stack)
    (ew--restore-snapshot (pop ew--redo-stack))
    (ew--redraw)
    (message "やり直しました")))

;;;; ============================================================
;;;; ファイル保存・読み込み
;;;; ============================================================

(defun ew--serialize ()
  "現在の図をS-expression形式の文字列に変換する。"
  (let ((data (list
               :version 1
               :canvas-rows ew--canvas-rows
               :canvas-cols ew--canvas-cols
               :style (if (eq (plist-get ew--style :tl) ?┌) 'unicode 'ascii)
               :boxes (mapcar
                       (lambda (b)
                         (list :id (ew--box-id b)
                               :row (ew--box-row b)
                               :col (ew--box-col b)
                               :width (ew--box-width b)
                               :height (ew--box-height b)
                               :label (ew--box-label b)
                               :z-order (ew--box-z-order b)))
                       ew--boxes)
               :connections (mapcar
                             (lambda (c)
                               (list :id (ew--connection-id c)
                                     :from-id (ew--connection-from-id c)
                                     :to-id (ew--connection-to-id c)
                                     :from-side (ew--connection-from-side c)
                                     :to-side (ew--connection-to-side c)))
                             ew--connections))))
    (pp-to-string data)))

(defun ew--deserialize (str)
  "S-expression文字列STRから図データを復元する。"
  (let ((data (car (read-from-string str))))
    ;; バージョンチェック
    (unless (eq (plist-get data :version) 1)
      (error "未対応のファイルバージョン: %s" (plist-get data :version)))
    ;; キャンバス設定
    (setq ew--canvas-rows (or (plist-get data :canvas-rows) 30))
    (setq ew--canvas-cols (or (plist-get data :canvas-cols) 80))
    ;; スタイル
    (setq ew--style (if (eq (plist-get data :style) 'unicode)
                        ew-style-unicode
                      ew-style-ascii))
    ;; ボックス復元
    (setq ew--boxes nil)
    (setq ew--next-z 0)
    (dolist (bd (plist-get data :boxes))
      (let ((box (ew--box-create
                  :id (plist-get bd :id)
                  :row (plist-get bd :row)
                  :col (plist-get bd :col)
                  :width (plist-get bd :width)
                  :height (plist-get bd :height)
                  :label (plist-get bd :label)
                  :z-order (plist-get bd :z-order))))
        (push box ew--boxes)
        (setq ew--next-z (max ew--next-z (1+ (ew--box-z-order box))))))
    ;; 接続線復元
    (setq ew--connections nil)
    (dolist (cd (plist-get data :connections))
      (push (ew--connection-create
             :id (plist-get cd :id)
             :from-id (plist-get cd :from-id)
             :to-id (plist-get cd :to-id)
             :from-side (plist-get cd :from-side)
             :to-side (plist-get cd :to-side))
            ew--connections))
    ;; undo/redoスタックをクリア
    (setq ew--undo-stack nil)
    (setq ew--redo-stack nil)))

(defun ew-save-file (file)
  "図をFILEに保存する。"
  (interactive
   (list (read-file-name "保存先: " nil ew--file-name nil
                         (or ew--file-name "diagram.ew"))))
  ;; バッファローカル変数を先に読み取ってからファイルに書き込む
  (let ((content (ew--serialize))
        (coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n")
      (insert ";; Emacs Windows ダイアグラムファイル\n\n")
      (insert content)))
  (setq ew--file-name file)
  (message "保存しました: %s" file))

(defun ew-load-file (file)
  "FILEから図を読み込む。"
  (interactive
   (list (read-file-name "読み込み: " nil nil t nil
                         (lambda (f) (or (file-directory-p f)
                                         (string-suffix-p ".ew" f))))))
  (let ((coding-system-for-read 'utf-8))
    (ew--deserialize (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
  (setq ew--file-name file)
  (ew--redraw)
  (message "読み込みました: %s" file))

;;;; ============================================================
;;;; パブリックAPI
;;;; ============================================================

(defun ew-add-box (id label &optional row col width height)
  "ボックスを追加する。
ID はシンボル、LABEL は文字列。
ROW COL WIDTH HEIGHT は省略可。"
  (let ((box (ew--box-create
              :id id
              :label label
              :row (or row 1)
              :col (or col 1)
              :width (or width 16)
              :height (or height 3)
              :z-order ew--next-z)))
    (cl-incf ew--next-z)
    (push box ew--boxes)
    (ew--clamp-box-position box)
    box))

(defun ew-remove-box (id)
  "ID で指定したボックスを削除する。関連する接続線も削除。"
  (setq ew--boxes (cl-remove-if (lambda (b) (eq (ew--box-id b) id))
                                ew--boxes))
  ;; 関連する接続線も削除
  (setq ew--connections
        (cl-remove-if (lambda (c)
                        (or (eq (ew--connection-from-id c) id)
                            (eq (ew--connection-to-id c) id)))
                      ew--connections)))

(defun ew-add-connection (id from-id to-id &optional from-side to-side)
  "接続線を追加する。
ID はシンボル、FROM-ID/TO-ID はボックスID。
FROM-SIDE/TO-SIDE は辺の指定 (top/bottom/left/right)。"
  (let ((conn (ew--connection-create
               :id id
               :from-id from-id
               :to-id to-id
               :from-side (or from-side 'right)
               :to-side (or to-side 'left))))
    (push conn ew--connections)
    conn))

(defun ew-remove-connection (id)
  "ID で指定した接続線を削除する。"
  (setq ew--connections
        (cl-remove-if (lambda (c) (eq (ew--connection-id c) id))
                      ew--connections)))

(defun ew-set-style (style)
  "描画スタイルを切り替える。STYLE は plist。"
  (setq ew--style style)
  (ew--redraw))

;;;; ============================================================
;;;; インタラクティブコマンド
;;;; ============================================================

(defun ew-add-box-interactive ()
  "対話的にボックスを追加する。カーソル位置に配置。"
  (interactive)
  (let* ((label (read-string "ラベル: "))
         (id    (intern (format "box-%s" (length ew--boxes))))
         (row   (1- (line-number-at-pos)))
         (col   (current-column)))
    (ew--push-undo)
    (ew-add-box id label row col)
    (ew--redraw)
    (message "ボックス '%s' を追加しました" label)))

(defun ew-remove-box-interactive ()
  "対話的にボックスを削除する。"
  (interactive)
  (if (null ew--boxes)
      (message "削除できるボックスがありません")
    (let* ((names (mapcar (lambda (b)
                           (cons (format "%s (%s)" (ew--box-id b) (ew--box-label b))
                                 (ew--box-id b)))
                          ew--boxes))
           (choice (completing-read "削除するボックス: " names nil t))
           (id     (cdr (assoc choice names))))
      (ew--push-undo)
      (ew-remove-box id)
      (ew--redraw)
      (message "ボックス '%s' を削除しました" id))))

(defun ew--read-box-id (prompt)
  "PROMPTでボックスIDを対話的に読み取る。"
  (let* ((names (mapcar (lambda (b)
                          (cons (format "%s (%s)" (ew--box-id b) (ew--box-label b))
                                (ew--box-id b)))
                        ew--boxes))
         (choice (completing-read prompt names nil t)))
    (cdr (assoc choice names))))

(defun ew--read-side (prompt)
  "PROMPTで辺の方向を対話的に読み取る。"
  (let* ((sides '(("right" . right) ("left" . left)
                  ("top" . top) ("bottom" . bottom)))
         (choice (completing-read prompt sides nil t)))
    (cdr (assoc choice sides))))

(defun ew-add-connection-interactive ()
  "対話的に接続線を追加する。"
  (interactive)
  (if (< (length ew--boxes) 2)
      (message "接続線を作成するにはボックスが2つ以上必要です")
    (let* ((from-id   (ew--read-box-id "接続元ボックス: "))
           (from-side (ew--read-side "接続元の辺: "))
           (to-id     (ew--read-box-id "接続先ボックス: "))
           (to-side   (ew--read-side "接続先の辺: "))
           (id        (intern (format "conn-%s" (length ew--connections)))))
      (ew--push-undo)
      (ew-add-connection id from-id to-id from-side to-side)
      (ew--redraw)
      (message "接続線 %s → %s を追加しました" from-id to-id))))

(defun ew-remove-connection-interactive ()
  "対話的に接続線を削除する。"
  (interactive)
  (if (null ew--connections)
      (message "削除できる接続線がありません")
    (let* ((names (mapcar (lambda (c)
                            (cons (format "%s: %s → %s"
                                          (ew--connection-id c)
                                          (ew--connection-from-id c)
                                          (ew--connection-to-id c))
                                  (ew--connection-id c)))
                          ew--connections))
           (choice (completing-read "削除する接続線: " names nil t))
           (id     (cdr (assoc choice names))))
      (ew--push-undo)
      (ew-remove-connection id)
      (ew--redraw)
      (message "接続線 '%s' を削除しました" id))))

(defun ew-edit-label-interactive ()
  "カーソル位置のボックスのラベルを編集する。"
  (interactive)
  (let* ((row (1- (line-number-at-pos)))
         (col (current-column))
         (box (ew--box-at row col)))
    (if (null box)
        (message "ここにボックスがありません")
      (let* ((old-label (ew--box-label box))
             (new-label (read-string
                         (format "ラベル [%s]: " old-label)
                         nil nil old-label))
             (max-len (ew--box-inner-physical-width box))
             (trimmed (if (> (string-width new-label) max-len)
                          (truncate-string-to-width new-label max-len)
                        new-label)))
        (ew--push-undo)
        (setf (ew--box-label box) trimmed)
        (ew--redraw)
        (message "ラベルを '%s' に変更しました" trimmed)))))

(defun ew-edit-label-mouse (event)
  "マウスダブルクリック位置のボックスのラベルを編集する。"
  (interactive "e")
  (let* ((posn (event-start event))
         (rc   (posn-col-row posn))
         (col  (car rc))
         (row  (cdr rc))
         (box  (ew--box-at row col)))
    (if (null box)
        (message "ここにボックスがありません")
      (let* ((old-label (ew--box-label box))
             (new-label (read-string
                         (format "ラベル [%s]: " old-label)
                         nil nil old-label))
             (max-len (ew--box-inner-physical-width box))
             (trimmed (if (> (string-width new-label) max-len)
                          (truncate-string-to-width new-label max-len)
                        new-label)))
        (ew--push-undo)
        (setf (ew--box-label box) trimmed)
        (ew--redraw)
        (message "ラベルを '%s' に変更しました" trimmed)))))

(defun ew-toggle-style ()
  "Unicode/ASCII スタイルを切り替える。
スタイル変更後、罫線幅に応じてキャンバスサイズを調整する。"
  (interactive)
  (ew--push-undo)
  (if (eq (plist-get ew--style :tl) ?┌)
      (ew-set-style ew-style-ascii)
    (ew-set-style ew-style-unicode))
  ;; フォント測定キャッシュをリセット
  (setq ew--actual-border-width nil)
  ;; スタイルに応じてキャンバス列数を調整
  (setq ew--canvas-cols (if (> (ew--border-h-width) 1) 160 80))
  (message "スタイル: %s" (if (eq (plist-get ew--style :tl) ?┌) "Unicode" "ASCII")))

(defun ew-show-help ()
  "キーバインドのヘルプを表示する。"
  (interactive)
  (message (concat
            "【操作】 "
            "ドラッグ:移動 右下ドラッグ:リサイズ ダブルクリック:ラベル編集 | "
            "a:追加 d:削除 e:ラベル | "
            "c:接続 x:接続削除 | "
            "u:undo r:redo | "
            "C-x C-s:保存 C-x C-f:読込 | "
            "s:スタイル ?:ヘルプ q:終了")))

;;;; ============================================================
;;;; メジャーモード
;;;; ============================================================

(defvar ew-mode-map
  (let ((map (make-sparse-keymap)))
    ;; マウス操作
    (define-key map [down-mouse-1] #'ew--mouse-drag)
    (define-key map [double-mouse-1] #'ew-edit-label-mouse)
    ;; ボックス操作
    (define-key map (kbd "a") #'ew-add-box-interactive)
    (define-key map (kbd "d") #'ew-remove-box-interactive)
    (define-key map (kbd "e") #'ew-edit-label-interactive)
    ;; 接続線操作
    (define-key map (kbd "c") #'ew-add-connection-interactive)
    (define-key map (kbd "x") #'ew-remove-connection-interactive)
    ;; undo/redo
    (define-key map (kbd "u") #'ew-undo)
    (define-key map (kbd "r") #'ew-redo)
    ;; ファイル操作
    (define-key map (kbd "C-x C-s") #'ew-save-file)
    (define-key map (kbd "C-x C-f") #'ew-load-file)
    ;; その他
    (define-key map (kbd "s") #'ew-toggle-style)
    (define-key map (kbd "?") #'ew-show-help)
    (define-key map (kbd "q") #'quit-window)
    map)
  "ew-mode のキーマップ。")

(define-derived-mode ew-mode special-mode "EW"
  "Emacs Windows - テキストベースボックス図エディタ。

\\{ew-mode-map}"
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (setq ew--style ew-style-unicode)
  (setq ew--next-z 0)
  (setq ew--boxes nil)
  (setq ew--connections nil)
  (setq ew--undo-stack nil)
  (setq ew--redo-stack nil)
  (setq ew--file-name nil)
  (setq ew--actual-border-width nil)  ;; フォント測定をリセット
  ;; 罫線の実表示幅に応じてキャンバスを拡張
  (setq ew--canvas-cols (if (> (ew--border-h-width) 1) 160 80)))

;;;; ============================================================
;;;; エントリポイント
;;;; ============================================================

;;;###autoload
(defun emacs-windows ()
  "テキストベースボックス図エディタを起動する。"
  (interactive)
  (let ((buf (get-buffer-create "*Emacs Windows*")))
    (switch-to-buffer buf)
    (ew-mode)
    ;; デモ用のボックスを配置（colはhwの倍数にスナップされる）
    (ew-add-box 'box-a "A" 2 2 16 3)
    (ew-add-box 'box-b "B" 8 50 20 5)
    (ew-add-box 'box-c "C" 18 10 12 3)
    ;; デモ用の接続線
    (ew-add-connection 'conn-ab 'box-a 'box-b 'right 'left)
    (ew-add-connection 'conn-bc 'box-b 'box-c 'bottom 'top)
    (ew--redraw)
    (message "?:ヘルプ | ドラッグ:移動 a:追加 d:削除 c:接続 u:undo r:redo C-x C-s:保存 C-x C-f:読込")))

(provide 'emacs-windows)

;;; emacs-windows.el ends here
