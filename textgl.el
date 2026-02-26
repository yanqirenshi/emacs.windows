;;; textgl.el --- テキストベースボックス図エディタ -*- lexical-binding: t; -*-

;; Author: yanqi
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: diagrams, tools

;;; Commentary:

;; textGL - テキストベースのボックス図エディタ。
;; 複数のボックスをマウスドラッグで移動できる。
;; Unicode罫線(┌─┐│└─┘)をデフォルトで使用。
;;
;; 使い方:
;;   M-x textgl

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
;;;; ボックス描画
;;;; ============================================================

(defun textgl--draw-box (box)
  "BOXをグリッドに物理列座標で描画する。
罫線文字の実表示幅(textgl--border-*-width)を考慮して座標を計算する。"
  (let* ((r  (textgl--box-row box))
         (c  (textgl--box-col box))        ;; 物理列
         (iw (textgl--box-width box))      ;; 論理内部幅
         (ih (textgl--box-height box))
         (st (or textgl--style textgl-style-unicode))
         (tl (plist-get st :tl))
         (tr (plist-get st :tr))
         (bl (plist-get st :bl))
         (br (plist-get st :br))
         (h  (plist-get st :h))
         (v  (plist-get st :v))
         (cw (textgl--border-corner-width)) ;; 角文字の実表示幅
         (hw (textgl--border-h-width))     ;; 水平罫線の実表示幅
         (vw (textgl--border-v-width))     ;; 垂直罫線の実表示幅
         (inner-phys (* iw hw)))       ;; 内部の物理列幅
    ;; 上ボーダー: 角 + 水平罫線×iw + 角
    (let ((pc c))
      (textgl--grid-set r pc tl)
      (setq pc (+ pc cw))
      (dotimes (_i iw)
        (textgl--grid-set r pc h)
        (setq pc (+ pc hw)))
      (textgl--grid-set r pc tr))
    ;; 左右の縦線 + 内部を空白で埋める
    (dotimes (i ih)
      (let ((row (+ r 1 i))
            (pc c))
        (textgl--grid-set row pc v)
        (setq pc (+ pc vw))
        ;; 内部スペース: inner-phys個（上辺と同じ物理幅）
        (dotimes (_j inner-phys)
          (textgl--grid-set row pc ?\s)
          (setq pc (1+ pc)))
        (textgl--grid-set row pc v)))
    ;; 下ボーダー: 角 + 水平罫線×iw + 角
    (let ((br-row (+ r ih 1))
          (pc c))
      (textgl--grid-set br-row pc bl)
      (setq pc (+ pc cw))
      (dotimes (_i iw)
        (textgl--grid-set br-row pc h)
        (setq pc (+ pc hw)))
      (textgl--grid-set br-row pc br))
    ;; ラベルを内部中央に配置
    (let* ((label (textgl--box-label box))
           (label-w (string-width label))  ;; ラベルの表示幅
           (mid-row (+ r 1 (/ ih 2)))
           (inner-start (+ c vw))
           (lbl-start (+ inner-start (/ (- inner-phys label-w) 2))))
      (dotimes (i (length label))
        (let ((ch (aref label i)))
          (textgl--grid-set mid-row lbl-start ch)
          (setq lbl-start (+ lbl-start (char-width ch))))))))

(defun textgl--draw-all ()
  "全ボックスをz-order昇順でグリッドに描画する。"
  (let ((sorted (sort (copy-sequence textgl--boxes)
                      (lambda (a b)
                        (< (textgl--box-z-order a) (textgl--box-z-order b))))))
    (dolist (box sorted)
      (textgl--draw-box box))))

;;;; ============================================================
;;;; 接続線描画
;;;; ============================================================

(defun textgl--find-box (id)
  "IDでボックスを検索して返す。"
  (cl-find-if (lambda (b) (eq (textgl--box-id b) id)) textgl--boxes))

(defun textgl--box-anchor (box side)
  "BOXのSIDE辺の中点座標を(row . col)で物理列座標で返す。
水平座標はhwの倍数にアラインして全角罫線との整合性を保つ。"
  (let* ((r (textgl--box-row box))
         (c (textgl--box-col box))
         (h (textgl--box-height box))
         (hw (textgl--border-h-width))
         (total-w (textgl--box-total-physical-width box))
         (total-h (+ h 2))
         (mid-col (* (/ (+ c (/ total-w 2)) hw) hw))    ;; hwの倍数にアライン
         (right-col (* (/ (+ c total-w -1) hw) hw)))    ;; 右辺もアライン
    (cl-case side
      (top    (cons r                    mid-col))
      (bottom (cons (+ r total-h -1)     mid-col))
      (left   (cons (+ r (/ total-h 2)) c))
      (right  (cons (+ r (/ total-h 2)) right-col)))))

(defun textgl--line-chars ()
  "現在のスタイルに対応する接続線文字を返す。"
  (if (eq (plist-get textgl--style :tl) ?┌)
      textgl-line-chars-unicode
    textgl-line-chars-ascii))

(defun textgl--box-bounds (box)
  "BOXの境界を(top left bottom right)のplistで返す。物理列座標。"
  (let ((r (textgl--box-row box))
        (c (textgl--box-col box))
        (tw (textgl--box-total-physical-width box))
        (th (+ (textgl--box-height box) 2)))
    (list :top r :left c :bottom (+ r th -1) :right (+ c tw -1))))

(defun textgl--align-col (c)
  "列Cをhwの倍数にアラインして返す。"
  (let ((hw (textgl--border-h-width)))
    (if (> hw 1) (* (/ c hw) hw) c)))

(defun textgl--route-connection (from-side to-side r1 c1 r2 c2 from-box to-box)
  "ウェイポイントリストを計算する。
FROM-SIDE/TO-SIDEは出発/到着辺、(R1,C1)/(R2,C2)はオフセット済み端点。
FROM-BOX/TO-BOXはボックスオブジェクト。
戻り値は ((r . c) ...) のリスト。最初と最後は(r1.c1)と(r2.c2)。"
  (let* ((hw (textgl--border-h-width))
         (margin-h (* hw 2))  ;; 水平方向の迂回マージン
         (margin-v 2)         ;; 垂直方向の迂回マージン
         (fb (textgl--box-bounds from-box))
         (tb (textgl--box-bounds to-box))
         ;; 出発方向の判定用
         (h-from (memq from-side '(left right)))
         (h-to   (memq to-side   '(left right))))
    (cond
     ;; 完全に直線
     ((and (= r1 r2) (or h-from h-to))
      (list (cons r1 c1) (cons r2 c2)))
     ((and (= c1 c2) (or (not h-from) (not h-to)))
      (list (cons r1 c1) (cons r2 c2)))

     ;; === 水平出発 → 水平到着 ===
     ((and h-from h-to)
      (let ((out-right (eq from-side 'right))
            (in-left   (eq to-side 'left)))
        (cond
         ;; right→left: 順方向（fromが左側）
         ((and out-right in-left (< c1 c2))
          (let ((mc (textgl--align-col (/ (+ c1 c2) 2))))
            (list (cons r1 c1) (cons r1 mc) (cons r2 mc) (cons r2 c2))))
         ;; left→right: 順方向（fromが右側）
         ((and (not out-right) (not in-left) (> c1 c2))
          (let ((mc (textgl--align-col (/ (+ c1 c2) 2))))
            (list (cons r1 c1) (cons r1 mc) (cons r2 mc) (cons r2 c2))))
         ;; right→left: 逆方向（fromが右側）またはright→right, left→left等
         (t
          (let* ((mr (/ (+ r1 r2) 2))
                 ;; 迂回列: 両ボックスの外側
                 (esc-c1 (textgl--align-col
                           (if out-right
                               (+ (max c1 (+ (plist-get fb :right) hw)) margin-h)
                             (- (min c1 (- (plist-get fb :left) hw)) margin-h))))
                 (esc-c2 (textgl--align-col
                           (if in-left
                               (- (min c2 (- (plist-get tb :left) hw)) margin-h)
                             (+ (max c2 (+ (plist-get tb :right) hw)) margin-h)))))
            (if (= esc-c1 esc-c2)
                ;; 同じ迂回列なら3セグメント
                (list (cons r1 c1) (cons r1 esc-c1) (cons r2 esc-c1) (cons r2 c2))
              ;; 異なる迂回列なら5セグメント
              (list (cons r1 c1) (cons r1 esc-c1)
                    (cons mr esc-c1) (cons mr esc-c2)
                    (cons r2 esc-c2) (cons r2 c2))))))))

     ;; === 垂直出発 → 垂直到着 ===
     ((and (not h-from) (not h-to))
      (let ((out-down (eq from-side 'bottom))
            (in-top   (eq to-side 'top)))
        (cond
         ;; bottom→top: 順方向（fromが上側）
         ((and out-down in-top (< r1 r2))
          (let ((mr (/ (+ r1 r2) 2)))
            (list (cons r1 c1) (cons mr c1) (cons mr c2) (cons r2 c2))))
         ;; top→bottom: 順方向（fromが下側）
         ((and (not out-down) (not in-top) (> r1 r2))
          (let ((mr (/ (+ r1 r2) 2)))
            (list (cons r1 c1) (cons mr c1) (cons mr c2) (cons r2 c2))))
         ;; 逆方向またはbottom→bottom, top→top等
         (t
          (let* ((mc (textgl--align-col (/ (+ c1 c2) 2)))
                 (esc-r1 (if out-down
                             (+ (max r1 (1+ (plist-get fb :bottom))) margin-v)
                           (- (min r1 (1- (plist-get fb :top))) margin-v)))
                 (esc-r2 (if in-top
                             (- (min r2 (1- (plist-get tb :top))) margin-v)
                           (+ (max r2 (1+ (plist-get tb :bottom))) margin-v))))
            (if (= esc-r1 esc-r2)
                (list (cons r1 c1) (cons esc-r1 c1) (cons esc-r1 c2) (cons r2 c2))
              (list (cons r1 c1) (cons esc-r1 c1)
                    (cons esc-r1 mc) (cons esc-r2 mc)
                    (cons esc-r2 c2) (cons r2 c2))))))))

     ;; === 水平出発 → 垂直到着 ===
     (h-from
      (let ((out-right (eq from-side 'right))
            (in-top    (eq to-side 'top)))
        (cond
         ;; 順方向: 1折れで到達可能
         ;; right→top: fromが左上（c1<c2 かつ r1<r2）
         ;; right→bottom: fromが左下（c1<c2 かつ r1>r2）
         ;; left→top: fromが右上（c1>c2 かつ r1<r2）
         ;; left→bottom: fromが右下（c1>c2 かつ r1>r2）
         ((and (if out-right (< c1 c2) (> c1 c2))
               (if in-top (< r1 r2) (> r1 r2)))
          (list (cons r1 c1) (cons r1 c2) (cons r2 c2)))
         ;; 逆方向: 3折れ
         (t
          (let* ((esc-c (textgl--align-col
                          (if out-right
                              (+ (max c1 (+ (plist-get fb :right) hw)) margin-h)
                            (- (min c1 (- (plist-get fb :left) hw)) margin-h))))
                 (esc-r (if in-top
                            (- (min r2 (1- (plist-get tb :top))) margin-v)
                          (+ (max r2 (1+ (plist-get tb :bottom))) margin-v))))
            (list (cons r1 c1) (cons r1 esc-c)
                  (cons esc-r esc-c) (cons esc-r c2)
                  (cons r2 c2)))))))

     ;; === 垂直出発 → 水平到着 ===
     (t
      (let ((out-down (eq from-side 'bottom))
            (in-left  (eq to-side 'left)))
        (cond
         ;; 順方向: 1折れで到達可能
         ;; bottom→left: fromが上で右（r1<r2 かつ c1>c2）
         ;; bottom→right: fromが上で左（r1<r2 かつ c1<c2）
         ;; top→left: fromが下で右（r1>r2 かつ c1>c2）
         ;; top→right: fromが下で左（r1>r2 かつ c1<c2）
         ((and (if out-down (< r1 r2) (> r1 r2))
               (if in-left (> c1 c2) (< c1 c2)))
          (list (cons r1 c1) (cons r2 c1) (cons r2 c2)))
         ;; 逆方向: 3折れ
         (t
          (let* ((esc-r (if out-down
                            (+ (max r1 (1+ (plist-get fb :bottom))) margin-v)
                          (- (min r1 (1- (plist-get fb :top))) margin-v)))
                 (esc-c (textgl--align-col
                          (if in-left
                              (- (min c2 (- (plist-get tb :left) hw)) margin-h)
                            (+ (max c2 (+ (plist-get tb :right) hw)) margin-h)))))
            (list (cons r1 c1) (cons esc-r c1)
                  (cons esc-r esc-c) (cons r2 esc-c)
                  (cons r2 c2))))))))))

(defun textgl--draw-waypoints (waypoints lc)
  "WAYPOINTSのリストに沿って接続線とLCの角文字を描画する。
連続する2点間を水平/垂直の線で描画し、折れ点に角文字を配置する。"
  (let ((ch-h (plist-get lc :h))
        (ch-v (plist-get lc :v))
        (pts waypoints))
    ;; 各セグメントを描画
    (while (cdr pts)
      (let ((p1 (car pts))
            (p2 (cadr pts)))
        (if (= (car p1) (car p2))
            ;; 水平セグメント
            (textgl--draw-h-line (car p1) (cdr p1) (cdr p2) ch-h)
          ;; 垂直セグメント
          (textgl--draw-v-line (cdr p1) (car p1) (car p2) ch-v)))
      (setq pts (cdr pts)))
    ;; 折れ点に角文字を配置（最初と最後のポイントは除く）
    (let ((pts waypoints))
      (while (cddr pts)
        (let* ((prev (car pts))
               (curr (cadr pts))
               (next (caddr pts))
               ;; prevからcurrへの方向
               (from-dir (cond
                          ((< (cdr prev) (cdr curr)) 'from-left)
                          ((> (cdr prev) (cdr curr)) 'from-right)
                          ((< (car prev) (car curr)) 'from-up)
                          (t                          'from-down)))
               ;; currからnextへの方向
               (to-dir (cond
                        ((< (cdr curr) (cdr next)) 'to-right)
                        ((> (cdr curr) (cdr next)) 'to-left)
                        ((< (car curr) (car next)) 'to-down)
                        (t                          'to-up))))
          (textgl--grid-set (car curr) (cdr curr)
            (textgl--corner-char lc from-dir to-dir)))
        (setq pts (cdr pts))))))

(defun textgl--draw-connection (conn)
  "接続線CONNをグリッドに描画する。
ボックスの位置関係に応じて最大5回折れのルーティングを自動選択する。
描画順序: 接続線→ボックスの順なので、ボックスと重なる部分はボックスが上書きする。"
  (let* ((from-box (textgl--find-box (textgl--connection-from-id conn)))
         (to-box   (textgl--find-box (textgl--connection-to-id conn))))
    (when (and from-box to-box)
      (let* ((from-side (textgl--connection-from-side conn))
             (to-side   (textgl--connection-to-side conn))
             (from-pt (textgl--box-anchor from-box from-side))
             (to-pt   (textgl--box-anchor to-box   to-side))
             (r1 (car from-pt)) (c1 (cdr from-pt))
             (r2 (car to-pt))   (c2 (cdr to-pt))
             (lc (textgl--line-chars)))
        ;; ボックスの辺から外側にオフセット
        (let ((hw (textgl--border-h-width)))
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
          ;; ルート計算→描画
          (let ((waypoints (textgl--route-connection
                            from-side to-side r1 c1 r2 c2
                            from-box to-box)))
            (textgl--draw-waypoints waypoints lc)))))))

(defun textgl--draw-connection-arrow (conn)
  "接続線CONNの矢印を描画する（ボックス描画後に呼ぶ）。
矢印はボックスの枠線の手前（1セル外側）に配置する。"
  (let* ((to-box (textgl--find-box (textgl--connection-to-id conn)))
         (to-side (textgl--connection-to-side conn)))
    (when to-box
      (let* ((to-pt (textgl--box-anchor to-box to-side))
             (lc (textgl--line-chars))
             (hw (textgl--border-h-width))
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
        (textgl--grid-set ar ac
          (plist-get lc (cl-case to-side
                          (left  :arrow-right)
                          (right :arrow-left)
                          (top   :arrow-down)
                          (bottom :arrow-up))))))))

(defun textgl--corner-char (lc from-dir to-dir)
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

(defun textgl--draw-h-line (row c1 c2 char)
  "水平線をROW行にC1からC2まで物理列で描画する。
開始位置をhwの倍数にアラインし、全角罫線の隙間を防ぐ。"
  (let* ((start (min c1 c2))
         (end   (max c1 c2))
         (w     (textgl--border-h-width))
         ;; 開始位置をwの倍数に切り下げてアライン（隙間防止）
         (aligned-start (* (/ start w) w)))
    (cl-loop for c from aligned-start to end by w
             do (textgl--grid-set row c char))))

(defun textgl--draw-v-line (col r1 r2 char)
  "垂直線をCOL列にR1からR2まで描画する。行方向は幅に無関係。"
  (let ((start (min r1 r2))
        (end   (max r1 r2)))
    (cl-loop for r from start to end
             do (textgl--grid-set r col char))))

(defun textgl--draw-all-connections ()
  "全接続線の線をグリッドに描画する。"
  (dolist (conn textgl--connections)
    (textgl--draw-connection conn)))

(defun textgl--draw-all-connection-arrows ()
  "全接続線の矢印をグリッドに描画する（ボックス描画後に呼ぶ）。"
  (dolist (conn textgl--connections)
    (textgl--draw-connection-arrow conn)))

;;;; ============================================================
;;;; インスペクタ描画
;;;; ============================================================

(defun textgl--inspector-content (box)
  "BOXのインスペクタ表示内容を行のリストで返す。
各行は (key . text) の形式。keyは編集用キー、textは表示文字列。"
  (list
   (cons nil (format " %s" (textgl--box-id box)))
   (cons ?1 (format " 1: ラベル: %s" (textgl--box-label box)))
   (cons ?2 (format " 2: 行: %d" (textgl--box-row box)))
   (cons ?3 (format " 3: 列: %d" (textgl--box-col box)))
   (cons ?4 (format " 4: 幅: %d" (textgl--box-width box)))
   (cons ?5 (format " 5: 高さ: %d" (textgl--box-height box)))
   (cons ?6 (format " 6: Z: %d" (textgl--box-z-order box)))))

(defun textgl--inspector-panel-size (lines)
  "LINESの内容に基づくインスペクタパネルのサイズを(width . height)で返す。
widthは最大行幅+左右罫線幅、heightは行数+2（上下罫線分）。
罫線文字が全角の場合、内部幅を全角幅の倍数に切り上げる。"
  (let* ((max-width 0)
         (border-w (char-width ?│))  ;; 縦罫線の幅
         (h-w (char-width ?─)))      ;; 水平罫線の幅
    (dolist (line lines)
      (setq max-width (max max-width (string-width (cdr line)))))
    ;; 内部幅を水平罫線幅の倍数に切り上げ
    (let ((inner-w (* (ceiling (float max-width) h-w) h-w)))
      (cons (+ inner-w (* 2 border-w)) (+ (length lines) 2)))))

(defun textgl--draw-inspector ()
  "選択中ボックスのインスペクタをグリッドに描画する。
クリック位置を起点に表示。全角罫線の幅を正しく考慮する。"
  (when (and textgl--selected-box textgl--inspector-row textgl--inspector-col)
    (let ((box (textgl--find-box textgl--selected-box)))
      (when box
        (let* ((lines (textgl--inspector-content box))
               (panel-size (textgl--inspector-panel-size lines))
               (panel-w (car panel-size))  ;; 表示幅（string-width）
               (panel-h (cdr panel-size))
               ;; インスペクタの配置位置（クリック位置を起点、全角幅にアライン）
               (insp-row textgl--inspector-row)
               (hw (char-width ?─))
               (insp-col (if (> hw 1)
                             (* (/ textgl--inspector-col hw) hw)
                           textgl--inspector-col)))
          ;; 描画用の罫線文字と幅
          (let* ((tl ?┌) (tr ?┐) (bl ?└) (br ?┘) (h ?─) (v ?│)
                 (corner-w (char-width tl))  ;; 角文字の幅
                 (h-w (char-width h))        ;; 水平罫線の幅
                 (v-w (char-width v))        ;; 垂直罫線の幅
                 ;; 内部の表示幅（左右罫線を除く）
                 (inner-w (- panel-w (* 2 v-w)))
                 ;; 水平罫線の数
                 (h-count (/ inner-w h-w)))
            ;; まずインスペクタ領域を空白でクリア（ボックスとの重なりを防ぐ）
            (dotimes (row-off panel-h)
              (let ((r (+ insp-row row-off)))
                (dotimes (col-off panel-w)
                  (textgl--grid-set r (+ insp-col col-off) ?\s))))
            ;; 上辺: 角 + 水平罫線 + 角
            (let ((col insp-col))
              (textgl--grid-set insp-row col tl)
              (setq col (+ col corner-w))
              (dotimes (_i h-count)
                (textgl--grid-set insp-row col h)
                (setq col (+ col h-w)))
              (textgl--grid-set insp-row col tr))
            ;; 内容行
            (let ((row-idx 1))
              (dolist (line lines)
                (let ((text (cdr line))
                      (r (+ insp-row row-idx)))
                  ;; 左罫線
                  (textgl--grid-set r insp-col v)
                  ;; テキスト内容（1文字ずつ）
                  (let ((col (+ insp-col v-w)))
                    (dotimes (i (length text))
                      (let ((ch (aref text i)))
                        (textgl--grid-set r col ch)
                        (setq col (+ col (max 1 (char-width ch))))))
                    ;; 残りを空白で埋める（右罫線の位置まで）
                    (let ((right-border-col (+ insp-col v-w inner-w)))
                      (while (< col right-border-col)
                        (textgl--grid-set r col ?\s)
                        (setq col (1+ col)))
                      ;; 右罫線
                      (textgl--grid-set r right-border-col v)))
                  (setq row-idx (1+ row-idx)))))
            ;; 下辺: 角 + 水平罫線 + 角
            (let ((bottom-row (+ insp-row panel-h -1))
                  (col insp-col))
              (textgl--grid-set bottom-row col bl)
              (setq col (+ col corner-w))
              (dotimes (_i h-count)
                (textgl--grid-set bottom-row col h)
                (setq col (+ col h-w)))
              (textgl--grid-set bottom-row col br))))))))

;;;; ============================================================
;;;; レンダリング
;;;; ============================================================

(defun textgl--render ()
  "グリッド内容をバッファに反映する。
パディングマーカーはスキップし、全角文字の表示幅を正しく処理する。
ウィンドウのスクロール位置を保持する。"
  (let* ((inhibit-read-only t)
         (win (selected-window))
         ;; スクロール位置を行数・列数で保存
         (start-line (count-lines (point-min) (window-start win)))
         (hscroll (window-hscroll win)))
    (erase-buffer)
    (dotimes (i textgl--canvas-rows)
      (let ((row-vec (aref textgl--grid i)))
        (dotimes (j textgl--canvas-cols)
          (let ((cell (aref row-vec j)))
            (unless (eq cell textgl--padding)
              (insert (if (characterp cell) cell ?\s))))))
      (insert "\n"))
    ;; スクロール位置を復元（行数ベース）
    (goto-char (point-min))
    (forward-line start-line)
    (set-window-start win (point) t)
    (set-window-hscroll win hscroll)))

(defun textgl--required-canvas-rows ()
  "全ボックスと接続線を収めるのに必要な最小キャンバス行数を返す。
最低30行は確保し、余白として下に3行追加する。"
  (let ((max-row 0))
    ;; 全ボックスの下端を計算
    (dolist (box textgl--boxes)
      (let ((bottom (+ (textgl--box-row box) (textgl--box-height box) 2)))
        (setq max-row (max max-row bottom))))
    ;; 接続線の矢印位置も考慮（to側ボックスの1行外側）
    (dolist (conn textgl--connections)
      (let ((to-box (textgl--find-box (textgl--connection-to-id conn))))
        (when to-box
          (let* ((to-side (textgl--connection-to-side conn))
                 (anchor-r (car (textgl--box-anchor to-box to-side)))
                 (arrow-r (cl-case to-side
                            (top    (- anchor-r 1))
                            (bottom (+ anchor-r 1))
                            (t      anchor-r))))
            (setq max-row (max max-row (1+ arrow-r)))))))
    ;; 最低30行 + 余白3行
    (max 30 (+ max-row 3))))

(defun textgl--required-canvas-cols ()
  "全ボックスと接続線を収めるのに必要な最小キャンバス列数を返す。
最低80列（全角罫線時は160列）は確保し、余白として右にhw*4列追加する。"
  (let ((max-col 0)
        (hw (textgl--border-h-width)))
    ;; 全ボックスの右端を計算
    (dolist (box textgl--boxes)
      (let ((right-col (+ (textgl--box-col box) (textgl--box-total-physical-width box))))
        (setq max-col (max max-col right-col))))
    ;; 接続線の矢印位置も考慮（to側ボックスのhw列外側）
    (dolist (conn textgl--connections)
      (let ((to-box (textgl--find-box (textgl--connection-to-id conn))))
        (when to-box
          (let* ((to-side (textgl--connection-to-side conn))
                 (anchor-c (cdr (textgl--box-anchor to-box to-side)))
                 (arrow-c (cl-case to-side
                            (left  (- anchor-c hw))
                            (right (+ anchor-c hw))
                            (t     anchor-c))))
            (setq max-col (max max-col (+ arrow-c hw)))))))
    ;; 最低80列（全角時160列）+ 余白
    (max (if (> hw 1) 160 80) (+ max-col (* hw 4)))))

(defun textgl--redraw ()
  "グリッド初期化→接続線→ボックス→矢印→インスペクタ→バッファ反映の一括実行。
キャンバスサイズを内容に合わせて自動調整する。"
  ;; キャンバスサイズを必要に応じて拡張・縮小
  (setq textgl--canvas-rows (textgl--required-canvas-rows))
  (setq textgl--canvas-cols (textgl--required-canvas-cols))
  (textgl--init-grid)
  (textgl--draw-all-connections)       ;; 1. 接続線（線のみ）
  (textgl--draw-all)                   ;; 2. ボックス（接続線の上に描画）
  (textgl--draw-all-connection-arrows) ;; 3. 矢印（ボックスの辺の上に描画）
  (textgl--draw-inspector)             ;; 4. インスペクタ（最前面に描画）
  (textgl--render))

;;;; ============================================================
;;;; ボックス判定
;;;; ============================================================

(defun textgl--box-hit-p (box row col)
  "BOXの物理範囲内に(ROW, COL)があればtを返す。"
  (let ((total-w (textgl--box-total-physical-width box)))
    (and (>= row (textgl--box-row box))
         (<= row (+ (textgl--box-row box) (textgl--box-height box) 1))
         (>= col (textgl--box-col box))
         (< col (+ (textgl--box-col box) total-w)))))

(defun textgl--box-at (row col)
  "指定座標にあるボックスのうちz-order最前面のものを返す。"
  (let ((hit nil))
    (dolist (box textgl--boxes)
      (when (textgl--box-hit-p box row col)
        (if (null hit)
            (setq hit box)
          (when (> (textgl--box-z-order box) (textgl--box-z-order hit))
            (setq hit box)))))
    hit))

;;;; ============================================================
;;;; Z-order管理
;;;; ============================================================

(defun textgl--bring-to-front (box)
  "BOXを最前面に移動する。"
  (setf (textgl--box-z-order box) textgl--next-z)
  (cl-incf textgl--next-z))

;;;; ============================================================
;;;; 位置クランプ
;;;; ============================================================

(defun textgl--clamp-box-position (box)
  "BOXの位置をクランプする。
行方向・列方向とも上端/左端(0)のみクランプし、下/右方向は無制限。
キャンバスは再描画時に自動拡張・縮小される。hwの倍数にスナップ。"
  (let ((hw (textgl--border-h-width)))
    (setf (textgl--box-row box) (max 0 (textgl--box-row box)))
    (setf (textgl--box-col box) (max 0 (textgl--box-col box)))
    ;; 列位置をhwの倍数にスナップ（全角罫線のアライメント）
    (when (> hw 1)
      (setf (textgl--box-col box) (* (/ (textgl--box-col box) hw) hw)))))

;;;; ============================================================
;;;; マウスドラッグ（移動・リサイズ統合）
;;;; ============================================================

(defun textgl--resize-region-p (box row col)
  "BOXの右下角付近に(ROW, COL)があればtを返す。物理幅を考慮。"
  (let* ((total-w (textgl--box-total-physical-width box))
         (br-row (+ (textgl--box-row box) (textgl--box-height box) 1))
         (br-col (+ (textgl--box-col box) total-w -1)))
    (and (>= row (- br-row 1)) (<= row br-row)
         (>= col (- br-col 2)) (<= col br-col))))

(defun textgl--mouse-drag (event)
  "マウスドラッグでビューポートをパンする。
Ctrl+ドラッグでボックスを移動・リサイズする。"
  (interactive "e")
  (let* ((posn (event-start event))
         (rc   (posn-col-row posn))
         (start-col (car rc))
         (start-row (cdr rc))
         (ctrl-p (memq 'control (event-modifiers event)))
         (box  (and ctrl-p (textgl--box-at start-row start-col))))
    (if box
        (progn
          (textgl--push-undo)
          (textgl--bring-to-front box)
          (if (textgl--resize-region-p box start-row start-col)
              (textgl--do-resize box start-row start-col)
            (textgl--do-move box start-row start-col)))
      (textgl--do-pan start-row start-col))))

(defun textgl--do-move (box start-row start-col)
  "BOXをドラッグ移動する。START-ROW/START-COLはドラッグ開始位置。"
  (let ((off-col (- start-col (textgl--box-col box)))
        (off-row (- start-row (textgl--box-row box))))
    (track-mouse
      (catch 'done
        (while t
          (let ((ev (read-event)))
            (cond
             ((mouse-movement-p ev)
              (let* ((p   (event-start ev))
                     (rc2 (posn-col-row p)))
                (when rc2
                  (setf (textgl--box-col box) (- (car rc2) off-col))
                  (setf (textgl--box-row box) (- (cdr rc2) off-row))
                  (textgl--clamp-box-position box)
                  (textgl--redraw))))
             (t (throw 'done nil)))))))))

(defun textgl--do-resize (box _start-row _start-col)
  "BOXをドラッグリサイズする。右下角をドラッグで幅・高さを変更。
物理列幅を考慮して論理内部幅を逆算する。"
  (let ((box-r (textgl--box-row box))
        (box-c (textgl--box-col box))
        (cw (textgl--border-corner-width))
        (hw (textgl--border-h-width))
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
                  (let* ((ntextgl-br-col (car rc2))
                         (ntextgl-br-row (cdr rc2))
                         ;; 物理列差分から論理内部幅を逆算
                         ;; total-w = corner + iw*hw + corner
                         ;; iw = (total-w - 2*corner) / hw
                         (phys-inner (- ntextgl-br-col box-c cw))
                         (ntextgl-w (max min-w (/ phys-inner hw)))
                         (ntextgl-h (max min-h (- ntextgl-br-row box-r 1))))
                    ;; 幅・高さとも無制限（キャンバスが自動拡張される）
                    (setf (textgl--box-width box) ntextgl-w)
                    (setf (textgl--box-height box) ntextgl-h)
                    (textgl--redraw)))))
             (t (throw 'done nil)))))))))

(defun textgl--ensure-buffer-lines (needed-lines)
  "バッファの行数がNEEDED-LINES以上になるよう空行を追加する。"
  (let ((inhibit-read-only t)
        (current-lines (count-lines (point-min) (point-max))))
    (when (< current-lines needed-lines)
      (save-excursion
        (goto-char (point-max))
        (dotimes (_i (- needed-lines current-lines))
          (insert "\n"))))))

(defun textgl--do-pan (start-row start-col)
  "空白領域のドラッグでビューポートをパン（スクロール）する。
START-ROW/START-COLはドラッグ開始時のカーソル位置。
マウスの移動差分だけウィンドウのスクロール位置を逆方向に動かす。
下方向にスクロールする際、バッファ末尾を超える場合は空行を追加する。
ドラッグせずにリリースした場合はクリックとして処理する。"
  (let ((prev-row start-row)
        (prev-col start-col)
        (win (selected-window))
        (moved nil))  ;; 実際に移動したかどうか
    (track-mouse
      (catch 'done
        (while t
          (let ((ev (read-event)))
            (cond
             ((mouse-movement-p ev)
              (let* ((p   (event-start ev))
                     (rc2 (posn-col-row p)))
                (when rc2
                  (let* ((cur-col (car rc2))
                         (cur-row (cdr rc2))
                         (dr (- prev-row cur-row))
                         (dc (- prev-col cur-col)))
                    ;; 移動があったかチェック
                    (when (or (/= dr 0) (/= dc 0))
                      (setq moved t))
                    ;; 垂直スクロール
                    (when (/= dr 0)
                      ;; 下方向スクロール時、バッファが足りなければ空行を追加
                      (when (> dr 0)
                        (let ((visible-end (+ (count-lines (point-min) (window-start win))
                                              (window-body-height win)
                                              dr)))
                          (textgl--ensure-buffer-lines visible-end)))
                      (let* ((ws (window-start win))
                             (ntextgl-start (save-excursion
                                          (goto-char ws)
                                          (forward-line dr)
                                          (point))))
                        (set-window-start win (max (point-min) ntextgl-start))))
                    ;; 水平スクロール
                    (when (/= dc 0)
                      (set-window-hscroll win
                        (max 0 (+ (window-hscroll win) dc))))
                    (setq prev-row cur-row
                          prev-col cur-col)))))
             (t (throw 'done nil)))))))
    ;; ドラッグしなかった場合はクリックとして処理
    (unless moved
      (textgl--handle-click start-row start-col))))

;;;; ============================================================
;;;; インスペクタ操作
;;;; ============================================================

(defun textgl--handle-click (row col)
  "座標(ROW, COL)でのクリック処理を行う。
ボックス上をクリック → そのボックスを選択しインスペクタ表示
空白をクリック → 選択解除、インスペクタ非表示"
  (let ((box (textgl--box-at row col)))
    (if box
        (progn
          (setq textgl--selected-box (textgl--box-id box))
          ;; クリック位置をインスペクタの表示位置として記録
          (setq textgl--inspector-row row)
          (setq textgl--inspector-col col)
          (textgl--redraw)
          (message "ボックス '%s' を選択 (1-6:編集 ESC:解除)"
                   (textgl--box-id box)))
      ;; 空白クリック → 選択解除
      (when textgl--selected-box
        (setq textgl--selected-box nil)
        (setq textgl--inspector-row nil)
        (setq textgl--inspector-col nil)
        (textgl--redraw)
        (message "選択解除")))))

(defun textgl--mouse-click (event)
  "マウスクリックでボックス選択/インスペクタ表示を切り替える。"
  (interactive "e")
  (let* ((posn (event-start event))
         (rc   (posn-col-row posn))
         (col  (car rc))
         (row  (cdr rc)))
    (textgl--handle-click row col)))

(defun textgl-deselect-box ()
  "ボックスの選択を解除する。"
  (interactive)
  (when textgl--selected-box
    (setq textgl--selected-box nil)
    (setq textgl--inspector-row nil)
    (setq textgl--inspector-col nil)
    (textgl--redraw)
    (message "選択解除")))

(defun textgl--edit-inspector-property (key)
  "KEYに対応するプロパティをミニバッファで編集する。
KEY: 1=ラベル, 2=行, 3=列, 4=幅, 5=高さ, 6=z-order"
  (when textgl--selected-box
    (let ((box (textgl--find-box textgl--selected-box)))
      (when box
        (textgl--push-undo)
        (pcase key
          (?1  ;; ラベル
           (let* ((old (textgl--box-label box))
                  (new (read-string (format "ラベル [%s]: " old) nil nil old))
                  (max-len (textgl--box-inner-physical-width box))
                  (trimmed (if (> (string-width new) max-len)
                               (truncate-string-to-width new max-len)
                             new)))
             (setf (textgl--box-label box) trimmed)
             (message "ラベルを '%s' に変更" trimmed)))
          (?2  ;; 行
           (let* ((old (textgl--box-row box))
                  (new (read-number (format "行 [%d]: " old) old)))
             (setf (textgl--box-row box) (max 0 new))
             (textgl--clamp-box-position box)
             (message "行を %d に変更" (textgl--box-row box))))
          (?3  ;; 列
           (let* ((old (textgl--box-col box))
                  (new (read-number (format "列 [%d]: " old) old)))
             (setf (textgl--box-col box) (max 0 new))
             (textgl--clamp-box-position box)
             (message "列を %d に変更" (textgl--box-col box))))
          (?4  ;; 幅
           (let* ((old (textgl--box-width box))
                  (new (read-number (format "幅 [%d]: " old) old)))
             (setf (textgl--box-width box) (max 4 new))
             (message "幅を %d に変更" (textgl--box-width box))))
          (?5  ;; 高さ
           (let* ((old (textgl--box-height box))
                  (new (read-number (format "高さ [%d]: " old) old)))
             (setf (textgl--box-height box) (max 1 new))
             (message "高さを %d に変更" (textgl--box-height box))))
          (?6  ;; z-order
           (let* ((old (textgl--box-z-order box))
                  (new (read-number (format "Z-order [%d]: " old) old)))
             (setf (textgl--box-z-order box) new)
             (message "Z-orderを %d に変更" new))))
        (textgl--redraw)))))

(defun textgl-edit-property-1 () "ラベルを編集。" (interactive) (textgl--edit-inspector-property ?1))
(defun textgl-edit-property-2 () "行を編集。" (interactive) (textgl--edit-inspector-property ?2))
(defun textgl-edit-property-3 () "列を編集。" (interactive) (textgl--edit-inspector-property ?3))
(defun textgl-edit-property-4 () "幅を編集。" (interactive) (textgl--edit-inspector-property ?4))
(defun textgl-edit-property-5 () "高さを編集。" (interactive) (textgl--edit-inspector-property ?5))
(defun textgl-edit-property-6 () "Z-orderを編集。" (interactive) (textgl--edit-inspector-property ?6))

;;;; ============================================================
;;;; undo/redo（スナップショットベース）
;;;; ============================================================

(defun textgl--snapshot ()
  "現在の状態のスナップショットを返す。"
  (list :boxes (mapcar #'copy-textgl--box textgl--boxes)
        :connections (mapcar #'copy-textgl--connection textgl--connections)
        :next-z textgl--next-z))

(defun textgl--restore-snapshot (snap)
  "スナップショットSNAPから状態を復元する。"
  (setq textgl--boxes (plist-get snap :boxes))
  (setq textgl--connections (plist-get snap :connections))
  (setq textgl--next-z (plist-get snap :next-z)))

(defun textgl--push-undo ()
  "現在の状態をundoスタックにプッシュし、redoスタックをクリアする。"
  (push (textgl--snapshot) textgl--undo-stack)
  (setq textgl--redo-stack nil)
  ;; undoスタックの最大サイズ制限（50件）
  (when (> (length textgl--undo-stack) 50)
    (setq textgl--undo-stack (cl-subseq textgl--undo-stack 0 50))))

(defun textgl-undo ()
  "直前の操作を元に戻す。"
  (interactive)
  (if (null textgl--undo-stack)
      (message "元に戻す操作がありません")
    (push (textgl--snapshot) textgl--redo-stack)
    (textgl--restore-snapshot (pop textgl--undo-stack))
    (textgl--redraw)
    (message "元に戻しました")))

(defun textgl-redo ()
  "元に戻した操作をやり直す。"
  (interactive)
  (if (null textgl--redo-stack)
      (message "やり直す操作がありません")
    (push (textgl--snapshot) textgl--undo-stack)
    (textgl--restore-snapshot (pop textgl--redo-stack))
    (textgl--redraw)
    (message "やり直しました")))

;;;; ============================================================
;;;; ファイル保存・読み込み
;;;; ============================================================

(defun textgl--serialize ()
  "現在の図をS-expression形式の文字列に変換する。"
  (let ((data (list
               :version 1
               :canvas-rows textgl--canvas-rows
               :canvas-cols textgl--canvas-cols
               :style (if (eq (plist-get textgl--style :tl) ?┌) 'unicode 'ascii)
               :boxes (mapcar
                       (lambda (b)
                         (list :id (textgl--box-id b)
                               :row (textgl--box-row b)
                               :col (textgl--box-col b)
                               :width (textgl--box-width b)
                               :height (textgl--box-height b)
                               :label (textgl--box-label b)
                               :z-order (textgl--box-z-order b)))
                       textgl--boxes)
               :connections (mapcar
                             (lambda (c)
                               (list :id (textgl--connection-id c)
                                     :from-id (textgl--connection-from-id c)
                                     :to-id (textgl--connection-to-id c)
                                     :from-side (textgl--connection-from-side c)
                                     :to-side (textgl--connection-to-side c)))
                             textgl--connections))))
    (pp-to-string data)))

(defun textgl--deserialize (str)
  "S-expression文字列STRから図データを復元する。"
  (let ((data (car (read-from-string str))))
    ;; バージョンチェック
    (unless (eq (plist-get data :version) 1)
      (error "未対応のファイルバージョン: %s" (plist-get data :version)))
    ;; キャンバス設定
    (setq textgl--canvas-rows (or (plist-get data :canvas-rows) 30))
    (setq textgl--canvas-cols (or (plist-get data :canvas-cols) 80))
    ;; スタイル
    (setq textgl--style (if (eq (plist-get data :style) 'unicode)
                        textgl-style-unicode
                      textgl-style-ascii))
    ;; ボックス復元
    (setq textgl--boxes nil)
    (setq textgl--next-z 0)
    (dolist (bd (plist-get data :boxes))
      (let ((box (textgl--box-create
                  :id (plist-get bd :id)
                  :row (plist-get bd :row)
                  :col (plist-get bd :col)
                  :width (plist-get bd :width)
                  :height (plist-get bd :height)
                  :label (plist-get bd :label)
                  :z-order (plist-get bd :z-order))))
        (push box textgl--boxes)
        (setq textgl--next-z (max textgl--next-z (1+ (textgl--box-z-order box))))))
    ;; 接続線復元
    (setq textgl--connections nil)
    (dolist (cd (plist-get data :connections))
      (push (textgl--connection-create
             :id (plist-get cd :id)
             :from-id (plist-get cd :from-id)
             :to-id (plist-get cd :to-id)
             :from-side (plist-get cd :from-side)
             :to-side (plist-get cd :to-side))
            textgl--connections))
    ;; undo/redoスタックをクリア
    (setq textgl--undo-stack nil)
    (setq textgl--redo-stack nil)))

(defun textgl-save-file (file)
  "図をFILEに保存する。"
  (interactive
   (list (read-file-name "保存先: " nil textgl--file-name nil
                         (or textgl--file-name "diagram.ew"))))
  ;; バッファローカル変数を先に読み取ってからファイルに書き込む
  (let ((content (textgl--serialize))
        (coding-system-for-write 'utf-8))
    (with-temp-file file
      (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n")
      (insert ";; textGL ダイアグラムファイル\n\n")
      (insert content)))
  (setq textgl--file-name file)
  (message "保存しました: %s" file))

(defun textgl-load-file (file)
  "FILEから図を読み込む。"
  (interactive
   (list (read-file-name "読み込み: " nil nil t nil
                         (lambda (f) (or (file-directory-p f)
                                         (string-suffix-p ".ew" f))))))
  (let ((coding-system-for-read 'utf-8))
    (textgl--deserialize (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
  (setq textgl--file-name file)
  (textgl--redraw)
  (message "読み込みました: %s" file))

;;;; ============================================================
;;;; パブリックAPI
;;;; ============================================================

(defun textgl-add-box (id label &optional row col width height)
  "ボックスを追加する。
ID はシンボル、LABEL は文字列。
ROW COL WIDTH HEIGHT は省略可。"
  (let ((box (textgl--box-create
              :id id
              :label label
              :row (or row 1)
              :col (or col 1)
              :width (or width 16)
              :height (or height 3)
              :z-order textgl--next-z)))
    (cl-incf textgl--next-z)
    (push box textgl--boxes)
    (textgl--clamp-box-position box)
    box))

(defun textgl-remove-box (id)
  "ID で指定したボックスを削除する。関連する接続線も削除。"
  (setq textgl--boxes (cl-remove-if (lambda (b) (eq (textgl--box-id b) id))
                                textgl--boxes))
  ;; 関連する接続線も削除
  (setq textgl--connections
        (cl-remove-if (lambda (c)
                        (or (eq (textgl--connection-from-id c) id)
                            (eq (textgl--connection-to-id c) id)))
                      textgl--connections)))

(defun textgl-add-connection (id from-id to-id &optional from-side to-side)
  "接続線を追加する。
ID はシンボル、FROM-ID/TO-ID はボックスID。
FROM-SIDE/TO-SIDE は辺の指定 (top/bottom/left/right)。"
  (let ((conn (textgl--connection-create
               :id id
               :from-id from-id
               :to-id to-id
               :from-side (or from-side 'right)
               :to-side (or to-side 'left))))
    (push conn textgl--connections)
    conn))

(defun textgl-remove-connection (id)
  "ID で指定した接続線を削除する。"
  (setq textgl--connections
        (cl-remove-if (lambda (c) (eq (textgl--connection-id c) id))
                      textgl--connections)))

(defun textgl-set-style (style)
  "描画スタイルを切り替える。STYLE は plist。"
  (setq textgl--style style)
  (textgl--redraw))

;;;; ============================================================
;;;; インタラクティブコマンド
;;;; ============================================================

(defun textgl-add-box-interactive ()
  "対話的にボックスを追加する。カーソル位置に配置。"
  (interactive)
  (let* ((label (read-string "ラベル: "))
         (id    (intern (format "box-%s" (length textgl--boxes))))
         (row   (1- (line-number-at-pos)))
         (col   (current-column)))
    (textgl--push-undo)
    (textgl-add-box id label row col)
    (textgl--redraw)
    (message "ボックス '%s' を追加しました" label)))

(defun textgl-remove-box-interactive ()
  "対話的にボックスを削除する。"
  (interactive)
  (if (null textgl--boxes)
      (message "削除できるボックスがありません")
    (let* ((names (mapcar (lambda (b)
                           (cons (format "%s (%s)" (textgl--box-id b) (textgl--box-label b))
                                 (textgl--box-id b)))
                          textgl--boxes))
           (choice (completing-read "削除するボックス: " names nil t))
           (id     (cdr (assoc choice names))))
      (textgl--push-undo)
      (textgl-remove-box id)
      (textgl--redraw)
      (message "ボックス '%s' を削除しました" id))))

(defun textgl--read-box-id (prompt)
  "PROMPTでボックスIDを対話的に読み取る。"
  (let* ((names (mapcar (lambda (b)
                          (cons (format "%s (%s)" (textgl--box-id b) (textgl--box-label b))
                                (textgl--box-id b)))
                        textgl--boxes))
         (choice (completing-read prompt names nil t)))
    (cdr (assoc choice names))))

(defun textgl--read-side (prompt)
  "PROMPTで辺の方向を対話的に読み取る。"
  (let* ((sides '(("right" . right) ("left" . left)
                  ("top" . top) ("bottom" . bottom)))
         (choice (completing-read prompt sides nil t)))
    (cdr (assoc choice sides))))

(defun textgl-add-connection-interactive ()
  "対話的に接続線を追加する。"
  (interactive)
  (if (< (length textgl--boxes) 2)
      (message "接続線を作成するにはボックスが2つ以上必要です")
    (let* ((from-id   (textgl--read-box-id "接続元ボックス: "))
           (from-side (textgl--read-side "接続元の辺: "))
           (to-id     (textgl--read-box-id "接続先ボックス: "))
           (to-side   (textgl--read-side "接続先の辺: "))
           (id        (intern (format "conn-%s" (length textgl--connections)))))
      (textgl--push-undo)
      (textgl-add-connection id from-id to-id from-side to-side)
      (textgl--redraw)
      (message "接続線 %s → %s を追加しました" from-id to-id))))

(defun textgl-remove-connection-interactive ()
  "対話的に接続線を削除する。"
  (interactive)
  (if (null textgl--connections)
      (message "削除できる接続線がありません")
    (let* ((names (mapcar (lambda (c)
                            (cons (format "%s: %s → %s"
                                          (textgl--connection-id c)
                                          (textgl--connection-from-id c)
                                          (textgl--connection-to-id c))
                                  (textgl--connection-id c)))
                          textgl--connections))
           (choice (completing-read "削除する接続線: " names nil t))
           (id     (cdr (assoc choice names))))
      (textgl--push-undo)
      (textgl-remove-connection id)
      (textgl--redraw)
      (message "接続線 '%s' を削除しました" id))))

(defun textgl-edit-label-interactive ()
  "カーソル位置のボックスのラベルを編集する。"
  (interactive)
  (let* ((row (1- (line-number-at-pos)))
         (col (current-column))
         (box (textgl--box-at row col)))
    (if (null box)
        (message "ここにボックスがありません")
      (let* ((old-label (textgl--box-label box))
             (ntextgl-label (read-string
                         (format "ラベル [%s]: " old-label)
                         nil nil old-label))
             (max-len (textgl--box-inner-physical-width box))
             (trimmed (if (> (string-width ntextgl-label) max-len)
                          (truncate-string-to-width ntextgl-label max-len)
                        ntextgl-label)))
        (textgl--push-undo)
        (setf (textgl--box-label box) trimmed)
        (textgl--redraw)
        (message "ラベルを '%s' に変更しました" trimmed)))))

(defun textgl-edit-label-mouse (event)
  "マウスダブルクリック位置のボックスのラベルを編集する。"
  (interactive "e")
  (let* ((posn (event-start event))
         (rc   (posn-col-row posn))
         (col  (car rc))
         (row  (cdr rc))
         (box  (textgl--box-at row col)))
    (if (null box)
        (message "ここにボックスがありません")
      (let* ((old-label (textgl--box-label box))
             (ntextgl-label (read-string
                         (format "ラベル [%s]: " old-label)
                         nil nil old-label))
             (max-len (textgl--box-inner-physical-width box))
             (trimmed (if (> (string-width ntextgl-label) max-len)
                          (truncate-string-to-width ntextgl-label max-len)
                        ntextgl-label)))
        (textgl--push-undo)
        (setf (textgl--box-label box) trimmed)
        (textgl--redraw)
        (message "ラベルを '%s' に変更しました" trimmed)))))

(defun textgl-toggle-style ()
  "Unicode/ASCII スタイルを切り替える。
スタイル変更後、罫線幅に応じてキャンバスサイズを調整する。"
  (interactive)
  (textgl--push-undo)
  (if (eq (plist-get textgl--style :tl) ?┌)
      (textgl-set-style textgl-style-ascii)
    (textgl-set-style textgl-style-unicode))
  ;; フォント測定キャッシュをリセット
  (setq textgl--actual-border-width nil)
  ;; スタイルに応じてキャンバス列数を調整
  (setq textgl--canvas-cols (if (> (textgl--border-h-width) 1) 160 80))
  (message "スタイル: %s" (if (eq (plist-get textgl--style :tl) ?┌) "Unicode" "ASCII")))

(defun textgl-show-help ()
  "キーバインドのヘルプを表示する。"
  (interactive)
  (message (concat
            "【操作】 "
            "クリック:選択 ドラッグ:パン C-ドラッグ:移動 C-右下ドラッグ:リサイズ | "
            "1-6:プロパティ編集 ESC:選択解除 | "
            "a:追加 d:削除 e:ラベル | "
            "c:接続 x:接続削除 | "
            "u:undo r:redo | "
            "C-x C-s:保存 C-x C-f:読込 | "
            "s:スタイル ?:ヘルプ q:終了")))

;;;; ============================================================
;;;; メジャーモード
;;;; ============================================================

(defvar textgl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; マウス操作
    (define-key map [down-mouse-1] #'textgl--mouse-drag)
    (define-key map [C-down-mouse-1] #'textgl--mouse-drag)
    (define-key map [C-mouse-1] #'ignore)  ;; Buffer Menuポップアップを抑制
    (define-key map [mouse-1] #'textgl--mouse-click)  ;; クリックでインスペクタ表示
    (define-key map [double-mouse-1] #'textgl-edit-label-mouse)
    ;; ボックス操作
    (define-key map (kbd "a") #'textgl-add-box-interactive)
    (define-key map (kbd "d") #'textgl-remove-box-interactive)
    (define-key map (kbd "e") #'textgl-edit-label-interactive)
    ;; 接続線操作
    (define-key map (kbd "c") #'textgl-add-connection-interactive)
    (define-key map (kbd "x") #'textgl-remove-connection-interactive)
    ;; インスペクタ編集 (数字キー)
    (define-key map (kbd "1") #'textgl-edit-property-1)
    (define-key map (kbd "2") #'textgl-edit-property-2)
    (define-key map (kbd "3") #'textgl-edit-property-3)
    (define-key map (kbd "4") #'textgl-edit-property-4)
    (define-key map (kbd "5") #'textgl-edit-property-5)
    (define-key map (kbd "6") #'textgl-edit-property-6)
    ;; 選択解除
    (define-key map (kbd "<escape>") #'textgl-deselect-box)
    (define-key map [escape] #'textgl-deselect-box)
    ;; undo/redo
    (define-key map (kbd "u") #'textgl-undo)
    (define-key map (kbd "r") #'textgl-redo)
    ;; ファイル操作
    (define-key map (kbd "C-x C-s") #'textgl-save-file)
    (define-key map (kbd "C-x C-f") #'textgl-load-file)
    ;; その他
    (define-key map (kbd "s") #'textgl-toggle-style)
    (define-key map (kbd "?") #'textgl-show-help)
    (define-key map (kbd "q") #'quit-window)
    map)
  "textgl-mode のキーマップ。")

(define-derived-mode textgl-mode special-mode "EW"
  "textGL - テキストベースボックス図エディタ。

\\{textgl-mode-map}"
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (setq textgl--style textgl-style-unicode)
  (setq textgl--next-z 0)
  (setq textgl--boxes nil)
  (setq textgl--connections nil)
  (setq textgl--undo-stack nil)
  (setq textgl--redo-stack nil)
  (setq textgl--file-name nil)
  (setq textgl--selected-box nil)  ;; インスペクタ選択状態をリセット
  (setq textgl--inspector-row nil)
  (setq textgl--inspector-col nil)
  (setq textgl--actual-border-width nil)  ;; フォント測定をリセット
  ;; 罫線の実表示幅に応じてキャンバスを拡張
  (setq textgl--canvas-cols (if (> (textgl--border-h-width) 1) 160 80))
  ;; C-down-mouse-1 のグローバルバインド(mouse-buffer-menu)を確実に上書き
  (local-set-key [C-down-mouse-1] #'textgl--mouse-drag)
  (local-set-key [C-mouse-1] #'ignore))

;;;; ============================================================
;;;; エントリポイント
;;;; ============================================================

;;;###autoload
(defun textgl ()
  "テキストベースボックス図エディタを起動する。"
  (interactive)
  (let ((buf (get-buffer-create "*textGL*")))
    (switch-to-buffer buf)
    (textgl-mode)
    ;; デモ用のボックスを配置（colはhwの倍数にスナップされる）
    (textgl-add-box 'box-a "A" 2 2 16 3)
    (textgl-add-box 'box-b "B" 8 50 20 5)
    (textgl-add-box 'box-c "C" 18 10 12 3)
    ;; デモ用の接続線
    (textgl-add-connection 'conn-ab 'box-a 'box-b 'right 'left)
    (textgl-add-connection 'conn-bc 'box-b 'box-c 'bottom 'top)
    (textgl--redraw)
    (message "?:ヘルプ | クリック:選択 C-ドラッグ:移動 1-6:編集 a:追加 d:削除 c:接続 u:undo")))

(provide 'textgl)

;;; textgl.el ends here
