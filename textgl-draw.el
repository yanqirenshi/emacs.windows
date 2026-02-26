;;; textgl-draw.el --- textGL 描画モジュール -*- lexical-binding: t; -*-

;;; Commentary:
;; textGL の描画モジュール。
;; ボックス描画、接続線描画、インスペクタ描画、レンダリングを提供。

;;; Code:

(require 'cl-lib)
(require 'textgl-core)

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

(provide 'textgl-draw)
;;; textgl-draw.el ends here
