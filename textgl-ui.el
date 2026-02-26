;;; textgl-ui.el --- textGL UIモジュール -*- lexical-binding: t; -*-

;;; Commentary:
;; textGL のユーザー操作・インタラクションモジュール。
;; ボックス判定、Z-order管理、マウスドラッグ、インスペクタ操作を提供。

;;; Code:

(require 'cl-lib)
(require 'textgl-core)
(require 'textgl-draw)

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

(declare-function textgl--push-undo "textgl-io")

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

(provide 'textgl-ui)
;;; textgl-ui.el ends here
