;;; textgl-io.el --- textGL 入出力モジュール -*- lexical-binding: t; -*-

;;; Commentary:
;; textGL の永続化とundo/redoモジュール。
;; スナップショットベースのundo/redo、シリアライズ、ファイル保存・読み込みを提供。

;;; Code:

(require 'cl-lib)
(require 'textgl-core)
(require 'textgl-draw)

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

(provide 'textgl-io)
;;; textgl-io.el ends here
