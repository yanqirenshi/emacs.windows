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
(require 'textgl-core)
(require 'textgl-draw)
(require 'textgl-ui)
(require 'textgl-io)

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
