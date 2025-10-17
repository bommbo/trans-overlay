;;; trans-overlay.el --- On-the-fly translation with overlay and database -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite-builtin)
(require 'ov)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup trans-overlay nil
  "On-the-fly translation with overlay."
  :group 'applications)

(defcustom trans-overlay-shell-command "trans"
  "Command for translate-shell executable."
  :type 'string
  :group 'trans-overlay)

(defcustom trans-overlay-source-lang "en"
  "Source language code."
  :type 'string
  :group 'trans-overlay)

(defcustom trans-overlay-target-lang "zh-CN"
  "Target language code."
  :type 'string
  :group 'trans-overlay)

(defcustom trans-overlay-word-face 'highlight
  "Face for translated word overlays."
  :type 'face
  :group 'trans-overlay)

(defcustom trans-overlay-paragraph-face 'secondary-selection
  "Face for translated paragraph overlays."
  :type 'face
  :group 'trans-overlay)

(defcustom trans-overlay-db-file
  (expand-file-name "trans-overlay.db"
					(expand-file-name "trans-overlay" user-emacs-directory))
  "Database file for storing translations."
  :type 'file
  :group 'trans-overlay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Private Database ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar trans-overlay--db-connection nil)

(defun trans-overlay--db ()
  "Return a live emacsql connection."
  (unless (and trans-overlay--db-connection
			   (emacsql-live-p trans-overlay--db-connection))
	(make-directory (file-name-directory trans-overlay-db-file) t)
	(setq trans-overlay--db-connection
		  (emacsql-sqlite-builtin trans-overlay-db-file)))
  trans-overlay--db-connection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Database API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trans-overlay-init-db ()
  "Initialize translation tables."
  (let ((db (trans-overlay--db)))
	(emacsql db
			 [:create-table :if-not-exists translations
			  ([(id integer :primary-key :autoincrement)
				(file text :not-null)
				(pos integer :not-null)
				(end_pos integer :not-null)
				(source_text text :not-null)
				(translation text :not-null)
				(trans_type text :not-null)
				(marked integer :default 0)
				(created_at text)]
			   (:unique [file pos]))])))

(defun trans-overlay-add (file pos end-pos source translation type)
  "Add translation record."
  (emacsql (trans-overlay--db)
		   [:insert-or-replace :into translations
			:values [nil $s1 $s2 $s3 $s4 $s5 $s6 0 (funcall current-timestamp)]]
		   file pos end-pos source translation type))

(defun trans-overlay-get (file pos)
  "Get translation at FILE POS."
  (car (emacsql (trans-overlay--db)
				[:select [pos end_pos source_text translation trans_type marked]
				 :from translations
				 :where (and (= file $s1) (= pos $s2))]
				file pos)))

(defun trans-overlay-get-file (file)
  "Get all translations for FILE."
  (emacsql (trans-overlay--db)
		   [:select [pos end_pos source_text translation trans_type marked]
			:from translations
			:where (= file $s1)
			:order-by pos]
		   file))

(defun trans-overlay-delete (file pos)
  "Delete translation at FILE POS."
  (emacsql (trans-overlay--db)
		   [:delete :from translations
			:where (and (= file $s1) (= pos $s2))]
		   file pos))

(defun trans-overlay-delete-all-marked ()
  "Delete all marked translations."
  (emacsql (trans-overlay--db)
		   [:delete :from translations :where (= marked 1)]))

(defun trans-overlay-toggle-mark (file pos)
  "Toggle export mark."
  (let ((current (trans-overlay-get file pos)))
	(when current
	  (let ((marked (nth 5 current)))
		(emacsql (trans-overlay--db)
				 [:update translations
				  :set (= marked $s1)
				  :where (and (= file $s2) (= pos $s3))]
				 (if (zerop marked) 1 0)
				 file pos)
		(zerop marked)))))

(defun trans-overlay-get-all ()
  "Get all translations."
  (emacsql (trans-overlay--db)
		   [:select [file pos end_pos source_text translation trans_type marked]
			:from translations
			:order-by (file pos)]))

(defun trans-overlay-get-marked ()
  "Get all marked translations."
  (emacsql (trans-overlay--db)
		   [:select [file pos source_text translation trans_type]
			:from translations
			:where (= marked 1)
			:order-by (file pos)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Position Synchronization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trans-overlay-update-position-db (file old-pos new-pos new-end-pos)
  "Update position in database when overlay moves."
  (when (and file old-pos new-pos)
	(emacsql (trans-overlay--db)
			 [:update translations
			  :set [(= pos $s1) (= end_pos $s2)]  ; ← 移除 updated_at
			  :where (and (= file $s3) (= pos $s4))]
			 new-pos new-end-pos file old-pos)))

(defun trans-overlay-sync-overlays-to-db (&optional file)
  "Sync current overlay positions back to database."
  (let ((target-file (or file (buffer-file-name))))
	(when target-file
	  (let ((synced-count 0))
		(dolist (ov (overlays-in (point-min) (point-max)))
		  (when (overlay-get ov 'trans-overlay-original-pos)
			(let ((old-pos (overlay-get ov 'trans-overlay-original-pos))
				  (new-pos (overlay-start ov))
				  (new-end-pos (overlay-end ov)))
			  (unless (and (= old-pos new-pos)
						   (= (overlay-get ov 'trans-overlay-original-end-pos) new-end-pos))
				(trans-overlay-update-position-db target-file old-pos new-pos new-end-pos)
				(overlay-put ov 'trans-overlay-original-pos new-pos)
				(overlay-put ov 'trans-overlay-original-end-pos new-end-pos)
				(cl-incf synced-count)))))
		(when (> synced-count 0)
		  (message "✓ Synced %d translation%s position%s to database"
				   synced-count
				   (if (= synced-count 1) "" "s")
				   (if (= synced-count 1) "" "s")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Translation Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trans-overlay--call-trans (text)
  "Call translate-shell to translate TEXT."
  (condition-case err
	  (let* ((trans-cmd (or (executable-find trans-overlay-shell-command)
							trans-overlay-shell-command))
			 (clean-text (replace-regexp-in-string "'" "'\"'\"'" text))
			 (cmd (format "%s -brief :%s '%s'"
						  trans-cmd
						  trans-overlay-target-lang
						  clean-text))
			 (result (string-trim (shell-command-to-string cmd))))
		(cond
		 ((string-empty-p result) nil)
		 ((string-match-p "\\[ERROR\\]\\|command not found\\|not found" result) nil)
		 (t result)))
	(error nil)))

(defun trans-overlay--get-text-at-point ()
  "Get text at point or region."
  (cond
   ((use-region-p)
	(let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
	  (unless (string-empty-p (string-trim text))
		(list (string-trim text) (region-beginning) (region-end) 'paragraph))))
   ((thing-at-point 'word)
	(let ((bounds (bounds-of-thing-at-point 'word)))
	  (list (thing-at-point 'word t) (car bounds) (cdr bounds) 'word)))
   (t
	(let ((para (thing-at-point 'paragraph)))
	  (when para
		(let ((bounds (bounds-of-thing-at-point 'paragraph)))
		  (list (string-trim para) (car bounds) (cdr bounds) 'paragraph)))))))

(defun trans-overlay--create-overlay (start end translation type)
  "Create overlay using ov with position tracking."
  (let* ((display-text (if (eq type 'word)
						   (format " (%s)" translation)
						 (format "\n──────────\n%s\n──────────" translation)))
		 (ov (ov start end
				 'trans-overlay t
				 'trans-overlay-type type
				 'after-string (propertize display-text
										   'face (if (eq type 'word)
													 trans-overlay-word-face
												   trans-overlay-paragraph-face))
				 'evaporate t)))
	;; Store original positions for sync
	(overlay-put ov 'trans-overlay-original-pos start)
	(overlay-put ov 'trans-overlay-original-end-pos end)
	ov))

(defun trans-overlay--clear-overlays-at-point ()
  "Clear translation overlays at point."
  (dolist (ov (overlays-in (point) (1+ (point))))
	(when (overlay-get ov 'trans-overlay)
	  (delete-overlay ov))))

(defun trans-overlay--clear-all-overlays ()
  "Clear all translation overlays in buffer using delete-overlay."
  (dolist (ov (overlays-in (point-min) (point-max)))
	(when (overlay-get ov 'trans-overlay)
	  (delete-overlay ov))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun trans-overlay-at-point ()
  "Translate word/paragraph at point."
  (interactive)
  (let ((file (buffer-file-name)))
	(unless file (user-error "Buffer must be visiting a file"))
	(trans-overlay-init-db)
	(let* ((text-info (trans-overlay--get-text-at-point))
		   (text (nth 0 text-info))
		   (start (nth 1 text-info))
		   (end (nth 2 text-info))
		   (type (nth 3 text-info)))
	  (unless text (user-error "No text found at point"))
	  (let ((translation (trans-overlay--call-trans text)))
		(if (not translation)
			(message "❌ Translation failed - check 'trans' command")
		  (trans-overlay-add file start end text translation (symbol-name type))
		  (trans-overlay--create-overlay start end translation type)
		  (message "✅ Translated: %s" translation))))))

;;;###autoload
(defun trans-overlay-clear-at-point ()
  "Clear translation at point."
  (interactive)
  (let ((file (buffer-file-name)))
	(unless file (user-error "Buffer must be visiting a file"))
	(trans-overlay--clear-overlays-at-point)
	(let ((trans (trans-overlay-get file (point))))
	  (when trans
		(trans-overlay-delete file (point))
		(message "✅ Translation cleared")))))

;;;###autoload
;;;###autoload
(defun trans-overlay-display-file ()
  "Display all translations from database using ov."
  (interactive)
  (let ((file (buffer-file-name)))
	(unless file (user-error "Buffer must be visiting a file"))
	(trans-overlay-init-db)
	(trans-overlay--clear-all-overlays)
	(let ((translations (trans-overlay-get-file file))
		  (count 0))
	  (dolist (trans translations)
		(let* ((pos (nth 0 trans))
			   (end-pos (nth 1 trans))
			   (translation (nth 3 trans))
			   (type (intern (nth 4 trans))))
		  (when (and (>= pos (point-min)) (<= end-pos (point-max)))
			(trans-overlay--create-overlay pos end-pos translation type)
			(cl-incf count))))
	  (message "✅ Displayed %d translation%s" count (if (= count 1) "" "s"))
	  ;; 🔑 Force immediate redisplay
	  (redisplay t))))

;;;###autoload
(defun trans-overlay-export-wordlist (file-path)
  "Export marked translations to FILE-PATH."
  (interactive "FExport wordlist to: ")
  (when (file-directory-p file-path)
	(user-error "Output path is a directory. Specify a file name."))
  (trans-overlay-init-db)
  (let ((marked (trans-overlay-get-marked)))
	(if (null marked)
		(if (y-or-n-p "No marked translations. Export ALL? ")
			(setq marked (emacsql (trans-overlay--db)
								  [:select [file pos source_text translation trans_type]
								   :from translations
								   :order-by (file pos)]))
		  (user-error "Export cancelled")))
	(with-temp-buffer
	  (insert "═══════════════════════════════════════════════════\n")
	  (insert "  Trans Overlay Wordlist\n")
	  (insert (format "  Exported: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
	  (insert (format "  Total: %d items\n" (length marked)))
	  (insert "═══════════════════════════════════════════════════\n\n")
	  (let ((current-file nil)
			(word-count 0)
			(para-count 0))
		(dolist (item marked)
		  (let* ((file (nth 0 item))
				 (source (nth 2 item))
				 (translation (nth 3 item))
				 (type (nth 4 item)))
			(unless (equal file current-file)
			  (when current-file (insert "\n"))
			  (insert (format "━━━ %s ━━━\n\n" (file-name-nondirectory file)))
			  (setq current-file file))
			(if (string= type "word")
				(progn
				  (insert (format "  • %s\n" source))
				  (insert (format "    → %s\n\n" translation))
				  (cl-incf word-count))
			  (insert (format "  ┌─ %s\n" (truncate-string-to-width source 60 nil nil "...")))
			  (insert (format "  └→ %s\n\n" translation))
			  (cl-incf para-count))))
		(insert "\n═══════════════════════════════════════════════════\n")
		(insert (format "  Words: %d | Paragraphs: %d\n" word-count para-count))
		(insert "═══════════════════════════════════════════════════\n")
		(write-region (point-min) (point-max) file-path nil :silent))
	  (message "✅ Exported %d items to %s" (length marked) file-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quick Translation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun trans-overlay-quick-translate ()
  "Translate word/region and show result in minibuffer (no save)."
  (interactive)
  (let* ((text-info (trans-overlay--get-text-at-point))
		 (text (nth 0 text-info)))
	(if (not text)
		(progn
		  (message "⚠️ No text found at point")
		  (ding))
	  (let ((translation (trans-overlay--call-trans text)))
		(if translation
			(progn
			  (message "🔤 %s → %s" text translation)
			  (sit-for 3))
		  (progn
			(message "❌ Translation failed: '%s'" text)
			(ding)))))))

;;;###autoload
(defun trans-overlay-quick-translate-popup ()
  "Translate and show in popup buffer with cursor at translation start."
  (interactive)
  (let* ((text-info (trans-overlay--get-text-at-point))
		 (text (nth 0 text-info)))
	(if (not text)
		(message "⚠️ No text found at point")
	  (let ((translation (trans-overlay--call-trans text)))
		(if (not translation)
			(message "❌ Translation failed for: '%s'" text)
		  (let ((buf (get-buffer-create "*Translation*")))
			(with-current-buffer buf
			  (let ((inhibit-read-only t))
				(erase-buffer)
				(insert "═══════════════════════════════════════\n")
				(insert "🔤 Source:\n")
				(insert "  " text "\n\n")
				(insert "🌐 Translation:\n")
				(insert "  ")
				(let ((trans-start (point)))
				  (insert translation "\n")
				  (insert "═══════════════════════════════════════\n")
				  (goto-char trans-start)))
			  (use-local-map
			   (let ((map (make-sparse-keymap)))
				 (set-keymap-parent map special-mode-map)
				 (define-key map "q" #'quit-window)
				 map))
			  (setq buffer-read-only t))
			(pop-to-buffer buf)
			(message "✅ Press 'q' to close")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Sync on Save ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun trans-overlay--sync-on-save ()
  "Sync translation positions to DB after saving a file."
  (when (buffer-file-name)
	(trans-overlay-sync-overlays-to-db)))

(add-hook 'after-save-hook #'trans-overlay--sync-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; add translate  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun trans-overlay-add-translation ()
  "Add or edit translation manually (without translate-shell)."
  (interactive)
  (let ((file (buffer-file-name)))
	(unless file (user-error "Buffer must be visiting a file"))
	(trans-overlay-init-db)
	(let* ((text-info (trans-overlay--get-text-at-point))
		   (source-text (nth 0 text-info))
		   (start (nth 1 text-info))
		   (end (nth 2 text-info))
		   (type (nth 3 text-info)))
	  (unless source-text (user-error "No text found at point"))

	  ;; Check if translation already exists
	  (let ((existing (trans-overlay-get file start)))
		(let* ((default-translation (when existing (nth 3 existing)))
			   (translation (read-string
							 (format "🔤 Translate \"%s\" to %s: "
									 (truncate-string-to-width source-text 30 nil nil "...")
									 trans-overlay-target-lang)
							 default-translation)))
		  (unless (string-empty-p (string-trim translation))
			(trans-overlay-add file start end source-text
							   (string-trim translation) (symbol-name type))
			(trans-overlay--create-overlay start end (string-trim translation) type)
			(message "✅ Manual translation added: %s"
					 (truncate-string-to-width translation 50 nil nil "..."))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Minor Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar trans-overlay-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c t t") #'trans-overlay-at-point)
	(define-key map (kbd "C-c t c") #'trans-overlay-clear-at-point)
	(define-key map (kbd "C-c t d") #'trans-overlay-display-file)
	(define-key map (kbd "C-c t l") #'trans-overlay-list)
	(define-key map (kbd "C-c t q") #'trans-overlay-quick-translate)
	(define-key map (kbd "C-c t p") #'trans-overlay-quick-translate-popup)
	map)
  "Keymap for Trans Overlay mode.")

(autoload 'trans-overlay-list "trans-overlay-list" "List all translations." t)
(autoload 'trans-overlay-wordlist "trans-overlay-list" "List marked translations." t)

;;;###autoload
(define-minor-mode trans-overlay-mode
  "Minor mode for on-the-fly translation."
  :lighter " 🌐"
  :keymap trans-overlay-mode-map
  (if trans-overlay-mode
	  (progn
		(trans-overlay-init-db)
		(trans-overlay-display-file)
		(message "✅ Trans Overlay enabled | C-c t l for list"))
	(trans-overlay--clear-all-overlays)
	(message "✅ Trans Overlay disabled")))

(provide 'trans-overlay)
;;; trans-overlay.el ends here
