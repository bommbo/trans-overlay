;;; trans-overlay-list.el --- Tabulated list for translations -*- lexical-binding: t -*-

(require 'trans-overlay)
(require 'cl-lib)
(require 'pulse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Edit Session Tracking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar trans-overlay-edit-original-buffer nil
  "Original list buffer for edit session.")
(defvar trans-overlay-edit-file nil
  "File being edited.")
(defvar trans-overlay-edit-pos nil
  "Position of translation.")
(defvar trans-overlay-edit-end-pos nil
  "End position of translation.")
(defvar trans-overlay-edit-source nil
  "Source text of translation.")
(defvar trans-overlay-edit-type nil
  "Type of translation (word/paragraph).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Edit Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode trans-overlay-edit-mode text-mode "Trans-Edit"
  "Mode for editing translations."
  (local-set-key (kbd "C-c C-c") #'trans-overlay-edit-save)
  (local-set-key (kbd "C-c C-k") #'trans-overlay-edit-cancel))

(defun trans-overlay-edit-save ()
  "Save edited translation without changing pos/end_pos/source."
  (interactive)
  (let ((new-translation (string-trim (buffer-string))))
	(if (string-empty-p new-translation)
		(message "⚠️ Translation cannot be empty")
	  (let ((file trans-overlay-edit-file)
			(pos trans-overlay-edit-pos)
			(end-pos trans-overlay-edit-end-pos)
			(source trans-overlay-edit-source)
			(type trans-overlay-edit-type)
			(edit-buf (current-buffer)))
		;; Delete old record
		(trans-overlay-delete file pos)
		;; Insert new record (keep pos, end_pos, source unchanged)
		(trans-overlay-add file pos end-pos source new-translation type)
		(message "✅ Translation updated")
		;; Refresh overlay in file buffer and all its windows
		(let ((buf (get-file-buffer file)))
		  (when buf
			(with-current-buffer buf
			  (trans-overlay-display-file))
			;; Force redisplay in every window showing this buffer
			(dolist (win (get-buffer-window-list buf nil t))
			  (with-selected-window win
				(redisplay t)))))
		;; Clean up edit buffer
		(when (buffer-live-p edit-buf)
		  (kill-buffer edit-buf))
		;; Return to list
		(when (buffer-live-p trans-overlay-edit-original-buffer)
		  (let ((win (get-buffer-window trans-overlay-edit-original-buffer 0)))
			(if win
				(select-window win)
			  (pop-to-buffer trans-overlay-edit-original-buffer))
			(trans-overlay-list-refresh)))
		;; Reset state
		(setq trans-overlay-edit-original-buffer nil
			  trans-overlay-edit-file nil
			  trans-overlay-edit-pos nil
			  trans-overlay-edit-end-pos nil
			  trans-overlay-edit-source nil
			  trans-overlay-edit-type nil)))))

(defun trans-overlay-edit-cancel ()
  "Cancel editing and return to list."
  (interactive)
  (when (get-buffer "*Trans Edit*")
	(kill-buffer "*Trans Edit*"))
  (when (buffer-live-p trans-overlay-edit-original-buffer)
	(let ((win (get-buffer-window trans-overlay-edit-original-buffer 0)))
	  (if win
		  (select-window win)
		(pop-to-buffer trans-overlay-edit-original-buffer))))
  (setq trans-overlay-edit-original-buffer nil
		trans-overlay-edit-file nil
		trans-overlay-edit-pos nil
		trans-overlay-edit-end-pos nil
		trans-overlay-edit-source nil
		trans-overlay-edit-type nil)
  (message "Cancelled"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; List Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar trans-overlay-list-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map tabulated-list-mode-map)
	(define-key map (kbd "RET") #'trans-overlay-list-jump)
	(define-key map "e" #'trans-overlay-list-edit)
	(define-key map "E" #'trans-overlay-list-export)
	(define-key map "d" #'trans-overlay-list-delete)
	(define-key map "D" #'trans-overlay-list-delete-marked)
	(define-key map "g" #'trans-overlay-list-refresh)
	(define-key map "/" #'trans-overlay-list-filter)
	(define-key map "m" #'trans-overlay-list-toggle-mark)
	(define-key map "M" #'trans-overlay-list-mark-all)
	(define-key map "U" #'trans-overlay-list-unmark-all)
	(define-key map "o" #'trans-overlay-list-other-window)
	map)
  "Keymap for `trans-overlay-list-mode'.")

(defvar-local trans-overlay-list--filter-term nil
  "Current filter term.")

(defun trans-overlay-list--build-entries (&optional filter-term show-marked-only)
  "Build entries for translation list."
  (let ((entries '())
		(all-translations (trans-overlay-get-all)))
	(dolist (trans all-translations)
	  (let* ((file (nth 0 trans))
			 (pos (nth 1 trans))
			 (src (nth 3 trans))
			 (translation (nth 4 trans))
			 (type (nth 5 trans))
			 (marked (nth 6 trans))
			 (show? (and (or (null filter-term)
							(string-match-p (regexp-quote filter-term)
										  (concat src " " translation)))
						(or (not show-marked-only) (= marked 1)))))
		(when show?
		  (push (list (cons file pos)
					  (vector (if (= marked 1) "⭐" "☐")
							  (concat (if (string= type "word") "📝 " "📄 ") type)
							  (file-name-nondirectory file)
							  (truncate-string-to-width src 40 nil nil "...")
							  (truncate-string-to-width translation 50 nil nil "...")))
				entries))))
	(nreverse entries)))

(defun trans-overlay-list--get-id ()
  "Get current entry ID or error."
  (or (tabulated-list-get-id) (user-error "No entry on this line")))

(defun trans-overlay-list-jump ()
  "Jump to translation location and close list window."
  (interactive)
  (let* ((id (trans-overlay-list--get-id))
		 (win (selected-window)))
	(find-file (car id))
	(goto-char (cdr id))
	(pulse-momentary-highlight-one-line)
	(delete-window win)))

(defun trans-overlay-list-other-window ()
  "Open translation location in other window."
  (interactive)
  (let* ((id (trans-overlay-list--get-id)))
	(let ((buf (or (get-file-buffer (car id))
				   (find-file-noselect (car id)))))
	  (save-selected-window
		(pop-to-buffer buf 'other-window)
		(goto-char (cdr id))
		(recenter)
		(pulse-momentary-highlight-one-line)))))

(defun trans-overlay-list-edit ()
  "Edit current translation."
  (interactive)
  (let* ((id (trans-overlay-list--get-id))
		 (file (car id))
		 (pos (cdr id))
		 (trans-data (trans-overlay-get file pos)))
	(when trans-data
	  (setq trans-overlay-edit-original-buffer (current-buffer))
	  (setq trans-overlay-edit-file file)
	  (setq trans-overlay-edit-pos pos)
	  (setq trans-overlay-edit-end-pos (nth 1 trans-data))
	  (setq trans-overlay-edit-source (nth 2 trans-data))
	  (setq trans-overlay-edit-type (nth 4 trans-data))
	  (let ((edit-buf (get-buffer-create "*Trans Edit*")))
		(pop-to-buffer edit-buf)
		(erase-buffer)
		(insert (nth 3 trans-data))
		(trans-overlay-edit-mode)
		(message "📝 Edit Translation | C-c C-c save | C-c C-k cancel")))))

(defun trans-overlay-list-toggle-mark ()
  "Toggle mark for current translation."
  (interactive)
  (let* ((id (trans-overlay-list--get-id))
		 (line (line-number-at-pos)))
	(trans-overlay-toggle-mark (car id) (cdr id))
	(trans-overlay-list-refresh)
	;; Move to next line
	(goto-char (point-min))
	(forward-line (1- line))
	(unless (eobp)
	  (forward-line 1))))

(defun trans-overlay-list-mark-all ()
  "Mark all visible translations."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (when-let ((id (tabulated-list-get-id)))
		(trans-overlay-toggle-mark (car id) (cdr id)))
	  (forward-line 1)))
  (trans-overlay-list-refresh)
  (message "✅ Marked all visible"))

(defun trans-overlay-list-unmark-all ()
  "Unmark all translations in database."
  (interactive)
  (when (yes-or-no-p "Unmark ALL translations? ")
	(emacsql (trans-overlay--db)
			 [:update translations :set (= marked 0)])
	(trans-overlay-list-refresh)
	(message "✅ Unmarked all")))

(defun trans-overlay-list-delete ()
  "Delete current translation."
  (interactive)
  (let* ((id (trans-overlay-list--get-id))
		 (line (line-number-at-pos)))
	(when (y-or-n-p "Delete this translation? ")
	  (trans-overlay-delete (car id) (cdr id))
	  ;; Clear overlay in file buffer
	  (let ((buf (get-file-buffer (car id))))
		(when buf
		  (with-current-buffer buf
			(dolist (ov (overlays-in (point-min) (point-max)))
			  (when (and (overlay-get ov 'trans-overlay)
						(= (overlay-start ov) (cdr id)))
				(delete-overlay ov))))))
	  ;; Refresh list and restore position
	  (trans-overlay-list-refresh)
	  (goto-char (point-min))
	  (forward-line (1- (min line (count-lines (point-min) (point-max)))))
	  (message "✅ Deleted"))))

(defun trans-overlay-list-delete-marked ()
  "Delete all marked translations."
  (interactive)
  (let ((marked (trans-overlay-get-marked)))
	(if (null marked)
		(message "No marked translations to delete.")
	  (when (y-or-n-p (format "Delete %d marked translation(s)? " (length marked)))
		;; Clear overlays in buffers
		(dolist (item marked)
		  (let ((buf (get-file-buffer (nth 0 item))))
			(when buf
			  (with-current-buffer buf
				(dolist (ov (overlays-in (point-min) (point-max)))
				  (when (and (overlay-get ov 'trans-overlay)
							(= (overlay-start ov) (nth 1 item)))
					(delete-overlay ov)))))))
		;; Delete from database
		(trans-overlay-delete-all-marked)
		(trans-overlay-list-refresh)
		(message "✅ Deleted %d marked translation(s)" (length marked))))))

(defun trans-overlay-list-export ()
  "Export marked translations to file."
  (interactive)
  (call-interactively #'trans-overlay-export-wordlist))

(defun trans-overlay-list-refresh ()
  "Refresh the list, maintaining cursor position."
  (interactive)
  (let* ((id (tabulated-list-get-id))
		 (line (line-number-at-pos))
		 (mode (eq major-mode 'trans-overlay-wordlist-mode)))
	(setq tabulated-list-entries
		  (trans-overlay-list--build-entries trans-overlay-list--filter-term mode))
	(tabulated-list-print t)
	(when id
	  (goto-char (point-min))
	  (let ((found nil))
		(while (and (not (eobp)) (not found))
		  (if (equal (tabulated-list-get-id) id)
			  (setq found t)
			(forward-line 1)))
		(unless found
		  (goto-char (point-min))
		  (forward-line (1- (max 1 (min line (count-lines (point-min) (point-max)))))))))))

(defun trans-overlay-list-filter (term)
  "Filter translations by TERM."
  (interactive "sFilter: ")
  (setq trans-overlay-list--filter-term (unless (string-empty-p term) term))
  (trans-overlay-list-refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Major Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode trans-overlay-list-mode tabulated-list-mode "Trans-List"
  "List all translations."
  (setq tabulated-list-format
		[("Mark" 4 nil)
		 ("Type" 10 t)
		 ("File" 20 t)
		 ("Source" 40 t)
		 ("Translation" 0 t)])
  (tabulated-list-init-header)
  (hl-line-mode 1))

(define-derived-mode trans-overlay-wordlist-mode trans-overlay-list-mode "Trans-Wordlist"
  "List marked translations only."
  (setq tabulated-list-format
		[("Type" 10 t)
		 ("File" 20 t)
		 ("Source" 40 t)
		 ("Translation" 0 t)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun trans-overlay-list ()
  "List all translations in tabulated format."
  (interactive)
  (trans-overlay-init-db)
  (let ((buf (get-buffer-create "*Trans Overlay List*")))
	(with-current-buffer buf
	  (trans-overlay-list-mode)
	  (trans-overlay-list-refresh))
	(pop-to-buffer buf)
	(message "e=edit E=export m=mark M=all U=unmark d=del D=del-marked o=open-other")))

;;;###autoload
(defun trans-overlay-wordlist ()
  "List marked translations (wordlist)."
  (interactive)
  (trans-overlay-init-db)
  (let ((marked (trans-overlay-get-marked))
		(buf (get-buffer-create "*Trans Wordlist*")))
	(if (null marked)
		(message "No marked translations. Use 'm' to mark in list.")
	  (with-current-buffer buf
		(trans-overlay-wordlist-mode)
		(setq tabulated-list-entries (trans-overlay-list--build-entries nil t))
		(tabulated-list-print t)
		(goto-char (point-min)))
	  (pop-to-buffer buf)
	  (message "RET=jump e=edit E=export d=delete D=del-all o=open-other"))))

(provide 'trans-overlay-list)
;;; trans-overlay-list.el ends here
