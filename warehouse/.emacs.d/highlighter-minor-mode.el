(defun regex-escape (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (replace-string "\\" "\\\\")
    (goto-char (point-min))
    (replace-string "]" "\\\]")
    (goto-char (point-min))
    (replace-string "[" "\\\[")
    (goto-char (point-min))
    (replace-string "+" "\\\+")
    (goto-char (point-min))
    (replace-string "*" "\\\*")
    (goto-char (point-min))
    (replace-string "?" "\\\?")
    (goto-char (point-min))
    (replace-string "." "\\\.")
    (buffer-substring (point-min) (point-max))))

(defun hl-mode-include-pattern (pattern) "Include only lines that match the pattern" 
  (interactive "sPattern: ")
  (message "[hightlighter-minor-mode] Including " pattern)
  (save-excursion
    (beginning-of-buffer)
    (keep-lines pattern)))

(defun hl-mode-exclude-pattern (pattern) "Exclude lines that match the pattern" 
  (interactive "sPattern: ")
  (message "[hightlighter-minor-mode] Excluding " pattern)
  (save-excursion
    (beginning-of-buffer)
    (flush-lines pattern)))

(defun hl-mode-include-string (string)
  (interactive "sString: ")
  (hl-mode-include-pattern (regex-escape string)))

(defun hl-mode-exclude-string (string)
  (interactive "sString: ")
  (hl-mode-exclude-pattern (regex-escape string)))

(defun hl-mode-include-selection () "Include only lines that contain the current selection"
  (interactive)
  (unless (and (point) (mark))
    (error "Some text must be selected to include it."))
  (hl-mode-include-string (buffer-substring (point) (mark))))

(defun hl-mode-exclude-selection () "Include only lines that contain the current selection"
  (interactive)
  (unless (and (point) (mark))
    (error "Some text must be selected to exclude it."))
  (hl-mode-exclude-string (buffer-substring (point) (mark))))

(defun hl-mode-include-word () "Include only lines that contain the current word"
  (interactive)
  (unless (current-word)
    (error "Cursor must be over some word to include it")
    (hl-mode-include-string (current-word))))

(defun hl-mode-exclude-word () "Exclude only lines that contain the current word"
  (interactive)
  (unless (current-word)
    (error "Cursor must be over some word to exclude it")
    (hl-mode-exclude-string (current-word))))

(defvar hl-mode-highlights '())
(defun hl-mode-highlight-pattern (pattern) "Highlight the pattern"
  (setq hl-mode-highlights (cons pattern hl-mode-highlights))
  (highlight-regexp pattern))

(defun hl-mode-highlight-selection () "Highlight the current selection"
  (interactive)
  (unless (and (point) (mark))
    (error "Some text must be selected to highlight it"))
  (hl-mode-highlight-pattern (regex-escape (buffer-substring (point) (mark)))))

(defun hl-mode-highlight-word () "Highlight the current word"
  (interactive)
  (unless (current-word)
    (error "Cursor must be over some word to highlight it"))
  (hl-mode-highlight-pattern (regex-escape (current-word))))

(defun hl-mode-highlight-clear () "Undo all highlights"
  (interactive)
  (while hl-mode-highlights
    (unhighlight-regexp (car hl-mode-highlights))
    (setq hl-mode-highlights (cdr hl-mode-highlights))))

(defvar hl-mode-tmp-file nil)
(defun hl-mode-start () 
  (save-excursion
    (setq hl-mode-tmp-file (make-temp-file "hlmode"))
    (when (file-writable-p hl-mode-tmp-file)
      (write-region (point-min)
                    (point-max)
                    hl-mode-tmp-file))))

(defun hl-mode-stop ()
  (hl-mode-highlight-clear)
  (when hl-mode-tmp-file
    (progn 
      (delete-region (point-min)
                     (point-max))
      (insert-file-contents hl-mode-tmp-file)
      (goto-char (point-min)))))

(define-minor-mode highlighter-minor-mode "Its like Mandiant Highlighter in Emacs"
  :lighter " Highlighter" 
  :keymap (let ((map (make-keymap)))
            (define-key map "\C-h\C-l\C-i\C-p" 'hl-mode-include-pattern)
            (define-key map "\C-h\C-l\C-i\C-t" 'hl-mode-include-string)
            (define-key map "\C-h\C-l\C-i\C-w" 'hl-mode-include-word)
            (define-key map "\C-h\C-l\C-i\C-s" 'hl-mode-include-selection)
            (define-key map "\C-h\C-l\C-x\C-p" 'hl-mode-exclude-pattern)
            (define-key map "\C-h\C-l\C-x\C-t" 'hl-mode-exclude-string)
            (define-key map "\C-h\C-l\C-x\C-w" 'hl-mode-exclude-word)
            (define-key map "\C-h\C-l\C-x\C-s" 'hl-mode-exclude-selection)
            (define-key map "\C-h\C-l\C-h\C-p" 'hl-mode-highlight-pattern)
            (define-key map "\C-h\C-l\C-h\C-w" 'hl-mode-highlight-word)
            (define-key map "\C-h\C-l\C-h\C-s" 'hl-mode-highlight-selection)
            (define-key map "\C-h\C-l\C-c"     'hl-mode-highlight-clear)
            map)
  (if highlighter-minor-mode
      (hl-mode-start)
    (hl-mode-stop)))

(provide 'highlighter-minor-mode)
