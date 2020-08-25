;;;;; w32-ime.el ---- Meadow features for NTEmacs.
;;
;;   Author H.Miyashita
;;
;;;;;

(defgroup W32-IME nil
  "w32-ime"
  :group 'emacs)

(defvar w32-last-selection nil
  "It is stored the last data from Emacs.")

;----------

(defvar w32-ime-on-hook nil
  "Functions to eval when IME is turned on at least.
Even if IME state is not changed, these functiona are maybe called.")
(defvar w32-ime-off-hook nil
  "Functions to eval when IME is turned off at least.
Even if IME state is not changed, these functiona are maybe called.")
(defvar w32-ime-buffer-switch-p t
  "If this variable is nil, IME control when buffer is switched is disabled.")
(defvar w32-ime-show-mode-line t
  "When t, mode line indicates IME state.")
(defvar w32-ime-mode-line-state-indicator "[O]"
  "This is shown at the mode line. It is regarded as state of ime.")
(make-variable-buffer-local 'w32-ime-mode-line-state-indicator)
(put 'w32-ime-mode-line-state-indicator 'permanent-local t)
(defvar w32-ime-mode-line-state-indicator-list '("-" "[|]" "[O]")
  "List of IME state indicator string.")
(defvar w32-ime-mode-line-format-original nil
  "Original mode line format.")
(defvar w32-ime-input-method-title nil
  "String denoting W32-IME input method.")

;;
;; Section: IME
;;

;; ;; This is temporal solution.  In the future, we will prepare
;; ;; dynamic configuration.
;; (defvar w32-ime-coding-system-language-environment-alist
;;   '(("Japanese" . japanese-shift-jis)
;;     ("Chinese-GB" . chinese-iso-8bit)
;;     ("Chinese-BIG5" . chinese-big5)
;;     ("Korean" . korean-iso-8bit)))

;;
;; IME state indicator
;;
(global-set-key [kanji] 'ignore)
(global-set-key [compend] 'ignore)

(defun wrap-function-to-control-ime
  (function interactive-p interactive-arg &optional suffix)
  "Wrap FUNCTION, and IME control is enabled when FUNCTION is called.
An original function is saved to FUNCTION-SUFFIX when suffix is string.
If SUFFIX is nil, \"-original\" is added. "
  (let ((original-function
	 (intern (concat (symbol-name function)
			 (if suffix suffix "-original")))))
    (cond
     ((not (fboundp original-function))
      (fset original-function
	    (symbol-function function))
      (fset function
	    (list
	     'lambda '(&rest arguments)
	     (when interactive-p
	       (list 'interactive interactive-arg))
	     `(cond
		((and (ime-get-mode)
		      (equal current-input-method "W32-IME"))
 		 (ime-force-off)
		 (unwind-protect
		     (apply ',original-function arguments)
		   (when (and (not (ime-get-mode))
			      (equal current-input-method "W32-IME"))
		     (ime-force-on))))
		(t
		 (apply ',original-function arguments)))))))))

(defvar w32-ime-toroku-region-yomigana nil
  "* if this variable is string, toroku-region regard this value as yomigana.")

(defun w32-ime-toroku-region (begin end)
  (interactive "r")
  (let ((string (buffer-substring begin end))
	(w32-ime-buffer-switch-p nil)
	(reading w32-ime-toroku-region-yomigana))
    (unless (stringp reading)
      (w32-set-ime-mode 'hiragana)
      (setq reading
	    (read-multilingual-string
            (format "Input reading of \"%s\": " string) nil "W32-IME")))
    (w32-ime-register-word-dialog reading string)))

;; for IME management system.

(defun w32-ime-sync-state (window)
  (if w32-ime-buffer-switch-p
      (progn
	(with-current-buffer (window-buffer window)
	  (let* ((frame (window-frame window))
		 (ime-state (ime-get-mode)))
	    (cond
	     ((and (not ime-state)
		   (equal current-input-method "W32-IME"))
	      (ime-force-on nil)
	      (run-hooks 'w32-ime-on-hook))
	     ((and ime-state
		   (not (equal current-input-method "W32-IME")))
	      (ime-force-off nil)
	      (run-hooks 'w32-ime-off-hook))))))
    (progn
      (dolist (win (window-list))
	(with-current-buffer (window-buffer win)
	  (w32-ime-mode-line-update))))
    ))

(defun w32-ime-set-selected-window-buffer-hook (oldbuf newwin newbuf)
  (w32-ime-sync-state newwin))

(defun w32-ime-select-window-hook (old new)
  (w32-ime-sync-state new))

(defun w32-ime-mode-line-update ()
  (if (featurep 'w32-ime)
      (progn
        (cond
         (w32-ime-show-mode-line
          (unless (window-minibuffer-p (selected-window))
            (setq w32-ime-mode-line-state-indicator
                  (nth (if (ime-get-mode) 1 2)
                       w32-ime-mode-line-state-indicator-list))))
         (t
          (setq w32-ime-mode-line-state-indicator
                (nth 0 w32-ime-mode-line-state-indicator-list))))
        (force-mode-line-update))
    ))

(defun w32-ime-init-mode-line-display ()
  (unless (member 'w32-ime-mode-line-state-indicator mode-line-format)
    (setq w32-ime-mode-line-format-original
	  (default-value 'mode-line-format))
    (if (and (stringp (car mode-line-format))
	     (string= (car mode-line-format) "-"))
	(setq-default mode-line-format
		      (cons ""
			    (cons 'w32-ime-mode-line-state-indicator
				  (cdr mode-line-format))))
      (setq-default mode-line-format
		    (cons ""
			  (cons 'w32-ime-mode-line-state-indicator
				mode-line-format))))
    (force-mode-line-update t)))

(defun w32-ime-initialize ()
   (when (and (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	      (eq window-system 'w32)
	      (featurep 'w32-ime))
     (w32-ime-init-mode-line-display)
     (w32-ime-mode-line-update)
     (add-hook 'select-window-functions
	       'w32-ime-select-window-hook)
     (add-hook 'set-selected-window-buffer-functions
	       'w32-ime-set-selected-window-buffer-hook)
     (define-key global-map [kanji] 'toggle-input-method)))
;;     (set-keyboard-coding-system 'utf-8)))

(defun w32-ime-uninitialize ()
  (when (and (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	     (eq window-system 'w32)
	     (featurep 'w32-ime))
    (setq-default mode-line-format
		  w32-ime-mode-line-format-original)
    (force-mode-line-update t)
    (remove-hook 'select-window-functions
		 'w32-ime-select-window-hook)
    (remove-hook 'set-selected-window-buffer-functions
		 'w32-ime-set-selected-window-buffer-hook)
    (define-key global-map [kanji] 'ignore)))

(defun w32-ime-exit-from-minibuffer ()
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'w32-ime-exit-from-minibuffer)))

(defun w32-ime-state-switch (&optional arg)
  (if arg
      (progn
	(setq deactivate-current-input-method-function
	      'w32-ime-state-switch)
	(run-hooks 'input-method-activate-hook)
	(run-hooks 'w32-ime-on-hook)
	(setq describe-current-input-method-function nil)
	(when (eq (selected-window) (minibuffer-window))
	  (add-hook 'minibuffer-exit-hook 'w32-ime-exit-from-minibuffer))
	(ime-force-on)
        (setq current-input-method-title w32-ime-input-method-title))
    (setq current-input-method nil)
    (run-hooks 'input-method-deactivate-hook)
    (run-hooks 'w32-ime-off-hook)
    (setq describe-current-input-method-function nil)
    (ime-force-off)
    (setq current-input-method-title nil))
  (w32-ime-mode-line-update))

(register-input-method "W32-IME" "Japanese" 'w32-ime-state-switch ""
		       "W32 System IME")

(if (symbol-function 'ime-get-mode)
    (provide 'w32-ime))
