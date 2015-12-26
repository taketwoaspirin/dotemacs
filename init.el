;; -*-Emacs-Lisp-*-

;; TODO
;; Document external packages needed

;; -- Web certificate checking
;; sudo apt-get install gnutls-bin
;; pip install --user certifi

;; -- jsonlint --
;; sudo apt-get install npm nodejs
;; sudo npm install jsonlint -g
;; edit /usr/local/bin/jsonlint to point at a real nodejs interpreter

;; -- jedi --
;; sudo apt-get install python3-pip
;; pip3 install --user jedi



;; ************************************************************
;; startup speedup tricks
(let ((file-name-handler-alist nil))
(setf gc-cons-threshold 100000000)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))
;;

(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; https certificate handling, from
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

; Package archives

(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(unless (version< emacs-version "24.4") 
  (use-package auto-package-update
    :ensure t
    :init (auto-package-update-maybe)
    )
)
(use-package fixme-mode
  :ensure t
  )
(use-package tramp
  )
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  )
(use-package smart-newline
  :ensure t
  :init (smart-newline-mode 1)
)
(use-package misc
  :bind ("M-z" . zap-up-to-char)
  )
(use-package grep
  :config (grep-compute-defaults)
  )
(use-package dbus
  ; tweak for dbus suggested by http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-06/msg00629.html
  :config (dbus-register-signal
		   :system "org.freedesktop.UPower" "/org/freedesktop/UPower"
		   "org.freedesktop.UPower" "Resuming" 'tramp-cleanup-all-connections)
  )

(use-package ido
  :ensure t
  :init (progn
		  (setq ido-enable-flex-matching t)
		  (ido-mode t)
		  (define-key ido-file-dir-completion-map (kbd "M-C-b") 'ido-goto-bookmark)
		  (defadvice ido-find-file 
			(before auto-refresh-ido nil activate)
			(setq ido-rescan t))
		  )
  )
(use-package ido-ubiquitous
  :ensure t
)
(use-package visual-regexp
  :ensure t
  :bind (
		 ("M-%" . vr/replace)
		 ("M-C-%" . vr/query-replace)
		 )
  )
(use-package visual-regexp-steroids
  :ensure t
  )
(use-package multiple-cursors
  :ensure t
  :bind (
		 ("C-'" . mc/mark-all-like-this-dwim)
         ("M-'" . mc/edit-lines)
         ("<mouse-8>" . mc/add-cursor-on-click)
		 ("H-<mouse-1>" . mc/add-cursor-on-click)
		 )
  )
(use-package expand-region
  :ensure t
  :bind (
		 ("H-SPC" . er/expand-region)
         ("H-S-SPC" . er/contract-region)
         ("H-9" . er/mark-inside-pairs)
         ("H-0" . er/mark-outside-pairs)
         ("H-'" . er/mark-inside-quotes)
         ("H-M-'" . er/mark-outside-quotes)

         )
  )
(use-package rpm-spec-mode
  :ensure t
  :bind (("C-c C-l" . specfile-insert-specfile-changelog-entry)
		 ("C-c C-v" . specfile-goto-version))
  :mode ("specfile.in" . rpm-spec-mode)
  )
(use-package minimap
  :ensure t
  )
(use-package magit
  :ensure t
  :bind ("H-g" . magit-status)
  )
;; (use-package magit-gh-pulls
;;   :ensure t
;;   :config (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;; )
;; (use-package magit-filenotify
;;   :ensure t
;; )
;; (use-package magit-tramp
;;   :ensure t
;; )
;; (use-package magit-simple-keys
;;   :ensure t
;; )
(use-package git-timemachine
  :ensure t
  :bind (
         ("C-H-G" . git-timemachine)
         )
  )
;; (use-package git-lens
;;   :ensure t
;;   )
(use-package gitconfig-mode
  :ensure t
  )
(use-package gitignore-mode
  :ensure t
  )
(use-package ssh-config-mode
  :ensure t
  )
(use-package web-mode
  :ensure t
  )
(use-package whole-line-or-region
  :ensure t
  :init (whole-line-or-region-mode 1)
)
(use-package thingatpt
  :ensure t
  )
(use-package flycheck
  :ensure t
  )
(use-package flycheck-pos-tip
  :ensure t
  )
(use-package pos-tip
  :ensure t
  )
(use-package which-func
  :ensure t
  :config (setq which-func-modes t)
  :init (which-func-mode 1)
  )
(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 200
        ;; https://www.reddit.com/r/emacs/comments/3g468d/stop_recent_files_showing_elpa_packages/
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              ".*\\.gz\\'"
                              "TAGS"
                              ".*-autoloads\\.el\\'"))
  :init (recentf-mode 1)
  )
(use-package ag
  :ensure t
  :bind ("H-r" . ag-project)
)
(use-package wgrep-ag
  :ensure t
  :config (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  )
(use-package json-mode
  :ensure t
)
(use-package flymake-json
  :ensure t
  :init (add-hook 'json-mode-hook 'flymake-json-load)
  :config (define-key json-mode-map "\C-c\C-n" 'flymake-goto-next-error)
)
(use-package ws-trim
  :ensure t
  :init (global-ws-trim-mode 1)
  )
(use-package shrink-whitespace
  :ensure t
  :bind ("M-SPC" . shrink-whitespace)
  )
(use-package multi-line
  :ensure t
  :bind ("H-p" . multi-line)
)
(use-package elpy
  :ensure t
  :init (progn
		  (elpy-enable)
          (global-whitespace-mode 1)
		  )
  :mode (".in" . python-mode)
  :bind (
         ("H-i" . my-jump-to-imports)
		 ("H-d" . my-jump-to-doc-string)
		 ("H-t" . my-python-set-test-id)
         ;; ("H-p" . python-params-to-multiple-lines)
         )
  )
(use-package js3-mode
  :ensure t
  )
(use-package semantic
  :ensure t
  :init (progn
		 (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
		 ;; (global-semanticdb-minor-mode 1)
		 ;; (global-semantic-idle-scheduler-mode 1)
		 (semantic-mode 1)
		 )
  )
(use-package stickyfunc-enhance
  :ensure t
)
(use-package markdown-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
          (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
          )
  )

(use-package string-edit
  :ensure t
  :bind (
         ("H-e" . string-edit-at-point)
         )
  )
(use-package general-close
  :ensure t
  :bind (
         (")" . general-close)
         (";" . general-close)
         )
  )


(setq visible-bell t)
(setq-default truncate-lines t)
(setq comment-auto-fill-only-comments t)
(setq scroll-error-top-bottom 'true)

; miscellaneous behavior tweaks

(auto-compression-mode 1)
(toggle-uniquify-buffer-names)
(transient-mark-mode t)
(blink-cursor-mode -1)
(defvar isearch-initial-string nil)


(set-buffer-file-coding-system 'binary 'utf-8 'utf-8-unix)
(set-default buffer-file-coding-system 'binary)
(set-default-coding-systems 'binary)
(set-default default-buffer-file-coding-system 'binary)

(setq require-final-newline 'ask)
(setq enable-recursive-minibuffers t)

(global-set-key "\M-)" 'match-paren)
(global-set-key "\M-s" 'isearch-forward-at-point)
(global-set-key "\C-c\C-q" 'quote-word-at-point)
(global-set-key "\C-xE" 'apply-macro-to-region-lines)
(global-set-key (kbd "M-n") 'scroll-forward-one-line)
(global-set-key (kbd "M-p") 'scroll-backward-one-line)
(global-set-key (kbd "C-j") 'my-jump-to-char)
(global-set-key (kbd "C-o") 'my-smart-open-line)
(global-set-key (kbd "C-x r v") 'list-registers)
(global-set-key (kbd "C-c r") 'my-revert-buffer-without-asking) 
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)
(global-set-key (kbd "M-]") 'next-error)
(global-set-key (kbd "H-b") 'load-makefile)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "C-;") 'my-comment-line-dwim)
(global-set-key (kbd "C-:") 'my-comment-line-dwim-next-line)
(global-set-key (kbd "C-(") 'delete-pair)

(global-set-key (kbd "(") 'my-magic-parentheses)
(global-set-key (kbd ")") 'my-magic-parentheses)
(global-set-key (kbd "[") 'my-magic-bracket)
(global-set-key (kbd "]") 'my-magic-bracket)
(global-set-key (kbd "{") 'my-magic-braces)
(global-set-key (kbd "}") 'my-magic-braces)
(global-set-key (kbd "\"") 'my-magic-double-quote)
(global-set-key (kbd "'") 'my-magic-quote)

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key (kbd "H-u") 'my-underline-line)
(windmove-default-keybindings 'hyper)
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
(global-set-key [remap move-end-of-line]
                'smarter-move-end-of-line)
(global-set-key (kbd "<C-return>") 'open-file-at-cursor)

(global-set-key (kbd "C-=") 'increment-integer-at-point)
(global-set-key (kbd "C--") 'decrement-integer-at-point)
(global-set-key (kbd "H-f") 'recentf-open-files)


(defun specfile-insert-specfile-changelog-entry ()
  (interactive)
  (goto-char 1)
  (search-forward "Version:")
  (skip-chars-forward " ")
  (let ((p (point)))
	(move-end-of-line 1)
	(setq version (buffer-substring-no-properties p (point))))

  (goto-char 1)
  (search-forward "Release:")
  (skip-chars-forward " ")
  (let ((p (point)))
	(move-end-of-line 1)
	(setq release (buffer-substring-no-properties p (point))))

  (setq versionstr (format "  [%s-%s]" version release))
  (rpm-goto-section "changelog")
  (forward-line 1)
  (setq newentrylocation (point))
  (forward-line 1)
  (if (looking-at (format "  \\[%s-%s\\]" version release))
	  (progn 
		(while (not (looking-at "\\s-*\n"))
		  (forward-line 1))
		(indent-for-tab-command)
		(save-excursion
		  (let ((p (point)))
			(end-of-line 1)
			(delete-region p (point))))
		)
	(goto-char newentrylocation)
	(insert "* ")
	(insert (format-time-string "%a %b %d %Y"))
	(insert " Stephen Ryan <sryan@renesys.com>\n")
	(insert versionstr)
	(insert "\n  \n\n")
	(backward-char 2)
	)
)

(defun specfile-goto-version ()
  (interactive)
  (goto-char 1)
  (search-forward "Version:")
  (move-end-of-line 1)
)

(defun match-paren (arg)
  "Go to the matching paren if on a paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
		((looking-at "\\s\)") (forward-char 1) (backward-list 1))
		)
  )

(defun bracket-region-with-parens (start end leftchar rightchar)
  "Put parenthetical characters around the current region."
  (save-excursion
	(goto-char start)
	(insert leftchar)
	(goto-char end)(forward-char 1)
	(insert rightchar)
	)
)

(defun insert-char-maybe-around-region (left right)
  (if (not (region-active-p))
      (self-insert-command 1)
    (bracket-region-with-parens (region-beginning) (region-end) left right)
    )
  )

(defun my-magic-quote ()
  (interactive)
  (insert-char-maybe-around-region "'" "'")
)

(defun my-magic-double-quote ()
  (interactive)
  (insert-char-maybe-around-region "\"" "\"")
)


(defun my-magic-parentheses ()
  "Put parentheses around the current region."
  (interactive)
  (insert-char-maybe-around-region  "(" ")")
)

(defun my-magic-braces ()
  "Put braces around the current region."
  (interactive)
  (insert-char-maybe-around-region "{" "}")
)

(defun my-magic-bracket ()
  "Put braces around the current region."
  (interactive)
  (insert-char-maybe-around-region "[" "]")
)


(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
	(let* ((end (progn (skip-syntax-forward "w_") (point)))
		   (begin (progn (skip-syntax-backward "w_") (point))))
	  (if (eq begin end)
		  (isearch-forward regexp-p no-recursive-edit)
		(setq isearch-initial-string (buffer-substring begin end))
		(add-hook 'isearch-mode-hook 'isearch-set-initial-string)
		(isearch-forward regexp-p no-recursive-edit)))))

(defun quote-word-at-point ()
  "Quote word at point"
  (interactive)
  (save-excursion
	(if (region-active-p)
		(bracket-region-with-parens (region-beginning) (region-end) "\"" "\"")
	  (forward-word)
	  (insert-char ?" 1)
	  (backward-word)
	  (insert-char ?" 1)
	  ))
  )

(defun scroll-forward-one-line ()
  "Scroll forward one line"
  (interactive)
  (scroll-up 1)
  (next-line)
)

(defun scroll-backward-one-line ()
  "Scroll backward one line"
  (interactive)
  (scroll-down 1)
  (previous-line)
)


(defun my-menu-bar-mode ()
  "Toggle menu bar"
  (interactive)
  (let ((pixelchange -22))
	(if (menu-bar-mode)
		(setq pixelchange 22)
	  )
	(setq mf-display-padding-height (+ mf-display-padding-height pixelchange))
	(maximize-frame)
	)
  )


; from http://emacswiki.org/emacs/InteractivelyDoThings
(defun ido-goto-bookmark (bookmark)
  (interactive
   (list (bookmark-completing-read "Jump to bookmark"
								   bookmark-current-bookmark)))
  (unless bookmark
	(error "No bookmark specified"))
  (let ((filename (bookmark-get-filename bookmark)))
	(ido-set-current-directory
	 (if (file-directory-p filename)
		 filename
	   (file-name-directory filename)))
	(setq ido-exit        'refresh
		  ido-text-init   ido-text
		  ido-rotate-temp t)
	(exit-minibuffer)))


(defun my-kill-buffers-matching-filenames (pattern)
  (interactive "sKill buffers matching:")
  (mapcar (lambda (buf)
			(if (buffer-file-name buf)
				(if (string-match pattern (buffer-file-name buf))
					(kill-buffer buf))))
		  (buffer-list))
)

(defun my-kill-buffers-matching (pattern)
  (interactive "sKill buffers matching:")
  (mapcar (lambda (buf)
			(if (buffer-name buf)
				(message (buffer-file-name buf))
				(if (string-match pattern (buffer-name buf))
					(kill-buffer buf))))
		  (buffer-list))
)

(defun my-jump-to-char (arg char)
  "Jump to next occurrence of CHAR."
  (interactive "P\ncJump to char: ")
  (if (equal arg nil)
	  (progn
		(forward-char)
		(if (search-forward (char-to-string char) nil t)
			(backward-char)
		  )
		)
	(search-backward (char-to-string char) nil t)
	)
)

(defun my-revert-buffer-without-asking ()
  "Revert buffer to saved version without asking for confirmation"
  (interactive)
  (revert-buffer 't 't)
)

(defun sudo-edit-current-file ()
  (interactive)
  (let ((pos (point)))
	(find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
	(goto-char pos)))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my-jump-to-imports (&optional arg)
  (interactive "P")
  (push-mark)
  (goto-char 1)
  (if (search-forward "import " nil t)
	  (progn (move-beginning-of-line 1)
			 (while (or (looking-at "import ") (looking-at "from .* import"))
			   (move-beginning-of-line 2))
			 (forward-line -1)
			 (move-end-of-line 1)
			 (if arg (insert "\nimport ")))
			 
	(forward-line)
	(while (not (looking-at "\n"))
	  (forward-line))
	(insert "\nimport "))
  )

(defun my-jump-to-doc-string (&optional arg)
  (interactive "P")
  (forward-line 1)
  (if arg
	  (goto-char 1)
	(beginning-of-defun)
	(search-forward ":"))
  (forward-line 1)
  (back-to-indentation)
  (if (looking-at "\"\"\"")
      (progn
        (forward-char 4)
        (back-to-indentation)
        )
    (indent-for-tab-command)
    (insert "\"\"\"\n\n")
    (indent-for-tab-command)
    (insert "\"\"\"\n")
	(indent-for-tab-command)
    (forward-line -2)
	(back-to-indentation)
    (indent-for-tab-command)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun my-comment-line-dwim (&optional arg)
  (interactive)
  (if (region-active-p)
	  (comment-or-uncomment-region (region-beginning) (region-end))

	(save-excursion
	  (move-beginning-of-line 1)
	  (if (looking-at "\s*\n")
		  (progn
			(comment-dwim arg)
			(message "comment-dwim"))
		(message "comment-or-uncomment line")
		(comment-or-uncomment-region (line-beginning-position) (line-end-position))))
	(if (comment-only-p (point) (line-end-position))
		(move-end-of-line 1))
	)
)

(defun my-comment-line-dwim-next-line (&optional arg)
  (interactive)
  (my-comment-line-dwim arg)
  (forward-line 1)
)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun my-underline-line ()
  (interactive)
  (if (and (bolp) (eolp))
	  (forward-line -1)
	(beginning-of-line 1))

  (if (or (looking-at "^-+$")
		  (looking-at "^$"))
	  (forward-line -1))
  (let ((a (point)))
	(end-of-line 1)
	(let ((b (point)))
	  (forward-char 1)
	  (if (looking-at "^-+$")
		  (kill-line 1))
	  (dotimes (i (- b a))
		(insert "-"))
	  (insert "\n"))))


;; From http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; From https://bitbucket.org/Fuco/.emacs.d/src/tip/files/defuns-edit.el?at=default#cl-161
(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((face (plist-get (text-properties-at (point)) 'face)))
    (when (not (listp face)) (setq face (list face)))
    (or (memq 'font-lock-comment-face face)
        (memq 'font-lock-comment-delimiter-face face))))

;; From https://bitbucket.org/Fuco/.emacs.d/src/tip/files/defuns-edit.el?at=default#cl-161
(defun smarter-move-end-of-line (&optional arg)
  "Move to the end of code.  If already there, move to the end of line,
that is after the possible comment.  If at the end of line, move
to the end of code.

Example:
  (serious |code here)1 ;; useless comment2

In the example, | is the current point, 1 is the position of
point after one invocation of this function, 2 is position after
repeated invocation. On subsequent calls the point jumps between
1 and 2.

Comments are recognized in any mode that sets syntax-ppss
properly."
  (interactive "p")
  (cl-flet ((end-of-line-lov () (if visual-line-mode
                                 (end-of-visual-line arg)
                               (move-end-of-line arg)))
         (beg-of-line-lov () (if visual-line-mode
                                 (beginning-of-visual-line arg)
                               (move-beginning-of-line arg))))
    (let ((eoc (save-excursion
                 (end-of-line-lov)
                 (while (and (point-in-comment)
                             (not (bolp)))
                   (backward-char))
                 (skip-syntax-backward " ")
                 ;; if we skipped all the way to the beginning, that
                 ;; means there's only comment on this line, so this
                 ;; should just jump to the end.
                 (if (= (point) (save-excursion
                                  (beg-of-line-lov)
                                  (point)))
                     (progn (end-of-line-lov)
                            (point))
                   (point)))))
      (if (= (point) eoc)
          (end-of-line-lov)
        (goto-char eoc)))))

(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path is starts with 'http:', open the URL in browser.
Input path can be {relative, full path, URL}.
This command is similar to `find-file-at-point' but without prompting for confirmation.
"
  (interactive)
  (let ( (path (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'filename) ) ))
    (if (string-match-p "\\`https?://" path)
        (browse-url path)
      (progn ; not starting 'http:'
        (if (file-exists-p path)
            (find-file path)
          (if (file-exists-p (concat path ".el"))
              (find-file (concat path ".el"))
            (when (y-or-n-p (format "file doesn't exist: '%s'. Create?" path) )
              (find-file path )) ) ) ) ) ))



;; From http://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/

(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))


(defun insert-python-current-defun ()
  ""
  (interactive)
  (open-line 1)
  (indent-for-tab-command)
  (insert "self.err_test_id = \"Failure: " (car (last (split-string (python-info-current-defun) "\\."))) "\"")
)

(defun my-python-set-test-id ()
  ""
  (interactive)
  (save-excursion
	(save-restriction
	  (narrow-to-defun)
      (let ((current-class (first (split-string (python-info-current-defun) "\\."))))
      (my-jump-to-doc-string)
      (forward-char 1)
      (back-to-indentation)
      (if (looking-at current-class)
          (kill-whole-line))
      (indent-for-tab-command)
      (insert (python-info-current-defun) "\n")
      (indent-for-tab-command)

      (goto-char 1)
      (if (search-forward "self.err_test_id = " nil t)
		  (progn
			(kill-whole-line)
			(insert-python-current-defun)
			)
		(forward-line)
		(if (looking-at "\\s-*\"\"\"")
			(progn 
			  (forward-line 1)
			  (search-forward "\"\"\"" nil t)
			  (forward-line 1)
			  )
		  )
		(insert-python-current-defun)
	))))
)

(put 'narrow-to-region 'disabled nil)

(defun my-smart-open-line (n)
  ""
  (interactive "p")
  (if (looking-back "^\s*")
      (progn
        (beginning-of-line)
        (open-line n)
        (indent-for-tab-command))
    (open-line n)
    )
  )

(defun python-params-to-multiple-lines ()
  ""
  (interactive)
  (save-excursion
    (python-nav-end-of-statement)
    (let ((end (point)))
      (python-nav-beginning-of-statement)
      (let ((beg (point)))
        (replace-string ", " ",\n" nil beg end)
        (python-nav-beginning-of-statement)
        (search-forward "(")
        (insert "\n")
        (python-nav-end-of-statement)
        (backward-char 1)
        (insert "\n")
        ))
    (python-nav-end-of-statement)
    (let ((end (point)))
      (python-nav-beginning-of-statement)
      (let ((beg (point)))
        (indent-region beg end)
        ))))

;; End let from the top
)

;************************************************************
; Custom, below the let since Custom doesn't know how to deal with that.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-list-file-prefix "~/.emacs.d-local/auto-save-list/.saves-")
 '(c-basic-offset 4)
 '(c-tab-always-indent t)
 '(case-fold-search t)
 '(cc-search-directories (quote ("." "/usr/include" "/usr/local/include/*" "../*")))
 '(clean-buffer-list-delay-general 7)
 '(cperl-continued-statement-offset 0)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(elpy-rpc-python-command "python3")
 '(elpy-test-nose-runner-command (quote ("nosetests3")))
 '(elpy-test-runner (quote elpy-test-nose-runner))
 '(global-font-lock-mode t nil (font-lock))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-max-directory-size 90000)
 '(ido-ubiquitous-mode t)
 '(indent-tabs-mode nil)
 '(jde-jdk-registry (quote (("1.6.0" . "/usr/lib/jvm/java-6-sun"))))
 '(mf-display-padding-height 75)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-mode t nil (mwheel))
 '(package-archives (quote (("marmalade" . "https://marmalade-repo.org/packages/") ("elpy" . "https://jorgenschaefer.github.io/packages/") ("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.milkbox.net/packages/"))))
 '(python-fill-docstring-style (quote django))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-ghost))
 '(semantic-idle-scheduler-idle-time 0.25)
 '(semanticdb-default-save-directory "~/.emacs-semanticdb")
 '(sgml-basic-offset 4)
 '(show-smartparens-global-mode t)
 '(sp-show-pair-from-inside t)
 '(tab-width 4)
 '(tls-checktrust t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(whitespace-global-modes (quote (makefile-gmake-mode\ python-mode)))
 '(whitespace-style (quote (face tabs space-before-tab)))
 '(window-combination-resize t)
 '(window-numbering-auto-assign-0-to-minibuffer t)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "Inconsolata")))))
