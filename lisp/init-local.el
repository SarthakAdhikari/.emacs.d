;;; init-local.el --- custom configs added to purcell's config
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Set xah-fly-keys bindings
;;----------------------------------------------------------------------------
(require 'xah-fly-keys)
(xah-fly-keys-set-layout 'qwerty) ; required
(xah-fly-keys 1)
(global-set-key [remap xah-fly-M-x] 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ivy)
(require 'ivy-avy)
(define-key ivy-minibuffer-map (kbd "M-i") 'ivy-beginning-of-buffer)
(define-key ivy-minibuffer-map (kbd "M-k") 'ivy-end-of-buffer)

;;----------------------------------------------------------------------------
;; Add Ace Windows Config
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-s-u") 'ace-swap-window)
(setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill column indicator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-display-fill-column-indicator-mode -1)
(toggle-frame-fullscreen)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sarthak/toggle-file-name ()
  "Toggle the entire file name in modeline."
  (interactive)
  (setq doom-modeline-buffer-file-name-style (if (eq doom-modeline-buffer-file-name-style 'auto)
                                                 'truncate-except-project
                                               'auto
                                               ))

  )
(global-set-key (kbd "C-s-f") 'sarthak/toggle-file-name)
(global-set-key (kbd "C-s-m") 'hide-mode-line-mode)

(doom-modeline-mode)
(set-face-attribute 'default nil :height 180)

(add-hook 'text-mode-hook '(lambda ()  (global-display-line-numbers-mode -1)))
(add-hook 'prog-mode-hook '(lambda ()  (global-display-line-numbers-mode -1)))
(add-hook 'python-mode-hook (lambda () (setq display-line-numbers nil)))

;;----------------------------------------------------------------------------
;; Multiple Cursors
;;----------------------------------------------------------------------------
(require 'multiple-cursors)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sarthak/surround-with (char)
  "Surronds a selected region with char."
  (interactive "cSurround with: ")
  (progn
    (save-excursion
      (let (
            (end(+ 1 (region-end)))
            )
        (goto-char (region-beginning))
        (message "%s" (region-beginning))
        (insert char)
        (goto-char end)
        (insert char)
        (deactivate-mark)

        )
      ))
  )

(global-set-key (kbd "C-c s") 'sarthak/surround-with)

;;----------------------------------------------------------------------------
;; Custom Shortcuts for Easier Navigation
;;----------------------------------------------------------------------------
(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))
(define-key xah-fly-command-map (kbd "b") #'insert-line-above)

(defun my/move-to-middle-line ()
  (interactive)
  (progn (goto-line (/ ( count-lines (point-min) (point-max)) 2) ) )
  )

(global-set-key (kbd "C-c n") 'my/move-to-middle-line)

(defun my/move-to-middle ()
  (interactive)
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (middle (/ (+ end begin) 2)))
    (goto-char middle)))

(global-set-key (kbd "C-c b") 'my/move-to-middle)

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

(defun arrayify-new-line (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ",\n")))
    (delete-region start end)
    (insert insertion)))

;;----------------------------------------------------------------------------
;; Org mode custom
;;----------------------------------------------------------------------------
;; (define-key org-mode-map (kbd "C-c C-x C-l") 'lsp-org)
(defvar org-blocks-hidden nil)

(defun org-in-tangle-dir (sub-path)
  "Expand the SUB-PATH into the directory given by the tangle-dir
property if that property exists, else use the
`default-directory'."
  (expand-file-name sub-path
                    (or
                     (org-entry-get (point) "tangle-dir" 'inherit)
                     (default-directory))))

(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

(add-hook 'org-mode-hook 'org-toggle-blocks)

(require 'org)
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)
(define-key org-mode-map (kbd "C-c C-x m") 'lsp-org)
;; (eval-after-load "org-mode" '(progn

;;                                (define-key org-mode-map (kbd "C-c t") 'org-toggle-blocks)
;;                                ))
(add-hook 'text-mode-hook (lambda ()
                            (interactive)
                            (if (eq major-mode 'Info-mode)
                                (progn (
                                        (message "Olivetti text-mode-hook")
                                        (olivetti-set-width 80)
                                        (text-scale-increase 4)
                                        (hide-mode-line-mode)
                                        (olivetti-mode 1)
                                        ))
                              ()
                              )
                            ))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages '((ruby . t)))

;; (org-babel-do-load-languages
;; 'org-babel-load-languages '((shell . t)))

(setq org-confirm-babel-evaluate nil)


;;----------------------------------------------------------------------------
;; Minibuffer
;;----------------------------------------------------------------------------
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(define-key xah-fly-leader-key-map (kbd "z") 'switch-to-minibuffer)

;;----------------------------------------------------------------------------
;; Org custom functions
;;----------------------------------------------------------------------------
(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(setq  org-edit-src-content-indentation 0)

;;----------------------------------------------------------------------------
;; Workarounds
;;----------------------------------------------------------------------------

;; Projectile file-missing "Setting current directory" "No such file or directory" #1302 workaround
(setq projectile-git-submodule-command nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree mode                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;turn on everywhere
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key xah-fly-leader-key-map (kbd "l d") 'shell-pop)
(add-hook 'vterm-mode-hook (lambda () (text-scale-decrease 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windmove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-i") 'windmove-up)
(global-set-key (kbd "C-S-k") 'windmove-down)
(global-set-key (kbd "C-S-l") 'windmove-right)
(global-set-key (kbd "C-S-j") 'windmove-left)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Truncate lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x w") 'toggle-truncate-lines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Winner config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-M-y") 'winner-undo)
(global-set-key (kbd "C-M-g") 'winner-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  set up unicode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smerge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq smerge-command-prefix (kbd "C-c v"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "s-l")
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Scale Workaround
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company-posframe)
(company-posframe-mode 1)
(setq company-posframe-quickhelp-delay nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elpy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elpy-snippet-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
            (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun elpy-snippet-current-method-and-args ()
  "Return information on the current definition."
  (let ((current-defun (python-info-current-defun))
        (current-arglist
         (save-excursion
           (python-nav-beginning-of-defun)
           (when (re-search-forward "(" nil t)
             (let* ((start (point))
                    (end (progn
                           (forward-char -1)
                           (forward-sexp)
                           (- (point) 1))))
               (elpy-snippet-split-args
                (buffer-substring-no-properties start end))))))
        class method args)
    (unless current-arglist
      (setq current-arglist '(("self"))))
    (if (and current-defun
             (string-match "^\\(.*\\)\\.\\(.*\\)$" current-defun))
        (setq class (match-string 1 current-defun)
              method (match-string 2 current-defun))
      (setq class "Class"
            method "method"))
    (setq args (mapcar #'car current-arglist))
    (list class method args)))

(defun elpy-snippet-init-assignments (arg-string)
  "Return the typical __init__ assignments for arguments."
  (let ((indentation (make-string (save-excursion
                                    (goto-char start-point)
                                    (current-indentation))
                                  ?\s)))
    (mapconcat (lambda (arg)
                 (if (string-match "^\\*" (car arg))
                     ""
                   (format "self.%s = %s\n%s"
                           (car arg)
                           (car arg)
                           indentation)))
               (elpy-snippet-split-args arg-string)
               "")))

(defun elpy-snippet-super-form ()
  "Return (Class, first-arg).method if Py2.
Else return ().method for Py3."
  (let* ((defun-info (elpy-snippet-current-method-and-args))
         (class (nth 0 defun-info))
         (method (nth 1 defun-info))
         (args (nth 2 defun-info))
         (first-arg (nth 0 args))
         (py-version-command " -c 'import sys ; print(sys.version_info.major)'")
         ;; Get the python version. Either 2 or 3
         (py-version-num (substring (shell-command-to-string (concat elpy-rpc-python-command py-version-command))0 1)))
    (if (string-match py-version-num "2")
        (format "(%s, %s).%s" class first-arg method)
      (format "().%s" method))))

(defun elpy-snippet-super-arguments ()
  "Return the argument list for the current method."
  (mapconcat (lambda (x) x)
             (cdr (nth 2 (elpy-snippet-current-method-and-args)))
             ", "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show-trailing-whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-trailing-whitespace ()
  "Doc-string for `my-switch` function."
  (interactive)
  (setq show-trailing-whitespace (if (eq show-trailing-whitespace nil) 1 nil)))
(define-key xah-fly-leader-key-map (kbd "l m") 'toggle-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whichkey
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq which-key-idle-delay 0.3)
(setq which-key-idle-secondary-delay 0.05)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq lsp-ui-doc-enable nil)
(setq lsp-eldoc-hook nil)
(global-set-key (kbd "C-c C-m") 'lsp-ui-doc-glance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0.05)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yasnippet)
(yas-global-mode 1)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"                 ;; personal snippets
;;         ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
;;         ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
;;         ))
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))
(global-set-key (kbd "C-S-x") 'yas/describe-tables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'treemacs)
(add-hook 'treemacs-select-hook 'xah-fly-insert-mode-activate)
(global-set-key (kbd "C-S-a") 'treemacs)
(global-set-key (kbd "C-S-d") 'treemacs-select-window)
(define-key treemacs-mode-map (kbd "C-f") 'treemacs-find-file)
(setq treemacs-show-cursor 1)
;; (define-key treemacs-mode-map [remap isearch-forward] #'treemacs-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'emms-setup)
(emms-all)
(emms-default-players)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook (lambda () (text-scale-decrease 2)))
;; (add-to-list 'dired-compress-files-alist '("\\.info\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o"))
;; (add-hook 'dired-mode-hook 'auto-revert-mode)
;; (add-hook 'dired-mode-hook 'dired-async-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(blink-cursor-mode 0)
(setq Info-hide-note-references 'hide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'vterm)
(define-key vterm-mode-map [remap xah-backward-kill-word] #'vterm-send-C-w)
(define-key vterm-mode-map [remap backward-kill-word] #'vterm-send-C-w)
(define-key vterm-mode-map [remap xah-kill-word] #'vterm-send-M-d)
(define-key vterm-mode-map [remap kill-line] #'vterm-send-M-d)
(define-key vterm-mode-map [remap backward-word] #'vterm-send-M-b)
(define-key vterm-mode-map [remap forward-word] #'vterm-send-M-f)
(define-key vterm-mode-map [remap forward-char] #'vterm-send-C-f)
(define-key vterm-mode-map [remap backward-char] #'vterm-send-C-b)
(define-key vterm-mode-map [remap xah-delete-backward-char-or-bracket-text] #'vterm-send-backspace)
(define-key vterm-mode-map [remap xah-paste-or-paste-previous] #'vterm-yank)
(define-key vterm-mode-map (kbd "C-c C-v") #'vterm-copy-mode)
(define-key vterm-mode-map (kbd "C-l") nil)
(define-key vterm-mode-map (kbd "C-;") #'vterm-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Olivetti mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/olivetti-python-mode ()
  "Set olivetti-body-widht to 80 for python mode"
  (interactive)
  ;; (setq olivetti-body-width (if (eq major-mode 'python-mode) 85 80))
  (setq olivetti-body-width 80)
  )
(add-hook 'olivetti-mode-hook 'my/olivetti-python-mode)

(global-set-key (kbd "C-c m o") 'olivetti-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace/Layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eyebrowse-keymap-prefix (kbd "s-i"))
(eyebrowse-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-latex-toc-command "\\tableofcontents \\clearpage")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files t)
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import path from shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exec-path-from-shell-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Garbage collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq gc-cons-threshold #x40000000)
;; (defvar k-gc-timer
;;   (run-with-idle-timer 60 t
;;                        (lambda ()
;;                          (message "Garbage Collector has run for %.06fsec"
;;                                   (k-time (garbage-collect))))))

(setq garbage-collection-messages t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remap default keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key key-translation-map (kbd "<menu>") (kbd "C-g"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/python-navigate-up-to-class-statement ()
  (interactive)
  (let ((pos nil))
    (while (not (equal pos (point)))
      (setf pos (point))
      (python-nav-backward-up-list))))

(defun my/python-navigate-to-next-python-class ()
  (interactive)
  (my/python-navigate-up-to-class-statement)
  (end-of-defun)
  (end-of-defun)
  (my/python-navigate-up-to-class-statement))

(defun my/python-navigate-to-previous-python-class ()
  (interactive)
  (my/python-navigate-up-to-class-statement)
  (beginning-of-defun))

(add-hook 'python-mode-hook '(lambda () (progn
                                          (define-key python-mode-map (kbd "M-k" ) 'my/python-navigate-to-next-python-class)
                                          (define-key python-mode-map (kbd "C-S-k") 'my/python-navigate-to-previous-python-class)

                                          ))
          )
;; (eval-after-load "python-mode" '(progn
;; ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speedread
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'speedread)
(provide 'init-local)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bm)

(setq bm-cycle-all-buffers t)
(setq bm-repository-file "~/.emacs.d/bm-repository")
(setq-default bm-buffer-persistence t)
(add-hook 'after-init-hook 'bm-repository-load)
(add-hook 'kill-buffer-hook #'bm-buffer-save)
(add-hook 'kill-emacs-hook #'(lambda nil
                               (bm-buffer-save-all)
                               (bm-repository-save)))
(add-hook 'find-file-hooks   #'bm-buffer-restore)
(add-hook 'after-revert-hook #'bm-buffer-restore)
(add-hook 'after-save-hook #'bm-buffer-save)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<C-f3>") 'bm-remove-all-current-buffer)
(global-set-key (kbd "C-s-b") 'bm-show-all)


;;; init-local.el ends here
