; Fullscreen
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . fullscreen)))))
;; Text scaling
(set-face-attribute 'default nil :height 180)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; Browser

(setenv "BROWSER" "/mnt/c/Users/olles/AppData/Local/qutebrowser/qutebrowser.exe")
(with-eval-after-load 'browse-url
  (setq browse-url-generic-program (getenv "BROWSER"))
  (setq browse-url-browser-function 'browse-url-generic)
  )

;; WSL Copy/Paste Solution found from stackexchange:
;; https://emacs.stackexchange.com/questions/39210/copy-paste-from-windows-clipboard-in-wsl-terminal
(defun wsl-copy (start end)
  ;; (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

; wsl-paste
(defun wsl-paste ()
  (interactive)
  (let ((clipboard
     (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
    (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
    (insert clipboard)))

;; use general for keybindings
(straight-use-package 'general)


;; we create this early in the file, it will be used for many states further down
(general-create-definer ergo-def
  :states '(normal insert emacs visual)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  )

(ergo-def
  "a" 'execute-extended-command
  "ö" 'save-buffer
  "o" 'other-window
  "f" 'switch-to-buffer
  "k" 'kill-this-buffer
  )

;; ergo-def-c-infix
(ergo-def
  :infix "c"
  "l" (lambda ()
	(interactive)
	(org-time-stamp-inactive '(16))
	(org-clock-update-time-maybe))
  )

(straight-use-package 'eyebrowse)
(eyebrowse-mode)
;; eyebrowse workspaces on SPC-r-"x"
(ergo-def
  :infix "r"
  "0" 'eyebrowse-switch-to-window-config-0
  "1" 'eyebrowse-switch-to-window-config-1
  "2" 'eyebrowse-switch-to-window-config-2
  "3" 'eyebrowse-switch-to-window-config-3
  "4" 'eyebrowse-switch-to-window-config-4
  "5" 'eyebrowse-switch-to-window-config-5
  "6" 'eyebrowse-switch-to-window-config-6
  "6" 'eyebrowse-switch-to-window-config-6
  "7" 'eyebrowse-switch-to-window-config-7
  "8" 'eyebrowse-switch-to-window-config-8
  "9" 'eyebrowse-switch-to-window-config-9)

;; Bookmarks
(ergo-def
  :infix "i"
  "o" #'bookmark-jump
  "p" #'bookmark-set
  "f" #'find-file
  )


;; Swiper
(straight-use-package 'swiper)
(ergo-def
  "l" #'swiper
  )



;; Straight package manager
;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" crafted-config-path))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Modules
(require 'crafted-defaults)
(require 'crafted-ui)

;; Don't use evil-collection for some modes
(with-eval-after-load 'evil-collection
  (setq evil-collection-mode-list
        (remove 'lispy  evil-collection-mode-list)))
(require 'crafted-evil)

(require 'crafted-completion)
         ;; (require
         ;; 'crafted-windows)	

(require 'crafted-lisp)

;; Lispy
(straight-use-package 'lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(straight-use-package 'form-feed)
(global-form-feed-mode)

(straight-use-package 'restart-emacs)

(straight-use-package 'magit)


;; insert mode bindings
(general-define-key
 :states 'insert
 "ö" "<escape>"
 )

;; avy timer
(setq avy-timeout-seconds 0.2)
;; Normal mode bindings
(general-define-key
 :states 'normal
 "å" "$"
 "ö" #'evil-avy-goto-char-timer
 )

;; Put in some org clock commands in the global keymap
;; The global keymap has the lowest priority but
;; the C-c prefix is often reserved for the user so
;; other modes are unlikely to overshadow it.
(general-define-key
 :prefix "C-c C-x"
 "C-o" 'org-clock-out
 "C-x" 'org-clock-in-last
 "ö" 'org-timer-set-timer
 )


; Bind wsl-copy to C-c C-v
(global-set-key
 (kbd "C-c C-c")
 'wsl-copy )

; Bind wsl-paste to C-c C-v
(global-set-key
 (kbd "C-c C-v")
 'wsl-paste)

(add-hook 'magit-process-prompt-functions 'magit-process-git-credential-manager-core)

;;; Clojure
(crafted-package-install-package 'cider)
(crafted-package-install-package 'clj-refactor)
(crafted-package-install-package 'clojure-mode)
(crafted-package-install-package 'flycheck-clojure)

(with-eval-after-load "clojure-mode"
  (require 'cider)
  (require 'clj-refactor)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              ;; keybindings mentioned on clj-refactor github page
              ;; conflict with cider, use this by default as it does
              ;; not conflict and is a better mnemonic
              (clj-add-keybindings-with-prefix "C-c r")))

  (with-eval-after-load "flycheck"
    (flycheck-clojure-setup)))

(add-hook 'clojure-mode #'aggressive-indent-mode)

;; org download
;; (crafted-package-install-package 'org-download)
(crafted-package-install-package 'org-web-tools)
(with-eval-after-load "org-mode"
  (autoload 'org-web-tools-insert-link-for-url "org-web-tools"))

(with-eval-after-load "org-mode"
  ;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (shell . t))))
