; Fullscreen
(toggle-frame-fullscreen)
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
  (interactive "r")
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

; Bind wsl-copy to C-c C-v
(global-set-key
 (kbd "C-c C-c")
 'wsl-copy)

; Bind wsl-paste to C-c C-v
(global-set-key
 (kbd "C-c C-v")
 'wsl-paste)


;; Straight package manager
;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

(straight-use-package 'form-feed)
(global-form-feed-mode)

(straight-use-package 'restart-emacs)

(straight-use-package 'magit)
