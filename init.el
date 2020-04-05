; LOAD PATH
(setq load-path (cons "~/.emacs.d/elisp/" load-path))

; Package
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; key bind
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-x\C-h" 'help)
(define-key global-map "\C-o" 'dabbrev-expand)
(define-key global-map "\C-x\C-g" 'goto-line)
(setq mac-command-key-is-meta t)
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; UTF-8
(set-locale-environment nil)
(set-language-environment 'Japanese)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(setq coding-system-for-read 'mule-utf-8)
(setq coding-system-for-write 'mule-utf-8)

;; windmove-left
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; hide menu-bar, tool-bar
(if window-system (menu-bar-mode 1)(menu-bar-mode -1)(tool-bar-mode -1))

;; undo
(setq undo-limit 100000)
(setq undo-strong-limit 130000)

;; backup file
(setq delete-auto-save-files t)
(setq backup-inhibited t)

;;scroll
(setq scroll-step 1)

;; tab
(setq indent-tabs-mode nil)
(setq tab-width 4)

;; new line code
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; trailing-whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default show-trailing-whitespace t)

;; highlights set of braket
(show-paren-mode 1)

;; setting
(setq-default transient-mark-mode t)
(setq-default truncate-lines t)
(blink-cursor-mode 0)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq ns-pop-up-frames nil)

;; make transparent
(add-to-list 'default-frame-alist '(alpha . (0.85 0.50)))

;; save the cursor position
(require 'saveplace)
(setq-default save-place t)

;; font lock
(global-font-lock-mode t)
(require 'font-lock)

;; ssh
(require 'tramp)
(setq tramp-default-method "ssh")

;; line column number
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d ")
(line-number-mode t)
(column-number-mode t)

;; Highlighting tab, two-byte space
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-display-mappings '((space-mark ?\u3000 [?\u25a1]) (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-foreground 'whitespace-tab "DimGrey")
(set-face-foreground 'whitespace-space "DimGrey")
(global-whitespace-mode 1)

;theme
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-charcoal-black))

;; workspace
(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

;; ruby mode
(add-to-list 'load-path "elisp/ruby")
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq-default indent-tabs-mode nil)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; zen code
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil))
(keyboard-translate ?\C-i ?\H-i)
(define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;for js3-mode
;;(autoload 'js3-mode "js3" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; yaml-mode
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("¥¥.yml$" . yaml-mode)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck company-go golint go-mode js2-mode yaml-mode web-mode emmet-mode typescript-mode editorconfig markdown-mode perspective ctags-update ctags linum-relative js3-mode f emms-info-mediainfo color-theme anaphora))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; typescript-mode
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(setq typescript-indent-level 2)
(put 'narrow-to-region 'disabled nil)


;; go-mode
(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin/"))
(add-to-list 'exec-path (expand-file-name "~/project/go/bin/"))

(require 'go-mode)
(require 'company-go)

(setq gofmt-command "goimports")
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook
          (lambda()
            (let ((envs '("GOROOT" "GOPATH")))
              (exec-path-from-shell-copy-envs envs))

            (add-hook 'before-save-hook 'gofmt-before-save)
            (local-set-key (kbd "M-.") 'godef-jump)
            (set (make-local-variable 'company-backends) '(company-go))
            (company-mode)
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 4)
            (setq tab-width 4)))
