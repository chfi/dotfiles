;;; -*- lexical-binding: t -*-

;; hide toolbars immediately upon emacs start
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use straight.el for package management
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (add-to-list 'straight-profiles '(unstable . "unstable.el"))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


(straight-use-package 'org)
(server-start)

;; ;;;;;;;;;;;;;;; Helper functions

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
          user-emacs-directory)
        ((boundp 'user-init-dir)
         user-init-dir)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  "Load FILE from current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))


;; ;;;;;;;;;;;;;;; Macbook-specific configuration

;; Bunch of stuff better off in another file
(setq jupiter-nix-hostname "jupiter-nix")
(setq macbook-hostname "Christians-MacBook-Air.local")

(load-user-file "scripts/macbook.el")

;; Can use (when (is-macbook) ..) and (text-scale-set `mul`) to change font size on laptop
;; (setq default-frame-alist '((font . "Source Code Pro-12")))

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     :height 130 :weight 'normal)


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path "/home/christian/.local/bin")

;; ;;;;;;;;;;;;;;; Packages/user

(load-file (expand-file-name "dhall/ob-dhall.el" user-init-dir))

(use-package diminish)

(use-package general)


(use-package avy
  :config
  (setq-default avy-keys '(97 111 101 117 105 104 116 110 115)))

(use-package ace-window
  :general
  (:keymaps 'override
   "s-n" 'ace-window))

;; (use-package clojure-mode)
  ;; :init
  ;; (add-hook 'clojure-mode-hook 'lispy-mode))

;; (use-package cider
;;   :after clojure-mode
;;   :init
;;   (add-hook 'clojure-mode-hook 'cider-mode)
;;   ;; (add-hook 'cider-mode-hook 'lispy-mode)
;;   ;; (add-hook 'cider-repl-mode-hook 'lispy-mode)
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'cider-repl-mode 'emacs))
;;   :general
;;   (:states '(normal insert)
;;            :keymaps 'clojure-mode-map
;;            "C-c C-z" 'cider-switch-to-repl-buffer
;;            "C-c C-k" 'cider-eval-buffer
;;            ;; "s-o"     'cider-find-dwim
;;            ;; "s-O"     'cider-find-dwim-other-window
;;            )
;;   (:states '(emacs)
;;            :keymaps 'cider-repl-mode-map
;;            "C-j" 'cider-repl-forward-input
;;            "C-k" 'cider-repl-backward-input)
;;   (:states '(normal emacs)
;;           :keymaps 'cider-browse-ns-mode-map
;;           "RET" 'cider-browse-ns-operate-at-point
;;           "^"   'cider-browse-ns-all
;;           "q"   'cider-popup-buffer-quit-function
;;           "s"   'cider-browse-ns-find-at-point
;;           ))




(use-package company
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  ;; (add-hook 'org-babel
  ;; (add-hook 'org-babely
  ;; (add-hook 'org-mode-hook 'company-mode)
  :config
  ;; (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  :general
  (:states '(normal insert)
           "C-SPC" 'company-complete))
           ;; "TAB" 'company-indent-or-complete-common))


(use-package counsel
  :init
  (setq-default counsel-find-file-at-point t))

(use-package deadgrep
  :config
  (general-define-key
   :states 'normal
   :keymaps 'deadgrep-mode-map
   "o" 'deadgrep-visit-result-other-window
   "O" 'deadgrep-visit-result
   "q" 'quit-window))


(use-package default-text-scale
  :init
  (default-text-scale-mode))

(use-package dhall-mode
  :mode "\\.dhall\\'"
  :config
  (setq-default dhall-format-command nil))

(use-package dyalog-mode)

(use-package ess)

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq-default evil-want-C-u-scroll nil)
  (setq-default evil-want-C-d-scroll nil)
  (setq-default evil-want-C-i-jump nil)
  (setq-default evil-want-keybinding nil)
  ;; (setq-default global-undo-tree-mode nil)
  ;; (setq-default undo-tree-visualizer-timestamps t)
  ;; (setq-default undo-tree-visualizer-diff t)
  (setq-default isearch-forward t) ;; Required to search downward by default when using swiper & the `n` key...
  (setq-default evil-ex-search-persistent-highlight nil)
  (evil-mode t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  :config
  (general-define-key
   :states 'normal
   "SPC" 'evil-scroll-down
   "S-SPC" 'evil-scroll-up))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        '(calc
          calendar
          cider
          company
          custom
          dired
          doc-view
          elisp-mode
          eshell
          flycheck
          flymake
          help
          ivy
          magit))
  (evil-collection-init))

(use-package evil-escape
  :diminish evil-escape-mode
  :after evil
  :config
  (setq-default evil-escape-key-sequence "uu"
                ;; evil-escape-unordered-key-sequence t
                evil-escape-delay 0.2)
  (evil-escape-mode))


(use-package evil-org
  :diminish evil-org-mode
  :after evil
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map
    "F" 'org-agenda-follow-mode))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :init
  (evil-commentary-mode))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1)
  :general
  (:states 'visual
   :keymaps 'override
   "s" 'evil-surround-region))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  ;; For some reason, having flycheck check syntax every new line in purescript-mode
  ;; is very slow -- but only on nixos, not the macbook!
  (unless (is-macbook)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
    (setq flycheck-idle-change-delay 4.0)))

(use-package glsl-mode)

(use-package haskell-mode
  :defer t)

(use-package hledger-mode
  :mode "\\.hledger.journal\\'"
  )

;; (use-package attrap
;;   ;; :defer t
;;   :general
;;   (:states '(normal insert)
;;    :keymaps 'haskell-mode-map
;;    "C-c s" 'attrap-attrap))

;; (use-package dante
;;   :defer t
;;   :after haskell-mode
;;   ;; :after attrap
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   :config
;;   (add-to-list 'company-backends 'dante-company)
;;   (setq dante-repl-command-line '("stack" "repl"))
;;   :general
;;   (:states '(normal insert)
;;    :keymaps 'haskell-mode-map
;;    "C-c C-t" 'dante-type-at))
;;    ;; "C-c s" 'dante-auto-fix))

(use-package hydra)

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t   ; extend searching to bookmarks
        ivy-height 20               ; set height of the ivy window
        ivy-count-format "(%d/%d) " ; count format, from the ivy help page
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package ivy-hydra)

(use-package ivy-bibtex
  :defer t
  :commands 'ivy-bibtex
  :init
  (general-define-key
    :states '(normal insert)
    :keymaps '(org-mode-map latex-mode-map)
    "C-c C-b" 'ivy-bibtex)
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

;; (use-package jq-mode
;;   :defer t
;;   :commands 'jq-mode
;;   :mode (("\\.jq\\'" . jq-mode)))

(use-package julia-mode
  :defer t
  )

(use-package lua-mode
  :init
  (setq lua-indent-level "2"))

(use-package magit
  :init
  (use-package evil-magit)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package nix-mode
  :defer t)

(use-package nixos-options
  :defer t)

(use-package nov
  :defer t
  :commands 'nov-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq-default nov-text-width 80))

(use-package company-nixos-options
  :defer t
  :after company
  :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package nix-sandbox
  :defer t)

;; (use-package lispy
;;   :diminish lispy-mode
;;   :config
;;   (setq-default lispy-close-quotes-at-end-p t)
;;   (add-hook 'emacs-lisp-mode-hook 'lispy-mode))
  ;; (add-hook 'clojure-mode-hook 'lispy-mode))

;; (use-package lispyville
;;   :diminish lispyville-mode
;;   :after lispy
;;   :config
;;   (add-hook 'lispy-mode-hook #'lispyville-mode)
;;   (lispyville-set-key-theme
;;    '(operators
;;      ;; c-w
;;      (escape insert)
;;      (additional-movement normal visual motion)
;;      slurp/barf-lispy)))

(use-package psc-ide
  :defer t
  :diminish psc-ide-mode
  :general
  (:states 'normal
   :keymaps 'psc-ide-mode-map
           "C-c s" 'psc-ide-flycheck-insert-suggestion)
  (:states '(normal insert)
   :keymaps 'psc-ide-mode-map
   "M-ö" 'psc-ide-goto-definition)
  :init
  (add-hook 'purescript-mode-hook 'psc-ide-mode))
  ;; :config
  ;; (setq psc-ide-editor-mode t))

(use-package purescript-mode
  :defer t
  :diminish purescript-indent-mode
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package python-mode
  :defer t)

(use-package prolog
  :defer t)


(use-package racket-mode
  :defer t)


(use-package rust-mode
  :defer t
  :commands 'rust-mode
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
  (setq rust-format-on-save t)
  :general
  (:states '(normal insert)
   :keymaps 'rust-mode-map
   "C-c C-c" 'rust-compile)
  )

(use-package flycheck-rust
  :defer t
  :commands 'flycheck-rust-setup)

(use-package racer
  :defer t
  :general
  (:states '(normal insert)
   :keymaps 'rust-mode-map
   ;; "M-ö" 'racer-find-definition
   "C-c C-t" 'racer-describe
   )
  (:states '(normal)
   :keymaps 'rust-mode-map
   "-" 'racer-describe
   )
  :config
  (add-hook 'racer-mode-hook 'eldoc-mode)
  )

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; (use-package smartparens
;;   :init
;;   (use-package evil-smartparens
;;     :diminish evil-smartparens-mode
;;     :init
;;     (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
;;   (require 'smartparens-config))


;; (use-package lispy
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;;   (use-package lispyville
;;     :init
;;     (add-hook 'lispymode-hook 'lispyville-mode)))

(use-package sparql-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))
  (add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode)))

(use-package swiper)

(use-package yaml-mode
  :defer t)

(use-package yasnippet
  :diminish 'yas-minor-mode
  :init
  (yas-global-mode 1)
  :general
  (:states 'insert
           "C-c o" 'company-yasnippet
           "C-c e" 'yas-expand))

(use-package zoom
  :diminish zoom-mode
  :init
  (setq-default zoom-size '(80 . 40))
  (zoom-mode t))


;; (use-package nand2tetris
;;   :defer t
;;   :config
;;   (use-package nand2tetris-assembler
;;     :defer t)
;;   (use-package company-nand2tetris
;;     :defer t))


;; ;;;;;;;;;;;;; Keybindings


(general-override-mode)


;; (defvar agenda-leader "å")

;; (known) Undefined keys to make use of (in all modes? probs not)
;; M-p
;; M-n


;; M-h - mark-paragraph, could be useful, except it works very strangely
;; M-m - move point to first non-whitespace character on line

;; unset some garbage default emacs keybindings I never use (in Normal
;; mode)
(general-define-key
 :states 'normal
 :keymaps 'global
 "M-a" nil ;; backward-sentence by default
 "M-e" nil ;; forward-sentence
 "M-u" nil ;; upcase-word
 "M-k" nil ;; kill-sentence
 "M-i" nil ;; tab-to-tab-stop
 "M-t" nil ;; transpose-words
 "M-c" nil ;; capitalize-word
 "M-l" nil ;; downcase-word
 )



(general-define-key
 ;; :prefix agenda-leader
 :states 'normal
 :keymaps 'global
 ;; "å" 'org-agenda-list
 "å" 'org-agenda
 ;; "u" 'org-todo-list
 )

(general-define-key
 :states '(normal insert visual)
 :keymaps 'global
 "<XF86HomePage>" 'org-agenda-list
 "<f2>" 'org-agenda-list
 )



(defvar leader-key "ä") ;; `ä` on Svorak is roughly comparable to `,` on QWERTY, ergonomically





(general-define-key
 :prefix leader-key
 :states 'normal
 :keymaps 'global
 "SPC" 'jump-to-register
 "w" 'window-configuration-to-register
 "p" 'point-to-register
 )

;; (general-define-key
;;   "s-h" 'evil-window-left
;;   "s-l" 'evil-window-right
;;   "s-j" 'evil-window-down
;;   "s-k" 'evil-window-up)


;; Swiper search with / in normal mode
(general-define-key
  :states 'normal
  "/" 'swiper)

;; replace default keybindings to use ivy & co
(general-define-key
  "C-s"     #'deadgrep
  "M-x"     'counsel-M-x
  "<f1> f"  'counsel-describe-function
  "<f1> v"  'counsel-describe-variable
  "<f1> l"  'counsel-find-library
  "<f1> i"  'counsel-info-lookup-symbol
  "<f1> u"  'counsel-unicode-char)


(general-define-key
  :states 'normal
  :keymaps 'override
  "Å" 'pop-global-mark
  "R" 'ivy-resume ;; i have never, ever, used `evil-replace-state`.
  "M-y" nil
  "-" nil
  "K" nil
  ;; "<f3> <f3>" 'org-agenda-list
  ;; "<f4>" 'org-agenda-list
  )



(general-define-key
 :states 'normal
 :keymaps 'global
 "s" 'ivy-switch-buffer
 "S" 'counsel-find-file)

(general-define-key
 :states '(normal insert)
 :keymaps 'global
 "C-+" 'default-text-scale-increase
 "C--" 'default-text-scale-decrease
 "C-=" 'default-text-scale-reset)

;; ;; <C-f> as prefix for finding files
;; (general-define-key
;;   :states 'normal
;;   :keymaps 'global
;;   "C-f C-f" 'counsel-find-file
;;   "C-f C-g" 'counsel-git
;;   "C-f C-j" 'counsel-file-jump
;;   "C-f C-r" 'counsel-rg)

(general-define-key
  :states 'normal
  :keymaps 'dired
  "g r" 'revert-buffer)

;; Bindings for company-mode
(general-define-key
  :states 'insert
  :keymaps 'company-mode-map
  "C-j" 'company-select-next
  "C-k" 'company-select-previous)


;; Nicer bindings for moving thru & dispatching ivy actions
(general-define-key
 :keymaps '(ivy-minibuffer-map
            ivy-switch-buffer-map)
  "C-j" 'ivy-next-line
  "C-k" 'ivy-previous-line
  "C-'" 'ivy-avy
  "M-." 'ivy-avy
  "C-SPC" 'ivy-avy
  "<backtab>" 'ivy-avy
  "C-s" 'ivy-dispatching-done)


;; Nicer Svorak-y bindings for x-ref, duplicated for linux & mac
(general-define-key
  :states '(normal insert)
  "M-ä" 'xref-pop-marker-stack
  "M-ö" 'xref-find-definitions
  "M-å" 'xref-find-references)
  ;; "s-a" 'xref-pop-marker-stack
  ;; "s-o" 'xref-find-definitions
  ;; "s-e" 'xref-find-references)


(general-define-key
  :prefix leader-key
  :states 'normal
  "e" 'flycheck-list-errors)

(general-define-key
  :prefix leader-key
  :states 'normal
  "gs" 'magit-status
  "gb" 'magit-blame)

;; fix keybindings for the flycheck error list
(general-evil-define-key 'normal 'flycheck-error-list-mode-map
  "j" 'flycheck-error-list-next-error
  "k" 'flycheck-error-list-previous-error
  "<return>" 'flycheck-error-list-goto-error)


;; Better org-mode bindings, compat w/ i3 etc.
;; (general-define-key
;;   :states '(normal insert)
;;   :keymaps 'org-mode-map
;;   "s-<return>" 'org-insert-heading
;;   "C-c C-q" 'counsel-org-tag
;;   )


(general-define-key
  :states '(normal insert)
  :keymaps 'global
  "C-c a" 'org-agenda)

;; ;;;;;;;;;;;;; Settings

;; remove trailing whitespace when saving a buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; enable abbrev-mode
(add-hook 'prog-mode-hook 'abbrev-mode)

(diminish 'abbrev-mode "")
(diminish 'eldoc-mode "")

;; spaces instead of tabs and other default indent config
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)


;; from https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(setq delete-old-versions -1)		; delete excess backup versions silently
(setq version-control t)		; use version control
(setq vc-make-backup-files t)		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t)				       ; don't ask for confirmation when opening symlinked file
(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)	; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";; Begin") ; print a default message in the empty scratch buffer opened at startup

(setq auto-save-default nil) ; disable auto-save
(setq make-backup-files nil)

(global-auto-revert-mode t)

(setq register-preview-delay 0)

; scroll line-by-line before cursor reaches top/bottom edge of screen
(setq scroll-margin 14
      scroll-conservatively 1)


(setq org-directory "~/Sync/org/")
;; (setq initial-buffer-choice (concat org-directory "writing.org"))
(add-hook 'after-init-hook 'org-agenda-list)




;;;;;;;;;;;;; Org-mode stuff

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-cite)
    (default       . bibtex-completion-format-citation-cite)))


(require 'subr-x)
(straight-use-package 'git)

(use-package htmlize)
(use-package ox-reveal
  :init
  (setq org-reveal-root "file:///home/christian/code/reveal.js"))

(use-package org
  :diminish org-indent-mode
  :config
  (require 'cl) ;; without this, using shift+up/down to modify timestamp minutes crashes
  (setq org-src-fontify-natively t)
  ;; (add-to-list 'org-agenda-files org-directory)
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 2)))

  ;; (setq org-refile-targets (quote ((nil :maxlevel . 2)))
                                   ;; (org-agenda-files :maxlevel . 2))))

  (setq org-startup-indented t)
  (setq org-tags-column 1)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-indirect-buffer-display 'current-window)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING" "HOLD" "|" "CANCELLED")))

  (setq org-todo-keyword-faces
        '(("NEXT" :foreground "white" :background "gray33" :weight bold)
          ("WAITING" :foreground "lightgray" :weight bold)
          ("HOLD" :foreground "darkgray" :weight bold)
          ("CANCELLED"  :foreground "lightgray" :background "DarkRed" :weight bold)))

  (setq org-agenda-window-setup 'other-window)
  (setq org-agenda-restore-windows-after-quit t)

  (setq org-agenda-prefix-format
        ;; '((agenda . " %i  %-16:c%?-6t% s")
        '((agenda . " %i  %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-skip-scheduled-if-done t)

  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " -- " "----------------"))

  (setq org-agenda-sorting-strategy
        '((agenda habit-up time-up priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))

  (setq org-global-properties
        '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
          ("STYLE_ALL" . "habit")))

  ;; (setq org-clock-persist-file  ())
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; for easy templates after org 9.1
  (require 'org-tempo)


  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key "christian@chfi.se")

  (require 'org-habit)
  (setq org-habit-graph-column 50)

  (require 'org-feed)

  (require 'ox-reveal)

  (require 'ox-beamer)

  ;; decrypt :crypt: entries with C-c C-c when point in entry body
  (add-hook 'org-ctrl-c-ctrl-c-final-hook
            (lambda ()
              (when (and (not (org-at-heading-or-item-p))
                         (org-at-encrypted-entry-p))
                (org-decrypt-entry))))

  ;; for capturing from firefox etc.
  (require 'org-protocol)

  ;; my own creation~~
  ;; (require 'ob-dhall)

  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)



  (mapc (apply-partially 'add-to-list 'org-src-lang-modes)
        '(("dhall"      . dhall-mode)
          ("purescript" . purescript-mode)
          ("clojure"    . clojure-mode)))


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (dhall . t)
     (haskell . t)
     (emacs-lisp . t)
     (sparql . t))))



(use-package org-journal
  :ensure t
  :defer t
  :init
  (add-hook 'org-journal-after-entry-create-hook
            'evil-insert-state)
  :config
;; (org-roam-directory (concat org-directory "roam/"))
  (setq org-journal-dir (concat org-roam-directory "journal/")
        org-journal-date-format "%Y-%m-%d %A"
        org-journal-time-format "[%Y-%m-%d %a %H:%M]"
        org-journal-time-prefix ""
        org-journal-file-type 'monthly
        org-journal-file-format "%Y-%m.org"
        org-journal-file-header "#+TITLE: %Y-%m\n#+STARTUP: overview\n"
        org-journal-hide-entries-p nil))

(general-define-key
 :states 'normal
 :keymaps 'global
 "C-c C-j" 'org-journal-new-entry)


(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (concat org-directory "roam/"))
      (org-roam-graph-exclude-matcher '("private"))
      (org-roam-completion-system 'ivy)
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

;; config org-agenda-custom-commands


(defun chfi/org-add-created-time ()
  "Add a 'CREATED' property with the current time to the entry at point, if it does not have one."
  (interactive)
  (unless (org-entry-get (point) "CREATED")
    (org-entry-put
     (point) "CREATED"
     (format-time-string (org-time-stamp-format t t)))))

(defun chfi/org-update-modified-time ()
  "Set the 'MODIFIED' property to the current time to the entry at point."
  (interactive)
  (org-entry-put
   (point)
   "MODIFIED"
   (format-time-string (org-time-stamp-format t t))))

(defun chfi/org-update-entry-time ()
  "Add 'CREATED' field if it does not exist, and update 'MODIFIED' field to current time at entry."
  (interactive)
  (chfi/org-add-created-time)
  (chfi/org-update-modified-time))



(setq-default
 org-agenda-custom-commands
 '(
   ("h" "Habits" tags-todo "STYLE=\"habit\""
    ((org-agenda-overriding-header "Habits")
     (org-agenda-sorting-strategy
      '(todo-state-down effort-up category-keep))))

   (" " "Agenda"
    ((agenda "" nil)
     (tags-todo "CATEGORY=\"Task\"" nil)
     (todo "NEXT")))

   ("n" "NEXT tasks" todo "NEXT")


   ("i" "View inbox" tags "inbox"
    ((org-agenda-prefix-format "%l")
      ))
;; "l" to search the library
;; "ll" to search all library files
;; "la" to search only (academic) articles
;; "lb" to search only books
;; "lw" to search only blog posts/online articles
   ("l" . "View library")
   ;; ("l "
   ("la" "View academic articles" tags "CATEGORY=\"Article\""
    (
     (org-agenda-prefix-format "%l - ")
     (org-agenda-sorting-strategy '())
     ;; (org-agenda-remove-tags t)
     ;; (org-agenda-sorting-strategy '(priority-down))
    ))
   ("lb" "View books" tags "CATEGORY=\"Book\""
    (
     (org-agenda-prefix-format "%l%T: ")
     (org-agenda-remove-tags t)
     (org-agenda-sorting-strategy '(priority-down))
    ))

   ))


;; config org-feed
(defun wrap-string (n string)
  "Wrap STRING to N columns width and return it."
  (mapconcat 'identity (org-wrap string n) "\n"))

(setq-default
 org-feed-alist
 '(
   ("Mad in America"
    "https://www.madinamerica.com/feed/"
    "~/Sync/org/news.org"
    "Mad in America"
    ;; unescaped regexp: "<!\\[CDATA\\[\\(.+\\)\\]\\]>"
    :template "
* %h
%U

%(wrap-string 60
  (replace-regexp-in-string
    \"<!\\\\[CDATA\\\\[\\\\(.+\\\\)\\\\]\\\\]>\"
    \"\\\\1\"
    \"%description\"))

%a
")
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry nil)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-max)))


(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/Sync/org/todo.org" "Tasks")
         "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%i\n  %a")

        ("j"
         "Journal"
         plain
         (function org-journal-find-location)
         "%i%?"
         :unnarrowed t)

        ("l" "Add entry to library.org")
        ;; ("ll" "Add entry" entry

        ("la" "Add academic publication with URL" entry
         (file+olp (lambda () (concat org-directory "library/library.org")) "Academic" "Uncategorized")
         "* %^{Title}
:PROPERTIES:
:CREATED: %U
:url:  %^{URL}
:year: %^{year}
:END:
%?\n\n")


        ("la" "Add web resource publication with URL" entry
         (file+olp (lambda () (concat org-directory "library/library.org")) "Web" "Uncategorized")
         "* %^{Title}
:PROPERTIES:
:CREATED: %U
:url:  %^{URL}
:END:
%?\n\n")

        ("lb" "Add book" entry
         (file+olp (lambda () (concat org-directory "library/library.org")) "Books" "Uncategorized")
         "* %^{Title}
:PROPERTIES:
:author: %^{author}
:year: %^{year}
:END:
%?\n\n")

        ("p" "Protocol" entry
         (file+headline (lambda () (concat org-directory "notes.org")) "Inbox")
         "\n* %^{Title}
:PROPERTIES:
:CREATED: %U
:END:

#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")

        ("L" "Protocol Link" entry
         (file+headline (lambda () (concat org-directory "notes.org")) "Inbox")
         "\n* %? [[%:link][%:description]]
:PROPERTIES:
:CREATED: %U
:END:\n\n\n")

        ))




(add-hook 'evil-org-mode-hook
  (lambda ()

    (general-evil-define-key 'normal 'evil-org-mode-map
      "-" 'org-ctrl-c-minus
      "|" 'org-table-goto-column)

    ;; (general-evil-define-key 'normal 'evil-org-mode-map
    ;;   "s-H" 'org-shiftmetaleft
    ;;   "s-J" 'org-shiftmetadown
    ;;   "s-K" 'org-shiftmetaup
    ;;   "s-L" 'org-shiftmetaright)

    (general-evil-define-key '(normal insert) 'evil-org-mode-map
      "C-c C-q" 'counsel-org-tag
      "C-c ," 'org-time-stamp-inactive)

    (general-define-key
     :states '(normal insert)
     "C-c c" 'org-capture
     "C-c l" 'org-store-link)



    (general-define-key
     :prefix leader-key
     :states 'normal
     :keymaps 'evil-org-mode-map
     "å" 'hydra-org-state/body
     "t" 'org-todo
     "T" 'org-show-todo-tree
     "v" 'org-mark-element
     "a" 'org-agenda
     "C-c" 'org-archive-subtree
     "l" 'evil-org-open-links
     "C" 'org-resolve-clocks
     "C-c" 'chfi/org-add-created-time
     "C-t" 'chfi/org-update-entry-time)

    (defhydra hydra-org-state (:hint nil)
      "
^_h_, _l_: Move up and down tree headlines
^_j_, _k_: Move between sibling headlines
^_i_, _SPC_, _I_: Cycle fold states

^_H_, _J_, _K_, _L_: org-meta(left|down|up|right)
^_J_, _K_: Move subtree up/down
^_H_, _L_: Demote/promote heading

^_o_: Open link at point
^_C_: Toggle ARCHIVE tag
^_R_: Archive subtree

^_t_: TODO dispatch
"
      ;; basic navigation
      ("i" org-cycle)
      ("SPC" org-cycle)
      ("I" org-shifttab)
      ("h" (lambda (_) (interactive "p") (org-up-heading-safe)))
      ("l" (lambda (_) (interactive "p") (org-goto-first-child)))
      ("j" org-forward-heading-same-level)
      ("k" org-backward-heading-same-level)
      ;; navigating links
      ;; ("n" org-next-link)
      ;; ("p" org-previous-link)
      ("o" org-open-at-point)
      ("C" org-toggle-archive-tag)
      ("R" org-archive-subtree)
      ;; navigation blocks
      ("N" org-next-block)
      ("P" org-previous-block)
      ;; updates
      ("." org-ctrl-c-ctrl-c)
      ("*" org-ctrl-c-star)
      ("-" org-ctrl-c-minus)
      ;; change todo state
      ("H" org-metaleft)
      ("L" org-metaright)
      ("J" org-metadown)
      ("K" org-metaup)
      ("t" org-todo))
    ))



;; ;;;;;;;;;;;;; Functions etc.

(defun sudo-edit (&optional arg)
  "Edit currently visited file as super user.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))



;; ;;;;;;;;;;;;; Themes

(use-package zenburn-theme
  :init
;; zenburn-default-colors-alist
  (load-theme #'zenburn t t))


(use-package material-theme
  :init
  (load-theme #'material t t)
  (load-theme #'material-light t t)
  :config
  (custom-theme-set-faces
   'material
   `(ivy-current-match
     ((,t (:background
           "dim gray"
           :underline ,t
           :weight bold))))
   `(org-agenda-date
     ((,t (:background
           "#364248"
           :foreground "#4dd0e1"
           :inverse-video nil
           :box (:line-width 2 :color "#465258")
           :overline nil
           :underline nil
           :slant normal
           :weight normal
           :height 1.0))))
   `(org-agenda-structure
     ((,t (:background
           "#364248"
           :foreground "#81d4fa"
           :inverse-video nil
           :box (:line-width 2 :color "#465258")
           :overline nil
           :underline nil
           :slant normal
           :weight normal
           :height 1.0))))
   `(org-agenda-date-today
     ((,t (:inherit
           org-agenda-date
           :foreground
           "#465258"
           ;; :foreground "#268bd2"
           :inverse-video t
           ;; :box 1
           :overline nil
           :underline t
           :weight bold))))
   ))
  ;; :custom
  ;; (custom-theme-set-faces 'material )
;; material


;; (ivy-current-match ((t (:background "dim gray" :underline t :weight bold))))
;; (org-agenda-date ((t (:background "dim gray" :foreground "#4dd0e1" :inverse-video nil :box (:line-width 2 :color "#fdf6e3") :overline nil :underline nil :slant normal :weight normal :height 1.0))))
;; (org-agenda-date ((t (:background nil

;; (use-package immaterial-theme
;;   :init
;;   (load-theme #'immaterial t t))

(use-package solarized-theme
  :init
  (load-theme #'solarized-dark t t)
  (load-theme #'solarized-light t t))

(use-package doneburn-theme
  :init
  (load-theme #'doneburn t t))


(use-package abyss-theme
  :init
  (load-theme #'abyss t t))

(use-package cyberpunk-theme
  :init
  (load-theme #'cyberpunk t t))

(use-package alect-themes
  :init
  (load-theme #'alect-light t t)
  (load-theme #'alect-black t t))


(enable-theme 'material)


;;;;;;;;;;;;; Customizer
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(avy-keys (quote (97 111 101 117 105 104 116 110 115)))
 '(browse-url-browser-function (quote browse-url-firefox))
 '(cider-show-error-buffer nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" "d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" "a1e99cb36d6235abbe426a0a96fc26c006306f6b9d2a64c2435363350a987b4c" default)))
 '(custom-theme-set-faces (quote material) t)
 '(dhall-command "/home/christian/.local/bin/dhall")
 '(dhall-format-command "nil")
 '(dhall-format-options (quote ("--inplace")))
 '(fci-rule-color "#383838")
 '(hl-sexp-background-color "#1c1f26")
 '(ivy-display-style nil)
 '(js-indent-level 2)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 2 :fileskip0 t)))
 '(org-agenda-files
   (quote
    ("~/Sync/org/work/phd.org" "~/Sync/org/work/uthsc.org" "~/Sync/org/projects.org" "~/Sync/org/news.org" "~/Sync/org/notes.org" "/home/christian/Sync/org/genome-browser.org" "~/Sync/org/common.org" "/home/christian/Sync/org/todo.org" "~/Sync/org/journal.org")))
 '(org-roam-completion-system (quote ivy))
 '(org-roam-directory "~/Sync/org/roam/")
 '(org-roam-graph-exclude-matcher (quote ("private")))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (org-remove-highlights-with-change)
     (eval let nil
           (org-babel-goto-named-src-block "setup-export-class")
           (org-babel-execute-src-block))
     (bibtex-completion-cite-prompt-for-optional-arguments)
     (bibtex-completion-bibliography . "./bibliography.bib"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
