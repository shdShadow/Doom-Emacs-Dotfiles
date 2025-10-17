
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(require 'dap-cpptools)
(map! :after dap-mode
      :map dap-mode-map
      "<f11>" #'dap-step-in)
(map! :after dap-mode
      :map dap-mode-map
      "<f10>" #'dap-next)
(map! :after dap-mode
      :map dap-mode-map
      "<f5>" #'dap-continue)
(after! nim-mode
  (setq lsp-nim-server-command '("nimlsp"))
  (add-hook 'nim-mode-hook #'lsp!)
  (add-hook 'nim-mode-hook
            (flycheck-mode 1))
  )
(after! lsp-mode
  (require 'lsp-pyright)
  (add-hook 'python-mode-hook #'lsp) ;; or #'lsp-deferred for lazy loading
  ;; Optional: configure pyright command if needed
  ;; (setq lsp-pyright-langserver-command '("pyright-langserver" "--stdio"))
  )
;; Ensure pdf-tools is installed and initialized
(pdf-tools-install)

;; Optional: Use pdf-view-mode by default for PDF files
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
;; Optional: Adjust some settings
(setq-default pdf-view-display-size 'fit-page) ;; Fit the entire page by default
(org-babel-do-load-languages
 'org-babel-load-languages
 '((rustic . t)))
(with-eval-after-load 'org (global-org-modern-mode))
(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))
;; Choose some fonts
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellipsis "…")

(global-org-modern-mode)
(set-face-attribute 'default nil :family "Iosevka" :height 140)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 150)
(set-face-attribute 'org-modern-symbol nil :family "Iosevka")

(add-hook 'lsp-after-open-hook (lambda ()
                                 (when (lsp-find-workspace 'rust-analyzer nil)
                                   (lsp-rust-analyzer-inlay-hints-mode))))
(after! lsp-mode
  (setq lsp-inlay-hint-enable t)
  (setq lsp-auto-guess-root nil))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;(add-hook 'org-mode-hook
;;          (lambda ()
;;            (add-hook 'after-save-hook #'org-latex-preview nil 'make-it-local)))
;; automatically preview LaTeX fragments
(setq org-startup-with-latex-preview t)
;; optional: auto-refresh previews on buffer changes
;;(setq org-latex-create-formula-image-program 'dvipng) ; or 'imagemagick

;;(setq doom-font (font-spec :family "Terminus" :size 20 :weight 'bold))

(add-to-list 'load-path "~/emacs-extern/site-lisp/emacs-application-framework/")
(require 'eaf)
(setq eaf-python-command "python3")
(require 'eaf-browser)


;;ORG-ROAM
(use-package! org-roam
  :custom
  (org-roam-directory "~/org-notes/") ;; where your notes will live
  :config
  (org-roam-db-autosync-enable))

;; Optional: Org-roam UI (graph view in browser)
(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))
(setq org-emphasis-alist
      '(("*" bold)
        ("_" underline)
        ("=" org-verbatim)
        ("+" (:foreground "red"))   ;; use + for red text
        ("%" (:foreground "green")) ;; use % for green text
        ("/" (:foreground "blue")) ;; ! for blue text
        ("~" (:foreground "orange"))));;? for orange text

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))


;; Test aesthetic

;; Fonts
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))

;; Enable font ligatures
(use-package! ligature
  :config
  (ligature-set-ligatures 't '("www" "**" "***" "==" "===" "!=" "!==" "=>" "->" "<-" "<=>" "<=" ">="))
  (global-ligature-mode t))

;; Theme (light + dark switch)
(setq doom-theme 'doom-gruvbox)
;; Optional: cycle themes with a keybind
(map! :leader "t t" #'counsel-load-theme)

;; Modeline
(use-package! doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project))

;; Icon support
(use-package! all-the-icons)

;; Transparent titlebar for macOS/Linux + rounded look
(setq default-frame-alist
      '((internal-border-width . 15)
        (left-fringe . 10)
        (right-fringe . 10)))

;; Smooth scrolling
(pixel-scroll-precision-mode 1)

;; Rainbow delimiters
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODOs nicely
(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Tree-sitter syntax highlighting
(use-package! treesit-auto
  :config
  (global-treesit-auto-mode))

;; Prettify symbols (λ, ∑, etc.)
(global-prettify-symbols-mode 1)

;; Company (completion)
(after! company
  (setq company-idle-delay 0.1        ;; how quickly suggestions pop up
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 15
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t))

;; Make completion UI pretty
(use-package! company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil
        company-box-max-candidates 50))

;; Optional: Use icons in completion popup
(use-package! all-the-icons
  :if (display-graphic-p))

;; Extra sources for company (like cape would add)
(use-package! company-math   ;; LaTeX, symbols, math mode
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package! company-dabbrev
  :after company
  :config
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-minimum-length 3))

;; LSP UI polish
(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-ui-sideline-enable t
        lsp-ui-doc-enable t))




;; Org modern (better fonts, spacing)
(use-package! org-modern
  :hook (org-mode . org-modern-mode))

;; Pretty bullets
(use-package! org-superstar
  :hook (org-mode . org-superstar-mode))

;; Org agenda polishing
(setq org-agenda-block-separator ?─
      org-agenda-time-grid '((daily today require-timed)
                             (800 1000 1200 1400 1600 1800 2000))
      org-agenda-current-time-string "⭠ now ─────────────────────────────")

;; Variable-pitch mode in org
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Org export aesthetic (HTML + LaTeX)
(setq org-html-htmlize-output-type 'css
      org-latex-packages-alist '(("" "booktabs" t)
                                 ("" "amsmath" t)
                                 ("" "amssymb" t)))

;; PDF tools
(use-package! pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;; Annotations
(use-package! org-noter
  :after pdf-tools
  :config
  (setq org-noter-notes-search-path '("~/org/notes/")))

;; EPUB reader
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode))
