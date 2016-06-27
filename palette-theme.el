;;; palette-theme.el --- palette theme -*- lexical-binding: t -*-

;; Author: Pavel Aslanov <asl.pavel@gmail.com>
;; URL: https://github.com/aslpavel/palette-emacs-theme
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: theme

;;; Commentary:

;; Color palette based Emacs theme.  Can be easily extended to support more color palettes.
;; Comes with gruvbox-{dark,light} included.  Please read README.md for details.

;;; Code:
(require 'cl)
(eval-when-compile (require 'rx))

(deftheme palette "palette color theme")

;; needs to be autoload (to be accessable in defcustom)
;;;###autoload
(defconst palette-colors-init
  '((gruvbox-dark . ((bg-hard     . "#1d2021")
                     (bg          . "#282828")
                     (bg-soft     . "#32302f")
                     (bg+1        . "#3c3836")
                     (bg+2        . "#504945")
                     (bg+3        . "#665c54")
                     (bg+4        . "#7c6f64")
                     (bg-bold     . "#928374")

                     (fg-bold     . "#a89984")
                     (fg+2        . "#bdae93")
                     (fg+1        . "#d5c4a1")
                     (fg          . "#ebdbb2")
                     (fg-1        . "#fbf1c7")

                     (red         . "#cc241d")
                     (red-bold    . "#fb4934")
                     (green       . "#98971a")
                     (green-bold  . "#b8bb26")
                     (yellow      . "#d79921")
                     (yellow-bold . "#fabd2f")
                     (blue        . "#458588")
                     (blue-bold   . "#83a598")
                     (purple      . "#b16286")
                     (purple-bold . "#d3869b")
                     (aqua        . "#689d6a")
                     (aqua-bold   . "#8ec07c")
                     (orange      . "#d65d0e")
                     (orange-bold . "#fe8019")))
    (gruvbox-light . ((bg-hard     . "#f9f5d7")
                      (bg          . "#fbf1c7")
                      (bg-soft     . "#f2e5bc")
                      (bg+1        . "#ebdbb2")
                      (bg+2        . "#d5c4a1")
                      (bg+3        . "#bdae93")
                      (bg+4        . "#a89984")
                      (bg-bold     . "#928374")

                      (fg-bold     . "#7c6f64")
                      (fg+2        . "#665c54")
                      (fg+1        . "#504945")
                      (fg          . "#3c3836")
                      (fg-1        . "#282828")

                      (red         . "#cc241d")
                      (red-bold    . "#9d0006")
                      (green       . "#98971a")
                      (green-bold  . "#79740e")
                      (yellow      . "#d79921")
                      (yellow-bold . "#b57614")
                      (blue        . "#458588")
                      (blue-bold   . "#076678")
                      (purple      . "#b16286")
                      (purple-bold . "#8f3f71")
                      (aqua        . "#689d6a")
                      (aqua-bold   . "#427b58")
                      (orange      . "#d65d0e")
                      (orange-bold . "#af3a03")
                      ))
    (satori . ((bg . "#3f3f4c")
               (bg-bold . "#525263")
               (fg . "#edeff2")
               (fg-bold . "#cccaca")
               (red . "#ac6a76")
               (red-bold . "#c47987")
               (green . "#7b8c58")
               (green-bold . "#90a366")
               (yellow . "#bc945a")
               (yellow-bold . "#d4a765")
               (blue . "#58698c")
               (blue-bold . "#7086b2")
               (purple . "#7b5e8d")
               (purple-bold . "#9572ab")
               (aqua . "#82a1b2")
               (aqua-bold . "#95b9cc")
               ))
    ))

;;;###autoload
(defgroup palette-theme nil
  "Base theme options."
  :group 'faces)

;;;###autoload
(defcustom palette-colors
  palette-colors-init
  "Base theme pallet as assoc list."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol
                                   :value-type color))
  :group 'palette-theme)

;;;###autoload
(defcustom palette-presets
  '((gruvbox-dark :soft purple-bold)
    (gruvbox-light :normal orange-bold))
  "Palette presets which will be rotated with `palette-preset-cycle'."
  :type '(alist :key-type symbol
                :value-type (group (radio (const :tag "Hard" :hard)
                                          (const :tag "Normal" :normal)
                                          (const :tag "Soft" :soft))
                                   symbol))
  :group 'palette-theme)

(defvar palette-preset-index 0
  "Current palette theme preset.")

;;;###autoload
(defun palette-preset-cycle ()
  "Cycle throught `palette-presets'."
  (interactive)
  (let* ((index (% palette-preset-index (length palette-presets)))
         (preset (nth index palette-presets))
         (colors (cdr (assoc (car preset) palette-colors)))
         (options (cdr preset))
         )
    (setq palette-preset-index (+ 1 index))
    (when colors
      (apply 'palette-theme-update colors options)
      preset)
    ))

(defun palette-color-blend (base-color mix-color mix-alpha)
  "Blends BASE-COLOR under MIX-COLOR with MIX-ALPHA."
  (let* ((blend (lambda (base mix) (+ (* mix mix-alpha) (* base (- 1 mix-alpha)))))
         ;; Built-in `color-name-to-rgb' tries to map color to closest color
         ;; from available color set, becuase it happens before blending,
         ;; it causes color distortion on tty
         (color-parse (lambda (color)
                        (if (string-match (rx (: bos "#"
                                                 (group hex hex)
                                                 (group hex hex)
                                                 (group hex hex)
                                                 eos)) color)
                            (mapcar (lambda (v) (/ (string-to-number v 16) 255.0))
                                    (list (match-string 1 color)
                                          (match-string 2 color)
                                          (match-string 3 color)))
                          (color-name-to-rgb color))))
         (color-to-string (lambda (rgb) (apply 'format "#%02x%02x%02x"
                                          (mapcar (lambda (c) (* c 255)) rgb))))
         (base-rgb (funcall color-parse base-color))
         (mix-rgb  (funcall color-parse mix-color)))
    (if (and base-rgb mix-rgb)
        (funcall color-to-string
                 (list (funcall blend (nth 0 base-rgb) (nth 0 mix-rgb))
                       (funcall blend (nth 1 base-rgb) (nth 1 mix-rgb))
                       (funcall blend (nth 2 base-rgb) (nth 2 mix-rgb)))))
    ))

;;;###autoload
(defun palette-theme-update (colors &optional contrast prime)
  "Update palette theme from COLORS, with optional CONTRAST, and PRIME color.

COLORS  : ((color-name . color) ...)|palette-symbol
CONTRAST: (:hard|:normal|:soft)
PRIME   : one color-name from COLORS"
  (let* (;; bind colors
         (bl          (lambda (b m a) (palette-color-blend b m a)))
         (color       (let ((colors-map (if (symbolp colors)
                                            (cdr (assoc colors palette-colors))
                                          colors)))
                        (lambda (name) (cdr (assq name colors-map)))))

         ;; background
         (bg          (or (funcall color (cdr (assq contrast '((:normal . bg)
                                                               (:hard . bg-hard)
                                                               (:soft . bg-soft)))))
                          (funcall color 'bg)))
         (bg-bold     (funcall color 'bg-bold))
         (bg+1        (or (funcall color 'bg+1) (funcall bl bg bg-bold 0.2)))
         (bg+2        (or (funcall color 'bg+2) (funcall bl bg bg-bold 0.4)))
         (bg+3        (or (funcall color 'bg+3) (funcall bl bg bg-bold 0.6)))
         (bg+4        (or (funcall color 'bg+4) (funcall bl bg bg-bold 0.8)))

         ;; foreground
         (fg          (funcall color 'fg))
         (fg-bold     (funcall color 'fg-bold))
         (fg-1        (or (funcall color 'fg-1) (funcall bl fg fg-bold 0.25)))
         (fg+1        (or (funcall color 'fg+1) (funcall bl fg fg-bold 0.50)))
         (fg+2        (or (funcall color 'fg+2) (funcall bl fg fg-bold 0.75)))

         ;; colors
         (red         (funcall color 'red))
         (red-bold    (funcall color 'red-bold))
         (green       (funcall color 'green))
         (green-bold  (funcall color 'green-bold))
         (yellow      (funcall color 'yellow))
         (yellow-bold (funcall color 'yellow-bold))
         (blue        (funcall color 'blue))
         (blue-bold   (funcall color 'blue-bold))
         (purple      (or (funcall color 'purple) (funcall color 'magenta)))
         (purple-bold (or (funcall color 'purple-bold) (funcall color 'magenta-bold)))
         (aqua        (or (funcall color 'aqua) (funcall color 'cyan)))
         (aqua-bold   (or (funcall color 'aqua-bold) (funcall color 'cyan-bold)))
         (orange      (or (funcall color 'orange) (funcall color 'yellow)))
         (orange-bold (or (funcall color 'orange-bold) (funcall color 'yellow-bold)))
         (prime       (or (funcall color prime) orange-bold))
         (highlights  (remove-if (apply-partially 'eq prime)
                                 (mapcar color '(purple-bold
                                                 blue-bold
                                                 aqua-bold
                                                 orange-bold
                                                 yellow-bold
                                                 green-bold))))
         ;; theme variables
         (vars
          `((highlight-symbol-foreground-color ,bg)
            (highlight-symbol-colors (list ,@highlights))
            (ansi-color-names-vector [,bg ,red ,green ,yellow ,blue ,purple ,aqua ,fg])
            (avy-background t)))

         ;; theme faces
         (faces
          `( ;; basic faces
            (default             :background ,bg :foreground ,fg)
            (cursor              :background ,fg)
            (fringe              :background ,bg+1)
            (shadow              :foreground ,bg+4)
            (link                :foreground ,blue-bold :underline t)
            (link-visited        :foreground ,blue :underline t)
            (trailing-whitespace :background ,(funcall bl bg red 0.5))
            (lazy-highlight      :background ,bg+1) ;; other then current match
            (error               :background ,red-bold :foreground ,bg)
            (warning             :background ,yellow-bold :foreground ,bg)
            (succes              :background ,bg :foreground ,green-bold)
            (escape-glyph        :inherit font-lock-comment-delimiter-face)
            (header-line         :background ,bg+2 :foreground ,fg-1)
            (match               :background ,(funcall bl bg prime 0.7))

            ;; modeline / powerline / airline
            (minibuffer-prompt   :background ,prime :foreground ,bg :bold t)
            (mode-line           :background ,bg+3 :foreground ,fg
                                 :box (:line-width 1 :color ,(funcall bl bg+3 prime 0.4)))
            (mode-line-inactive  :background ,bg+1 :foreground ,fg-bold
                                 :box (:line-width 1 :color ,bg+2))
            (mode-line-buffer-id :foreground ,fg-1 :bold t)
            (mode-line-highlight :background ,bg+3)
            (powerline-inactive1 :background ,(funcall bl bg+1 prime 0.6) :foreground ,fg)
            (powerline-inactive2 :background ,bg+1 :foreground ,fg-bold)
            (powerline-active1 :background ,prime :foreground ,bg)
            (powerline-active2 :background ,bg+3 :foreground ,fg)

            ;; syntax
            (font-lock-builtin-face       :foreground ,aqua-bold)
            (font-lock-constant-face      :foreground ,purple-bold)
            (font-lock-comment-face       :foreground ,bg+4)
            (font-lock-comment-delimiter-face :foreground ,bg+3)
            (font-lock-doc-face           :foreground ,bg-bold)
            (font-lock-function-name-face :foreground ,yellow-bold)
            (font-lock-keyword-face       :foreground ,red-bold :bold t)
            (font-lock-string-face        :foreground ,green-bold)
            (font-lock-variable-name-face :foreground ,blue-bold)
            (font-lock-type-face          :foreground ,purple-bold)
            (font-lock-warning-face       :foreground ,red :bold t)

            ;; current line{number}
            (linum                        :background ,bg+1 :foreground ,fg-bold)
            (hl-line                      :background ,bg+1)
            (linum-highlight-face         :inherit default :background ,prime :foreground ,bg)
            ;; highlighting
            (highlight-symbol-face :background ,(funcall bl bg prime 0.4) :foreground ,fg-1)
            (region                :background ,bg+2)
            (highlight             :background ,bg+1)
            (highlight-80+         :inherit trailing-whitespace)
            (secondary-selection   :background ,bg+1)
            (show-paren-match      :background ,(funcall bl bg green 0.5))
            (show-paren-mismatch   :inherit error)

            ;; isearch
            (isearch :background ,(funcall bl bg prime 0.8) :foreground ,bg)
            (isearch-fail :inherit error)

            ;; ivy
            (ivy-current-match           :background ,bg+2 :foreground ,fg)
            (ivy-minibuffer-match-face-1 :background ,bg+1 :foreground ,fg)
            (ivy-minibuffer-match-face-2 :background ,(funcall bl bg prime 0.8) :foreground ,bg)
            (ivy-minibuffer-match-face-3 :background ,(funcall bl bg (nth 0 highlights) 0.8) :foreground ,bg)
            (ivy-minibuffer-match-face-4 :background ,(funcall bl bg (nth 1 highlights) 0.8) :foreground ,bg)
            (ivy-confirm-face            :background ,green-bold :foreground ,bg)
            (ivy-match-required-face     :inherit error)
            (ivy-action                  :foreground ,green-bold)
            (ivy-subdir                  :inherit dired-directory)
            (ivy-modified-buffer         :foreground ,fg-1)
            (ivy-remote                  :foreground ,purple)
            (ivy-virtual                 :inherit link)
            (swiper-match-face-1 :inherit ivy-minibuffer-match-face-1)
            (swiper-match-face-2 :inherit ivy-minibuffer-match-face-2)
            (swiper-match-face-3 :inherit ivy-minibuffer-match-face-3)
            (swiper-match-face-4 :inherit ivy-minibuffer-match-face-4)
            (swiper-line-face    :inherit hl-line)

            ;; diff
            (diff-header                 :background ,bg+2 :foreground nil)
            (diff-file-header            :background ,bg+3 :foreground nil)
            (diff-hunk-header            :background ,bg+2 :foreground nil)
            (diff-context                :background nil :foreground ,fg-bold)
            (diff-changed                :background nil :foreground ,fg)
            (diff-added                  :background ,(funcall bl bg green 0.5))
            (diff-removed                :background ,(funcall bl bg red 0.5))
            (diff-indicator-changed      :inherit diff-changed)
            (diff-indicator-added        :inherit diff-added)
            (diff-indicator-removed      :inherit diff-removed)

            ;; company
            (company-tooltip                  :background ,bg+2 :foreground ,fg)
            (company-tooltip-selection        :background ,(funcall bl bg+2 prime 0.4))
            (company-tooltip-common           :foreground ,(funcall bl fg prime 0.8))
            (company-tooltip-common-selection :foreground ,(funcall bl fg prime 0.5))
            (company-tooltip-annotation       :foreground ,fg-bold)
            (company-tooltip-annotation-selection :foreground ,fg+2)
            (company-scrollbar-bg             :background ,bg+4 :foreground nil)
            (company-scrollbar-fg             :background ,prime :foreground nil)
            (company-preview                  :background ,bg+2 :foreground ,fg-bold)
            (company-preview-common           :inherit company-preview)

            ;; helm
            (helm-selection        :inherit highlight)
            (helm-header           :background ,bg+2 :foreground ,fg-1)
            (helm-candidate-number :background ,prime :foreground ,bg)

            ;; dired
            (dired-directory :foreground ,blue-bold :bold t)
            (dired-flagged   :inherit error :bold t)
            (dired-header    :foreground ,purple :bold t)
            (dired-ignored   :inherit shadow)
            (dired-mark      :background ,prime :foreground ,bg)
            (dired-marked    :inherit warning)
            (dired-symlink   :foreground ,aqua-bold)

            ;; rainbow delimiters
            (rainbow-delimiters-depth-1-face :foreground ,(funcall bl fg-1 bg+4 1.00))
            (rainbow-delimiters-depth-2-face :foreground ,(funcall bl fg-1 bg+4 0.88))
            (rainbow-delimiters-depth-3-face :foreground ,(funcall bl fg-1 bg+4 0.75))
            (rainbow-delimiters-depth-4-face :foreground ,(funcall bl fg-1 bg+4 0.65))
            (rainbow-delimiters-depth-5-face :foreground ,(funcall bl fg-1 bg+4 0.50))
            (rainbow-delimiters-depth-6-face :foreground ,(funcall bl fg-1 bg+4 0.38))
            (rainbow-delimiters-depth-7-face :foreground ,(funcall bl fg-1 bg+4 0.25))
            (rainbow-delimiters-depth-8-face :foreground ,(funcall bl fg-1 bg+4 0.12))
            (rainbow-delimiters-depth-9-face :foreground ,(funcall bl fg-1 bg+4 0.00))
            (rainbow-delimiters-unmatched-face :inherit show-paren-mismatch)

            ;; tty-menu
            (menu                   :background ,bg+3 :foreground ,fg-1)
            (tty-menu-enabled-face  :background ,bg+2 :foreground ,fg)
            (tty-menu-disabled-face :background ,bg+2 :foreground ,fg-bold)
            (tty-menu-selected-face :background ,(funcall bl bg+2 prime 0.4))

            ;; widget
            (widget-field :background ,bg+2 :foreground ,fg)
            (button       :background ,bg :foreground ,blue-bold :underline t)

            ;; customize UI settings
            (custom-button       :background ,bg+4 :foreground ,fg+1
                                 :box (:line-width 2 :style released-button))
            (custom-button-mouse :background ,bg+3 :foreground ,fg+1
                                 :box (:line-width 2 :style released-button))
            (custom-button-pressed :background ,bg+2 :foreground ,fg
                                   :box (:line-width 2 :style pressed-button))
            (custom-visibility   :inherit link)
            (custom-variable-tag :foreground ,blue :bold nil)
            (custom-face-tag     :foreground ,blue-bold :bold nil)

            ;; flycheck
            (flycheck-error :background ,(funcall bl bg red-bold 0.4) :foreground ,fg)
            (flycheck-fringe-error :background ,(funcall bl bg+1 red-bold 0.7) :foreground ,bg)
            (flycheck-warning :background ,(funcall bl bg yellow-bold 0.4) :foreground ,fg)
            (flycheck-fringe-warning :background ,(funcall bl bg+1 yellow-bold 0.7) :foreground ,bg)
            (flycheck-info :background ,(funcall bl bg green-bold 0.4) :foreground ,fg)
            (flycheck-fringe-info :background ,(funcall bl bg+1 aqua-bold 0.7) :foreground ,bg)

            ;; macrostep
            (macrostep-compiler-macro-face :foreground ,prime)
            (macrostep-expansion-highlight-face :inherit highlight)
            (macrostep-gensym-1 :foreground ,(nth 0 highlights))
            (macrostep-gensym-2 :foreground ,(nth 1 highlights))
            (macrostep-gensym-3 :foreground ,(nth 2 highlights))
            (macrostep-gensym-4 :foreground ,(nth 3 highlights))
            (macrostep-gensym-5 :foreground ,(nth 4 highlights))
            (macrostep-macro-face :inherit font-lock-keyword-face)

            ;; compile
            (compilation-info :foreground ,green-bold)
            (compilation-error :foreground ,red-bold)
            (compilation-warning :foreground ,yellow-bold)
            (compilation-line-number :foreground ,fg-bold)

            ;; hydra
            (hydra-face-amaranth :foreground ,purple :bold t)
            (hydra-face-blue     :foreground ,blue :bold t)
            (hydra-face-pink     :foreground ,purple-bold :bold t)
            (hydra-face-red      :foreground ,red :bold t)
            (hydra-face-teal     :foreground ,aqua :bold t)

            ;; magit
            (magit-popup-heading          :foreground ,prime :bold t)
            (magit-popup-disabled-argument :foreground ,bg+4)
            (magit-popup-key              :foreground ,(funcall bl bg prime 0.7)
                                          :bold t)
            (magit-section-highlight      :inherit highlight)
            (magit-section-heading        :foreground ,prime :bold t)
            (magit-branch-remote          :foreground ,(funcall bl bg prime 0.6))
            (magit-branch-local           :foreground ,(funcall bl bg prime 0.85))
            (magit-branch-current         :foreground ,prime :bold t)
            (magit-diff-hunk-heading      :background ,bg+2)
            (magit-diff-hunk-heading-highlight :background ,bg+3)
            (magit-diff-added             :background ,(funcall bl bg green 0.35))
            (magit-diff-added-highlight   :background ,(funcall bl bg green 0.5))
            (magit-diff-removed           :background ,(funcall bl bg red 0.35))
            (magit-diff-removed-highlight :background ,(funcall bl bg red 0.5))
            (magit-diff-context           :foreground ,fg-bold)
            (magit-diff-context-highlight :foreground ,fg+2)
            (magit-diffstat-added         :foreground ,green-bold)
            (magit-diffstat-removed       :foreground ,red-bold)
            (magit-diff-file-heading)
            (magit-diff-file-heading-highlight :inherit highlight)
            (magit-hash                   :foreground ,fg-bold)
            (magit-tag :foreground ,yellow)
            (magit-log-author             :foreground ,fg+2)
            (magit-log-date               :foreground ,fg+2)
            (magit-log-graph              :foreground ,fg-1)

            ;; whitespace
            (whitespace-space :foreground ,bg+3)
            (whitespace-hspace :foreground ,bg+3)
            (whitespace-tab :foreground ,bg+3)
            (whitespace-newline :foreground ,bg+3)
            (whitespace-space-before-tab :foreground ,bg+3)
            (whitespace-indentation :foreground ,bg+3)
            (whitespace-empty :foreground ,bg+3)
            (whitespace-space-after-tab :foreground ,bg+3)
            (whitespace-trailing :inherit trailing-whitespace)
            (whitespace-line :background ,(funcall bl bg red 0.35))

            ;; avy
            (avy-lead-face :background ,prime :foreground ,bg)
            (avy-lead-face-0 :background ,(nth 0 highlights) :foreground ,bg)
            (avy-lead-face-1 :background ,(funcall bl bg prime 0.8) :foreground ,bg)
            (avy-lead-face-2 :background ,(nth 1 highlights) :foreground ,bg)
            (avy-background-face :foreground ,bg+4)

            ;; haskell
            (haskell-constructor-face :foreground ,purple)

            ;; js2-mode
            (js2-error :inherit error)
            (js2-warning :inherit warning)
            (js2-external-variable :foreground ,orange)
            (js2-function-call :inherit default)
            (js2-function-param :foreground ,aqua-bold)
            (js2-instance-member :foreground ,purple)
            (js2-object-property :inherit default)
            (Js2-private-function-call :foreground ,orange-bold)
            (js2-private-member :foreground ,purple)

            ;; eshell
            (eshell-prompt :foreground ,prime :bold t)
            (eshell-ls-executable :foreground ,green-bold)
            (eshell-ls-directory :inherit dired-directory)
            (eshell-ls-archive :inherit dired-directory :underline t)
            (eshell-ls-symlink :inherit dired-symlink)
            (eshell-ls-readonly :foreground ,(funcall bl fg red-bold 0.5))
            (eshell-ls-unreadable :foreground ,red-bold)
            (eshell-ls-special :foreground ,purple)
            (eshell-ls-clutter :foreground ,bg+4)
            (eshell-ls-backup :foreground ,bg+4)
            (eshell-ls-product :foreground ,yellow-bold)
            (eshell-ls-missing :foreground ,yellow)
            ))
         )
    (apply 'custom-theme-set-variables
             'palette
             (mapcar (lambda (var) `(,@var t)) vars))
    (apply 'custom-theme-set-faces
           'palette
           (mapcar (lambda (face)
                     `(,(car face) ((t ,@(cdr face))) t))
                   faces))
    ))

(when palette-presets
  ;; update palette them with first preset
  (setq palette-preset-index 0)
  (palette-preset-cycle)
  )

(provide-theme 'palette)
;;; palette-theme.el ends here
