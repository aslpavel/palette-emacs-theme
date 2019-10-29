;;; palette-theme-lib.el --- palette theme -*- lexical-binding: t -*-

;; Author: Pavel Aslanov <asl.pavel@gmail.com>
;; URL: https://github.com/aslpavel/palette-emacs-theme
;; License: MIT
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: theme

;;; Commentary:

;; Color palette based Emacs theme.  Can be easily extended to support more color palettes.
;; Comes with gruvbox-{dark,light} included.  Please read README.md for details.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'rx))

(eval-and-compile
  (defun palette-hex-to-rgb (color)
    "Parse from hex sRGB COLOR into linear RGB space

Built-in `color-name-to-rgb` tries to map color closest to color from
available color set, and causes problem on tty.
"
    (let ((srgb (if (string-match (rx (: bos "#"
                                         (group hex hex)
                                         (group hex hex)
                                         (group hex hex)
                                         eos)) color)
                    (mapcar (lambda (c) (/ (string-to-number c 16) 255.0))
                            (list (match-string 1 color)
                                  (match-string 2 color)
                                  (match-string 3 color)))
                  (color-name-to-rgb color))))
      (mapcar (lambda (c) (if (<= c 0.04045)
                              (/ c 12.95)
                            (expt (/ (+ c 0.055) 1.055) 2.4)))
              srgb)
      ))

  (defun palette-rgb-to-hex (srgb)
    (apply 'format "#%02x%02x%02x"
           (mapcar (lambda (c) (round (* (if (<= c 0.0031308)
                                             (* 12.92 c)
                                           (- (* 1.055 (expt c (/ 2.4))) 0.055)) 255)))
                   srgb)))

  (defun palette-blend (base-color mix-color mix-alpha)
    "Blends BASE-COLOR under MIX-COLOR with MIX-ALPHA."
    (let ((blend (lambda (base mix) (+ (* mix mix-alpha) (* base (- 1 mix-alpha)))))
          )
      (list (funcall blend (nth 0 base-color) (nth 0 mix-color))
            (funcall blend (nth 1 base-color) (nth 1 mix-color))
            (funcall blend (nth 2 base-color) (nth 2 mix-color))
            )
      ))

  (defun palette-blend-hex (base-color mix-color mix-alpha)
    "Blends #RRGGBB BASE-COLOR under MIX-COLOR with MIX-ALPHA."
    (let ((base-rgb (palette-hex-to-rgb base-color))
          (mix-rgb  (palette-hex-to-rgb mix-color)))
      (if (and base-rgb mix-rgb)
          (palette-rgb-to-hex (palette-blend base-rgb mix-rgb mix-alpha)))
      ))

  (defun palette-luminance (color)
    (+ (* .2126 (nth 0 color))
       (* .7152 (nth 1 color))
       (* .0722 (nth 2 color)))
    )

  (defun palette-auto-attrs (bg-hex bg-mix-hex bg-alpha fg-0-hex fg-1-hex &optional fg-alpha)
    "Create face with BG-MIX-HEX background with specified BG-ALPHA layered on top of BG-HEX.
     Foreground is picked from either FG-0-HEX or FG-1-HEX to result in best contrast.

BG-HEX: base background color
BG-MIX-HEX: background color layerd on top of BG-HEX
BG-ALPHA: alpha of BG-MIX-HEX
FG-{0|1}-HEX: alternative foreground colors
FG-ALPHA: optional foreground alpha
"
    (let* ((bg (palette-blend (palette-hex-to-rgb bg-hex)
                              (palette-hex-to-rgb bg-mix-hex)
                              bg-alpha))
           (fg-0 (palette-hex-to-rgb fg-0-hex))
           (fg-1 (palette-hex-to-rgb fg-1-hex))
           (fg-pair (if (> (palette-luminance fg-0)
                           (palette-luminance fg-1))
                        (cons fg-0 fg-1) (cons fg-1 fg-0)))
           (fg-mix (if (> (palette-luminance bg) .5)
                       (cdr fg-pair) (car fg-pair)))
           (fg (if fg-alpha (palette-blend bg fg-mix fg-alpha) fg-mix))
           )
      (list :foreground (palette-rgb-to-hex fg)
            :background (palette-rgb-to-hex bg))
      )))

;;;###autoload
(defmacro palette-theme-gen (name colors &optional contrast prime)
  "Generate palette theme with NAME from COLORS, with CONTRAST, and PRIME color.

NAME     : deftheme symbol name
COLORS   : ((color-name . color) ...)
CONTRAST : (:hard|:normal|:soft)
PRIME    : one color-name from COLORS"
  (let* (;; bind colors
         (bl          (lambda (b m a) (palette-blend-hex b m a)))
         (color       (lambda (name) (cdr (assq name colors))))

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
         (purple      (or (funcall color 'purple)
                          (funcall color 'magenta)))
         (purple-bold (or (funcall color 'purple-bold)
                          (funcall color 'magenta-bold)))
         (aqua        (or (funcall color 'aqua)
                          (funcall color 'cyan)))
         (aqua-bold   (or (funcall color 'aqua-bold)
                          (funcall color 'cyan-bold)))
         (orange      (or (funcall color 'orange)
                          (funcall color 'yellow)))
         (orange-bold (or (funcall color 'orange-bold)
                          (funcall color 'yellow-bold)))
         (prime       (or (funcall color prime) orange-bold))
         (highlights  (cl-remove-if
                       (apply-partially 'eq prime)
                       (list purple-bold blue-bold aqua-bold orange-bold yellow-bold green-bold)))

         ;; auto attrs
         (aa          (lambda (b m a &optional fa) (palette-auto-attrs b m a bg fg fa)))

         ;; theme variables
         (vars
          `((highlight-symbol-foreground-color ,bg)
            (highlight-symbol-colors (list ,@(mapcar (lambda (c) (funcall bl bg c 0.35)) highlights)))
            (highlight-symbol-foreground-color ,fg)
            (ansi-color-names-vector [,bg ,red ,green ,yellow ,blue ,purple ,aqua ,fg])
            (avy-background t)))

         ;; theme faces
         (faces
          `(;; basic faces
            (default             :background ,bg :foreground ,fg)
            (cursor              :background ,fg)
            (fringe              :background ,bg+1)
            (shadow              :foreground ,bg+4)
            (link                :foreground ,blue-bold :underline t)
            (link-visited        :foreground ,blue :underline t)
            (trailing-whitespace :background ,(funcall bl bg red 0.5))
            (lazy-highlight      :background ,bg+1) ;; other then current match
            (error               :background ,red-bold :foreground ,bg)
            (warning             ,@(funcall aa bg yellow-bold 0.8))
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
            (powerline-inactive1 :background ,(funcall bl bg+1 prime 0.3) :foreground ,fg)
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
            (line-number :background ,bg+1 :foreground ,fg-bold)
            (line-number-current-line :inherit default :background ,prime :foreground ,bg)

            ;; highlighting
            (highlight-symbol-face ,@(funcall aa bg prime 0.2))
            (region                :background ,bg+2)
            (highlight             :background ,bg+1)
            (highlight-80+         :inherit trailing-whitespace)
            (secondary-selection   :background ,bg+1)
            (show-paren-match      :background ,(funcall bl bg green 0.5))
            (show-paren-mismatch   :inherit error)
            (fic-face :background ,(funcall bl bg red 0.2) :foreground ,red-bold :bold t)
            (fic-author-face :foreground ,orange-bold :underline t)

            ;; isearch
            (isearch :background ,(funcall bl bg prime 0.8) :foreground ,bg)
            (isearch-fail :inherit error)

            ;; ivy
            (ivy-current-match :background ,bg+2)
            (ivy-minibuffer-match-face-1)
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

            ;; ediff
            (ediff-current-diff-A :background ,(funcall bl bg red-bold 0.35))
            (ediff-fine-diff-A :background ,(funcall bl bg red-bold 0.6))
            (ediff-even-diff-A :background ,(funcall bl bg red-bold 0.2))
            (ediff-odd-diff-A :inherit ediff-even-diff-A)

            (ediff-current-diff-B :background ,(funcall bl bg green 0.35))
            (ediff-fine-diff-B :background ,(funcall bl bg green 0.6))
            (ediff-even-diff-B :background ,(funcall bl bg green-bold 0.2))
            (ediff-odd-diff-B :inherit ediff-even-diff-B)

            (ediff-current-diff-C :background ,(funcall bl bg purple 0.35))
            (ediff-fine-diff-C :background ,(funcall bl bg purple 0.6))
            (ediff-even-diff-C :background ,(funcall bl bg purple 0.2))
            (ediff-odd-diff-C :inherit ediff-even-diff-C)

            (ediff-current-diff-Ancestor :background ,(funcall bl bg blue 0.35))
            (ediff-fine-diff-Ancestor :background ,(funcall bl bg blue 0.6))
            (ediff-even-diff-Ancestor :background ,(funcall bl bg blue 0.2))
            (ediff-odd-diff-Ancestor :inherit ediff-even-diff-Ancestor)

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
            (flycheck-error ,@(funcall aa bg red-bold 0.4))
            (flycheck-fringe-error ,@(funcall aa bg+1 red-bold 0.7))
            (flycheck-warning ,@(funcall aa bg yellow-bold 0.3))
            (flycheck-fringe-warning ,@(funcall aa bg+1 yellow-bold 0.7))
            (flycheck-info ,@(funcall aa bg green-bold 0.4))
            (flycheck-fringe-info ,@(funcall aa bg+1 aqua-bold 0.7))

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
            (magit-diff-added             :background ,(funcall bl bg green 0.2))
            (magit-diff-added-highlight   :background ,(funcall bl bg green 0.3))
            (magit-diff-removed           :background ,(funcall bl bg red 0.2))
            (magit-diff-removed-highlight :background ,(funcall bl bg red 0.3))
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

            ;; thrift
            (thrift-ordinal-face :bold t :foreground ,yellow)

            ;; eshell
            (eshell-prompt :foreground ,prime :bold t)
            (eshell-ls-executable :foreground ,green-bold)
            (eshell-ls-directory :inherit dired-directory)
            (eshell-ls-archive :inherit dired-directory :underline t)
            (eshell-ls-symlink :inherit dired-symlink)
            (eshell-ls-readonly :foreground ,(funcall bl fg red-bold 0.3))
            (eshell-ls-unreadable :foreground ,red-bold)
            (eshell-ls-special :foreground ,purple)
            (eshell-ls-clutter :foreground ,bg+4)
            (eshell-ls-backup :foreground ,bg+4)
            (eshell-ls-product :foreground ,yellow-bold)
            (eshell-ls-missing :foreground ,yellow)

            ;; language server protocol mode (lsp-mode)
            (lsp-face-highlight-read ,@(funcall aa bg yellow-bold 0.4))
            (lsp-face-highlight-write ,@(funcall aa bg red-bold 0.4))
            (lsp-face-highlight-textual ,@(funcall aa bg prime 0.4))
            (lsp-ui-doc-background ,@(funcall aa bg blue 0.2))
            (lsp-ui-sideline-symbol :foreground ,blue-bold)
            (lsp-ui-sideline-current-symbol :inherit lsp-ui-sideline-symbol :underline t)
            (lsp-ui-sideline-symbol-info :foreground ,bg+4)
            (lsp-ui-peek-list :background ,bg+1 :foreground ,fg)
            (lsp-ui-peek-peek ,@(funcall aa bg blue 0.1))
            (lsp-ui-peek-header :background ,bg+2)
            (lsp-ui-peek-highlight ,@(funcall aa bg prime 0.4))
            (lsp-ui-peek-selection :background ,bg+2)
            (lsp-ui-peek-line-number :foreground ,bg+4)
            (lsp-ui-peek-filename :foreground ,yellow-bold)
            ))
         )
    `(progn
       (custom-theme-set-variables ,name ,@(mapcar (lambda (var) `'(,@var t)) vars))
       (custom-theme-set-faces ,name ,@(mapcar (lambda (face) `'(,(car face) ((t ,@(cdr face))) t)) faces)))))

(provide 'palette-theme-lib)
;;; palette-theme-lib.el ends here
