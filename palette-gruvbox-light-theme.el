;;; palette-gruvbox-light-theme.el --- palette theme -*- lexical-binding: t -*-
;;; Commentary:

;; Palette theme generated from Gruvbox Light colors

;;; Code:
(eval-when-compile
 (require 'palette-theme-lib))

(deftheme palette-gruvbox-light "Palette theme generated from Gruvbox Light colors")

(palette-theme-gen
 'palette-gruvbox-light
 ( ;; background colors
  (bg-hard     . "#f9f5d7")
  (bg          . "#fbf1c7")
  (bg-soft     . "#f2e5bc")
  (bg+1        . "#ebdbb2")
  (bg+2        . "#d5c4a1")
  (bg+3        . "#bdae93")
  (bg+4        . "#a89984")
  (bg-bold     . "#928374")

  ;; foreground colors
  (fg-bold     . "#7c6f64")
  (fg+2        . "#665c54")
  (fg+1        . "#504945")
  (fg          . "#3c3836")
  (fg-1        . "#282828")

  ;; misc colors
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
  )
 )

(provide-theme 'palette-gruvbox-light)
;;; palette-gruvbox-light-theme.el ends here
