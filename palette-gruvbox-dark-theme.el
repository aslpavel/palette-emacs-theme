;;; palette-gruvbox-dark-theme.el --- palette theme -*- lexical-binding: t -*-
;;; Commentary:

;; Palette theme generated from Gruvbox Dark colors

;;; Code:
(eval-when-compile
 (require 'palette-theme-lib))

(deftheme palette-gruvbox-dark "Palette theme generated from Gruvbox Dark colors")

(palette-theme-gen
 'palette-gruvbox-dark
 ( ;; background colors
  (bg-hard     . "#1d2021")
  (bg          . "#282828")
  (bg-soft     . "#32302f")
  (bg+1        . "#3c3836")
  (bg+2        . "#504945")
  (bg+3        . "#665c54")
  (bg+4        . "#7c6f64")
  (bg-bold     . "#928374")

  ;; foreground colors
  (fg-bold     . "#a89984")
  (fg+2        . "#bdae93")
  (fg+1        . "#d5c4a1")
  (fg          . "#ebdbb2")
  (fg-1        . "#fbf1c7")

  ;; misc colors
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
  (orange-bold . "#fe8019"))
 :normal
 aqua-bold
 )

(provide-theme 'palette-gruvbox-dark)
;;; palette-gruvbox-dark-theme.el ends here
