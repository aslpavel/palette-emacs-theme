### Palette Emacs Theme
Color palette based emacs theme. You can specify small set of colors (palette) and based on it whole theme will be constructed, it would also include blended colors based on this palette.

#### Features
 - Add new color with ease, by specifying only 16 colors in `palette-colors`
 - Uses blended colors to extend palette

#### Color palettes
 - [gruvbox](https://github.com/morhetz/gruvbox) dark and light

#### Supported packages
  - [X] company
  - [X] diff
  - [X] dired
  - [X] ediff
  - [X] eshell
  - [X] fic
  - [X] flycheck
  - [X] fontlock faces
  - [X] highlight-symbol
  - [X] js2-mode
  - [X] magit
  - [X] raindow delimiters
  - [X] swiper/avy/counsel
  - [x] lsp-mode

### Usage
```emacs-lisp
;; make sure that this directoray is is added to `custom-theme-load-path`
(add-to-list 'custom-theme-load-path "path_to_checked_out_repository")
;; load theme
(load-theme 'palette-gruvbox-dark t)
;; or
(load-theme 'palette-gruvbox-light t)
```

### Contributions
If you don't have your favourite package supported, feel free to create pull request. 

### Screenshots
Gruvbox dark:
![gruvbox dark](/imgs/gruvbox-dark.png "gruvbox dark")
Gruvbox light:
![gruvbox light](/imgs/gruvbox-light.png "gruvbox light")
