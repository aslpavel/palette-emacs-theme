### Palette Emacs Theme
Color palette based emacs theme

#### Rationale
I like to change color theme, but it is often frustrating experience as different themes support different packages (faces). And after you switched to different color palette you need manually update all the faces that are not supported by this particular theme.

#### Features
 - Easy switching between palettes `palette-preset-cycle`
 - Add new color with ease, by specifying only 16 colors in `palette-colors`
 - Uses blended colors to extend palette

#### Color palettes
 - [gruvbox](https://github.com/morhetz/gruvbox) dark and light

#### Supported packages
  - [X] fontlock faces
  - [X] swiper/avy/counsel
  - [X] company
  - [X] flycheck
  - [X] magit
  - [X] eshell
  - [X] dired
  - [X] raindow delimiters
  - [X] diff
  - [X] js2-mode

### Usage
```emacs-lisp
(load-theme 'palette t)
```
You can setup presets which you can cycle over by setting `palette-presets` custom property. And use interactive `palette-preset-cycle` function to switch between presets.

### Contributions
If you don't have favourite package supported, feel free to create pull request. 

### Screenshots
Gruvbox dark:
![gruvbox dark](/imgs/gruvbox-dark.png "gruvbox dark")
Gruvbox light:
![gruvbox light](/imgs/gruvbox-light.png "gruvbox light")
