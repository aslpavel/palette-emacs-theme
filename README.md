### Palette Emacs Theme
Color palette based emacs theme

#### Rationale
I like to change color theme, but it is often frustrating experience as different themes support different packages (faces). And after you switched to different color palette you need manually update all the faces that are not supported by this particular theme.

#### Features
 - Easy switching between palettes `palette-presset-cycle`
 - Add new color with ease, by specifying only 16 colors to `palette-colors`

#### Color palettes
 - [gruvbox](https://github.com/morhetz/gruvbox) dark and light

#### Supported packages
  - [X] Fontlock faces
  - [X] Swiper/Avy/Counsel
  - [X] Company
  - [X] Flycheck
  - [X] Magit
  - [X] EShell

### Usage
```emacs-list
(load-theme 'palette t)
```
You can setup pressets which you can cycle over by setting `palette-pressets` custom property. And use interactive `palette-presset-cycle` function to switch between pressets.

### Screenshots
Gruvbox dark:
![gruvbox dark](/imgs/gruvbox-dark.png "gruvbox dark")
Gruvbox light:
![gruvbox light](/imgs/gruvbox-light.png "gruvbox light")
