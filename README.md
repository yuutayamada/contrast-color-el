# contrast-color-picker

This package only provide a single function that return a contrast
color using CIEDE2000 algorithm.

## Usage

``` lisp
(contrast-color-picker "#ff00ff") ; -> "#4caf50"

;                 or

(contrast-color "Brightmagenta") ; -> "#4caf50"

```

Note that if you want to choose from more colors, below configuration set
[Google's material design's colors](https://material.google.com/style/color.html)
as color candidates:

``` lisp
(setq contrast-color-candidates contrast-color-material-colors
      contrast-color--lab-cache nil)
```

But keep in mind that this configuration may increase calculation time.

## License
MIT License
