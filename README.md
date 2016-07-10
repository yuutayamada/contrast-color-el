# contrast-color-picker

This package only provide a single function that return a contrast
color using CIEDE2000 algorithm.  The contrast color will be picked
from `contrast-color-candidates` variable.  The default
colors are based on [Google's material design palette](https://material.google.com/style/color.html).

## Usage

``` lisp
(contrast-color-picker "#ff00ff") ; -> "#4caf50"

```

## License
MIT License
