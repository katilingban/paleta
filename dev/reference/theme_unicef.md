# A [ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html) theme using GNC fonts, colours, and palettes

These are wrappers for
[`theme_paleta()`](http://katilingban.io/paleta/dev/reference/theme_paleta.md)
that use colours and fonts from the GNC visual identity guidelines.

These are wrappers for
[`theme_paleta()`](http://katilingban.io/paleta/dev/reference/theme_paleta.md)
that use colours and fonts from the UNICEF visual identity guidelines.

## Usage

``` r
theme_gnc(
  base_family = set_gnc_font(),
  base_size = 11.5,
  plot_title_family = base_family,
  plot_title_colour = gnc_grey,
  subtitle_family = base_family,
  subtitle_colour = gnc_grey,
  caption_colour = gnc_grey,
  axis_title_colour = gnc_grey,
  legend_title_colour = gnc_grey,
  legend_text_colour = gnc_grey,
  grid_col = gnc_grey,
  grid = TRUE,
  axis_col = gnc_grey,
  axis = FALSE,
  ticks = FALSE
)

theme_unicef(
  base_family = set_unicef_font(),
  base_size = 11.5,
  plot_title_family = base_family,
  plot_title_colour = unicef_black,
  subtitle_family = base_family,
  subtitle_colour = unicef_cool_grey,
  caption_colour = unicef_cool_grey,
  axis_title_colour = unicef_cool_grey,
  legend_title_colour = unicef_cool_grey,
  legend_text_colour = unicef_cool_grey,
  grid_col = unicef_warm_grey,
  grid = TRUE,
  axis_col = unicef_warm_grey,
  axis = FALSE,
  ticks = FALSE
)
```

## Arguments

- base_family:

  Base font family using UNICEF fonts. Default is set by what UNICEF
  font is available in the system via
  [`set_unicef_font()`](http://katilingban.io/paleta/dev/reference/unicef_font.md).
  If none of the UNICEF fonts are available, the default becomes *Noto
  Sans*.

- base_size:

  Base font size. Default is 11.5.

- plot_title_family:

  Font family to use for the plot title. Default is `base_family`.

- plot_title_colour:

  Colour of the plot title text. Default is
  [unicef_black](http://katilingban.io/paleta/dev/reference/unicef_colours.md).

- subtitle_family:

  Font family to use for the plot subtitle. Default is `base_family`.

- subtitle_colour:

  Colour of the subtitle text. Default is
  [unicef_cool_grey](http://katilingban.io/paleta/dev/reference/unicef_colours.md).

- caption_colour:

  Colour of the caption text. Default is
  [unicef_cool_grey](http://katilingban.io/paleta/dev/reference/unicef_colours.md).

- axis_title_colour:

  Colour of the axis title text. Default is
  [unicef_cool_grey](http://katilingban.io/paleta/dev/reference/unicef_colours.md).

- legend_title_colour:

  Colour of the legend title text. Default is NULL.

- legend_text_colour:

  Colour of the legend text. Default is NULL.

- grid_col:

  Grid colour. Default to
  [unicef_warm_grey](http://katilingban.io/paleta/dev/reference/unicef_colours.md).

- grid:

  Panel grid. Either `TRUE`, `FALSE`, or a combination of `X` (major x
  grid), `x` (minor x grid), `Y` (major y grid), and/or `y` (minor y
  grid). Default is TRUE.

- axis_col:

  Axis colours. Default to
  [unicef_warm_grey](http://katilingban.io/paleta/dev/reference/unicef_colours.md).

- axis:

  Add x or y axes? `TRUE`, `FALSE`, "`xy`". Default is FALSE.

- ticks:

  Logical. Should ticks be added? Default is FALSE.

## Value

A
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
theme.

A
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
theme.

## Colours

The GNC theme is based on the colours from the
[gnc_palettes](http://katilingban.io/paleta/dev/reference/gnc_palette.md).
The primary palette consists of one colour: `gnc_palettes$gnc_primary`.
The secondary palette consists of ten colours:
`gnc_palettes$gnc_secondary`.

The UNICEF theme is based on the colours from the
[unicef_palettes](http://katilingban.io/paleta/dev/reference/unicef_palette.md).
The primary palette consists of one colour:
`unicef_palettes$unicef_primary`. The secondary palette consists of
eleven colours: `unicef_palettes$unicef_secondary`.

## Fonts

The GNC theme uses four fonts as prescribed by the GNC visual identity
guidelines. These fonts (in hierarchical order of preference) are
*Arial*, *Calibri*, *Source Sans Pro*, and *Bebas Neue*. Any or all of
these fonts should be available in the user's system for them to be used
in the theme. If none of these fonts are available in the user's system,
a freely downloadable alternative called *Noto Sans* is the default
fallback font and can be obtained from [Google
Fonts](https://fonts.google.com/).

The UNICEF theme uses four fonts as prescribed by the UNICEF visual
identity guidelines. These fonts (in hierarchical order of preference)
are *Univers LT Pro*, *Arial*, *Roboto*, and *Aleo*. Any or all of these
fonts should be available in the user's system for them to be used in
the theme. If none of these fonts are available in the user's system, a
freely downloadable alternative called *Noto Sans* is the default
fallback font and can be obtained from [Google
Fonts](https://fonts.google.com/).

## Examples

``` r
if (FALSE) { # \dontrun{
  ggplot(
    data = mtcars,
    mapping = aes(
      x = factor(vs, levels = c(0, 1), labels = c("v-shaped", "straight")),
      fill = factor(cyl))
  ) +
  geom_bar() +
  scale_fill_manual(
    name = "Cylinders",
    values = gnc_palettes$gnc_secondary
  ) +
  labs(
    title = "Engine shape by number of cylinders",
    subtitle = "An example plot for this package",
    x = "Engine Shape",
    y = "Counts"
   ) +
   theme_unicef()
} # }

if (FALSE) { # \dontrun{
  ggplot(
    data = mtcars,
    mapping = aes(
      x = factor(vs, levels = c(0, 1), labels = c("v-shaped", "straight")),
      fill = factor(cyl))
  ) +
  geom_bar() +
  scale_fill_manual(
    name = "Cylinders",
    values = unicef_palettes$unicef_secondary
  ) +
  labs(
    title = "Engine shape by number of cylinders",
    subtitle = "An example plot for this package",
    x = "Engine Shape",
    y = "Counts"
   ) +
   theme_unicef()
} # }
```
