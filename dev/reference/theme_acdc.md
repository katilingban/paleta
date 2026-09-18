# A [ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html) theme using Africa CDC fonts, colours, and palettes

These are wrappers for
[`theme_paleta()`](http://katilingban.io/paleta/dev/reference/theme_paleta.md)
that use colours and fonts from the Africa CDC visual identity
guidelines.

## Usage

``` r
theme_acdc_light(
  base_family = set_acdc_font(),
  base_size = 11.5,
  plot_title_family = base_family,
  plot_title_colour = acdc_text,
  subtitle_family = base_family,
  subtitle_colour = acdc_text,
  caption_colour = acdc_text,
  axis_title_colour = acdc_text,
  legend_title_colour = acdc_text,
  legend_text_colour = acdc_text,
  grid_col = acdc_gold,
  grid = TRUE,
  axis_col = acdc_gold,
  axis = FALSE,
  ticks = FALSE
)

theme_acdc_dark(
  base_family = set_acdc_font(),
  base_size = 11.5,
  plot_title_family = base_family,
  plot_title_colour = acdc_white,
  subtitle_family = base_family,
  subtitle_colour = acdc_white,
  caption_colour = acdc_white,
  axis_title_colour = acdc_white,
  legend_title_colour = acdc_white,
  legend_text_colour = acdc_white,
  plot_background_fill = tint_colour(acdc_gold, 0.8),
  grid_col = acdc_green,
  grid = TRUE,
  axis_col = acdc_green,
  axis = FALSE,
  ticks = FALSE
)
```

## Arguments

- base_family:

  Base font family using Africa CDC fonts. Default is set by what Africa
  CDC font is available in the system via
  [`set_acdc_font()`](http://katilingban.io/paleta/dev/reference/acdc_font.md).
  If none of the Africa CDC fonts are available, the default becomes
  Noto Sans.

- base_size:

  Base font size. Default is 11.5.

- plot_title_family:

  Font family to use for the plot title. Default is `base_family`.

- plot_title_colour:

  Colour of the plot title text. Default is
  [acdc_text](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- subtitle_family:

  Font family to use for the plot subtitle. Default is `base_family`.

- subtitle_colour:

  Colour of the subtitle text. Default is
  [acdc_text](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- caption_colour:

  Colour of the caption text. Default is
  [acdc_text](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- axis_title_colour:

  Colour of the axis title text. Default is
  [acdc_text](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- legend_title_colour:

  Colour of the legend title text. Default is
  [acdc_text](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- legend_text_colour:

  Colour of the legend text. Default is
  [acdc_text](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- grid_col:

  Grid colour. Default to
  [acdc_gold](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- grid:

  Panel grid. Either `TRUE`, `FALSE`, or a combination of `X` (major x
  grid), `x` (minor x grid), `Y` (major y grid), and/or `y` (minor y
  grid). Default is TRUE.

- axis_col:

  Axis colours. Default to
  [acdc_gold](http://katilingban.io/paleta/dev/reference/acdc_colours.md).

- axis:

  Add x or y axes? `TRUE`, `FALSE`, "`xy`". Default is FALSE.

- ticks:

  Logical. Should ticks be added? Default is FALSE.

- plot_background_fill:

  Fill colour for the plot background. Default is NULL.

## Value

A
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
theme.

## Colours

The Africa CDC theme is based on the colours from the
[acdc_palettes](http://katilingban.io/paleta/dev/reference/acdc_palette.md).
The primary palette consists of three colours:
`acdc_palettes$acdc_primary`. The secondary palette consists of eleven
colours: `acdc_palettes$acdc_secondary`.

## Fonts

The Africa CDC theme uses one or up to two fonts from the four fonts
prescribed by the Africa CDC visual identity guidelines. These fonts (in
hierarchical order of preference) are *Univers*, *Din Next Arabic*,
and/or *Arial*. Any or all of these fonts should be available in the
user's system for them to be used in the theme. If none of these fonts
are available in the user's system, a freely downloadable alternative
called *Noto Sans* is the default fallback font and can be obtained from
[Google Fonts](https://fonts.google.com/).

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
    values = acdc_palettes$acdc_secondary
  ) +
  labs(
    title = "Engine shape by number of cylinders",
    subtitle = "An example plot for this package",
    x = "Engine Shape",
    y = "Counts"
   ) +
   theme_acdc_light()
} # }
```
