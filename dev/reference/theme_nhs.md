# A [ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html) theme using NHS fonts, colours, and palettes

These are wrappers for
[`theme_paleta()`](http://katilingban.io/paleta/dev/reference/theme_paleta.md)
that use colours and fonts from the NHS visual identity guidelines.

## Usage

``` r
theme_nhs(
  base_family = set_nhs_font(),
  base_size = 11.5,
  plot_title_family = base_family,
  plot_title_colour = nhs_blue,
  subtitle_family = base_family,
  subtitle_colour = nhs_dark_grey,
  caption_colour = nhs_dark_grey,
  axis_title_colour = nhs_dark_grey,
  legend_title_colour = nhs_dark_grey,
  legend_text_colour = nhs_dark_grey,
  grid_col = nhs_mid_grey,
  grid = TRUE,
  axis_col = nhs_mid_grey,
  axis = FALSE,
  ticks = FALSE
)
```

## Arguments

- base_family:

  Base font family using NHS fonts. Default is set by what NHS font is
  available in the system via
  [`set_nhs_font()`](http://katilingban.io/paleta/dev/reference/nhs_font.md).
  If none of the NHS fonts are available, the default becomes *Noto
  Sans*.

- base_size:

  Base font size. Default is 11.5.

- plot_title_family:

  Font family to use for the plot title. Default is `base_family`.

- plot_title_colour:

  Colour of the plot title text. Default is
  [nhs_blue](http://katilingban.io/paleta/dev/reference/nhs_colours.md).

- subtitle_family:

  Font family to use for the plot subtitle. Default is \`base_family“.

- subtitle_colour:

  Colour of the subtitle text. Default is
  [nhs_mid_grey](http://katilingban.io/paleta/dev/reference/nhs_colours.md).

- caption_colour:

  Colour of the caption text. Default is
  [nhs_mid_grey](http://katilingban.io/paleta/dev/reference/nhs_colours.md).

- axis_title_colour:

  Colour of the axis title text. Default is
  [nhs_mid_grey](http://katilingban.io/paleta/dev/reference/nhs_colours.md).

- legend_title_colour:

  Colour of the legend title text. Default is NULL.

- legend_text_colour:

  Colour of the legend text. Default is NULL.

- grid_col:

  Grid colour. Default to
  [nhs_pale_grey](http://katilingban.io/paleta/dev/reference/nhs_colours.md).

- grid:

  Panel grid. Either `TRUE`, `FALSE`, or a combination of `X` (major x
  grid), `x` (minor x grid), `Y` (major y grid), and/or `y` (minor y
  grid). Default is TRUE.

- axis_col:

  Axis colours. Default to
  [nhs_pale_grey](http://katilingban.io/paleta/dev/reference/nhs_colours.md).

- axis:

  Add x or y axes? `TRUE`, `FALSE`, "`xy`". Default is FALSE.

- ticks:

  Logical. Should ticks be added? Default is FALSE.

## Value

A
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
theme.

## Colours

The NHS theme is based on the colours from the
[nhs_palettes](http://katilingban.io/paleta/dev/reference/nhs_palette.md).
The primary palette consists of two colours: `nhs_palettes$nhs_primary`.
The secondary palette consists of nineteen colours:.

## Fonts

The NHS theme uses two fonts as prescribed by the NHS visual identity
guidelines. These fonts (in hierarchical order of preference) are
*Frutiger* and *Arial*. Any or all of these fonts should be available in
the user's system for them to be used in the theme. If none of these
fonts are available in the user's system, a freely downloadable
alternative called *Noto Sans* is the default fallback font and can be
obtained from [Google Fonts](https://fonts.google.com/).

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
    values = nhs_palettes$nhs_support_greens
  ) +
  labs(
    title = "Engine shape by number of cylinders",
    subtitle = "An example plot for this package",
    x = "Engine Shape",
    y = "Counts"
   ) +
   theme_nhs()
} # }
```
