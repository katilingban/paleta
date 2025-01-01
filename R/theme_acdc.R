#'
#' Africa CDC colours
#'
#' @examples
#' acdc_green
#' acdc_red
#' acdc_gold
#'
#' @rdname acdc_colours
#' @export
#'
acdc_green           <- "#348F41"

#' @rdname acdc_colours
#' @export
acdc_red             <- "#9F2241"

#' @rdname acdc_colours
#' @export
acdc_gold            <- "#B4A269"

#' @rdname acdc_colours
#' @export
acdc_white           <- "#FFFFFF"

#' @rdname acdc_colours
#' @export
acdc_grey            <- "#58595B"

#' @rdname acdc_colours
#' @export
acdc_corporate_green <- "#1A5632"

#' @rdname acdc_colours
#' @export
acdc_text            <- "#53575A"

#' @rdname acdc_colours
#' @export
acdc_blue            <- "#194F90"

#' @rdname acdc_colours
#' @export
acdc_plum            <- "#AE1857"

#' @rdname acdc_colours
#' @export
acdc_blue_grey       <- "#5B7E96"

#' @rdname acdc_colours
#' @export
acdc_amber           <- "#FFB71B"

#' @rdname acdc_colours
#' @export
acdc_cyan            <- "#1DCAD3"

#' @rdname acdc_colours
#' @export
acdc_deep_orange     <- "#FF5C35"

#' @rdname acdc_colours
#' @export
acdc_purple          <- "#8F4899"

#' @rdname acdc_colours
#' @export
acdc_lime            <- "#DAE343"

#' @rdname acdc_colours
#' @export
acdc_mauve           <- "#385CAD"

#' @rdname acdc_colours
#' @export
acdc_pink            <- "#E81F76"

#' @rdname acdc_colours
#' @export
acdc_teal            <- "#009383"


#'
#' Africa CDC palettes
#'
#' @examples
#' acdc_palettes
#'
#' @rdname acdc_palette
#' @export
#'
acdc_palettes <- list(
  acdc_primary = c(acdc_green, acdc_red, acdc_gold),
  acdc_secondary = c(
    acdc_blue, acdc_plum, acdc_blue_grey, acdc_amber, acdc_cyan, 
    acdc_deep_orange, acdc_purple, acdc_lime, acdc_mauve, acdc_pink, acdc_teal
  ),
  acdc_blues        = c("#D1DBE8", "#A3B8D2", "#7595BC", "#4772A6", acdc_blue),
  acdc_plums        = c("#EED0DD", "#DEA2BB", "#CE749A", "#BE4678", acdc_plum),
  acdc_blue_greys   = c("#DEE5EA", "#BDCBD5", "#9CB1C0", "#7B97AB", acdc_blue_grey),
  acdc_ambers       = c("#FFF0D1", "#FFE2A3", "#FFD376", "#FFC548", acdc_amber),
  acdc_cyans        = c("#D1F4F6", "#A4E9ED", "#77DFE4", "#4AD4DB", acdc_cyan),
  acdc_deep_oranges = c("#FFDED6", "#FFBDAE", "#FF9D85", "#FF7C5D", acdc_deep_orange),
  acdc_purples      = c("#E8DAEA", "#D2B5D6", "#BB91C1", "#A56CAD", acdc_purple),
  acdc_limes        = c("#F7F9D9", "#F0F3B3", "#E8EE8E", "#E1E868", acdc_lime),
  acdc_mauves       = c("#D7DEEE", "#AFBDDE", "#879DCD", "#5F7CBD", acdc_mauve),
  acdc_pinks        = c("#FAD2E3", "#F5A5C8", "#F178AC", "#EC4B91", acdc_pink),
  acdc_teals        = c("#CCE9E6", "#99D3CD", "#66BEB4", "#33A89B", acdc_teal)
)

#'
#' @examples
#' acdc_brewer_palettes
#'
#' @rdname acdc_palette
#' @export
#'
acdc_brewer_palettes <- list(
  blues  = rev(c(acdc_blue, acdc_blue_grey, acdc_palettes$acdc_blues[2])),
  bugn   = c(acdc_blue, acdc_lime, acdc_teal),
  bupu   = c(acdc_blue, acdc_blue_grey, acdc_purple),
  gnbu   = c(acdc_teal, acdc_lime, acdc_blue),
  pubu   = c(acdc_purple, acdc_blue_grey, acdc_blue),
  pubugn = c(acdc_purple, acdc_blue, acdc_lime),
  purd   = c(acdc_purple, acdc_plum, acdc_pink),
  rdpu   = c(acdc_red, acdc_pink, acdc_purple),
  ylgn   = c(acdc_amber, acdc_teal, acdc_green),
  ylgnbu = c(acdc_amber, acdc_lime, acdc_blue),
  ylorrd = c(acdc_amber, acdc_deep_orange, acdc_red),
  piylgn = c(acdc_pink, acdc_amber, acdc_lime),
  prgn   = c(acdc_purple, acdc_mauve, acdc_green),
  puor   = c(acdc_purple, acdc_amber, acdc_deep_orange),
  rdbu   = c(acdc_red, acdc_plum, acdc_blue),
  rdylbu = c(acdc_red, acdc_amber, acdc_blue),
  rdylgn = c(acdc_red, acdc_amber, acdc_lime),
  pastel1 = c(
    acdc_palettes$acdc_blues[3], 
    acdc_palettes$acdc_plums[3], 
    acdc_palettes$acdc_blue_grey[3], 
    acdc_palettes$acdc_amber[3], 
    acdc_palettes$acdc_cyan[3], 
    acdc_palettes$acdc_deep_orange[3], 
    acdc_palettes$acdc_purple[3], 
    acdc_palettes$acdc_lime[3], 
    acdc_palettes$acdc_mauve[3], 
    acdc_palettes$acdc_pink[3], 
    acdc_palettes$acdc_teal[3]
  ),
  pastel2 = c(
    acdc_palettes$acdc_blues[2], 
    acdc_palettes$acdc_plums[2], 
    acdc_palettes$acdc_blue_grey[2], 
    acdc_palettes$acdc_amber[2], 
    acdc_palettes$acdc_cyan[2], 
    acdc_palettes$acdc_deep_orange[2], 
    acdc_palettes$acdc_purple[2], 
    acdc_palettes$acdc_lime[2], 
    acdc_palettes$acdc_mauve[2], 
    acdc_palettes$acdc_pink[2], 
    acdc_palettes$acdc_teal[2]
  )
)


#'
#' Africa CDC fonts
#'
#' @examples
#' acdc_fonts
#'
#' @rdname acdc_font
#' @export
#'
acdc_fonts <- list(
  acdc_univers = "Univers",
  acdc_din_next_arabic = "Din Next Arabic",
  acdc_arial = "Arial"
)

#'
#' Set ACDC font to use based on what is available from the system
#'
#' The function will search the system for availability of any of the Africa
#' CDC fonts in hierarchical order starting with *Univers*, then
#' *Din Next Arabic*, and then finally *Arial*. If none of these
#' are found in the system, the function will return *Noto Sans* by default or
#' the user can set which font to use as alternative by specifying `alt`.
#'
#' @param alt A character value for font family to use if all of the Africa
#'   CDC fonts are not available in the system.
#'
#' @returns A character value for font family to use as Africa CDC font.
#'
#' @examples
#' set_acdc_font()
#'
#' @rdname acdc_font
#' @export
#'

set_acdc_font <- function(alt = paleta_fonts$paleta_noto) {
  ## Detect which fonts are available to the system ----
  fonts <- systemfonts::system_fonts()

  ## Check which Africa CDC font is available ----
  if (any(fonts$family == acdc_fonts$acdc_univers)) {
    acdc_font <- acdc_fonts$acdc_univers
  } else {
    if (any(fonts$family == acdc_fonts$acdc_din_next_arabic)) {
      acdc_font <- acdc_fonts$acdc_din_next_arabic
    } else {
      if (any(fonts$family == acdc_fonts$acdc_arial)) {
          acdc_font <- acdc_fonts$acdc_arial
      } else {
          acdc_font <- alt
      }
    }
  }

  ## Return acdc_font ----
  acdc_font
}


#'
#' A [ggplot2] theme using Africa CDC fonts, colours, and palettes
#'
#' These are wrappers for [theme_paleta()] that use colours and fonts from the
#' Africa CDC visual identity guidelines.
#'
#' @section Colours:
#' The Africa CDC theme is based on the colours from the [acdc_palettes]. The
#' primary palette consists of three colours: `acdc_palettes$acdc_primary`. The
#' secondary palette consists of eleven colours: `acdc_palettes$acdc_secondary`.
#'
#' @section Fonts:
#' The Africa CDC theme uses one or up to two fonts from the four fonts
#' prescribed by the Africa CDC visual identity guidelines. These fonts (in
#' hierarchical order of preference) are *Univers*, *Din Next Arabic*, and/or 
#' *Arial*. Any or all of these fonts should be available in the user's system 
#' for them to be used in the theme. If none of these fonts are available in the 
#' user's system, a freely downloadable alternative called *Noto Sans* is the 
#' default fallback font and can be obtained from
#' [Google Fonts](https://fonts.google.com/).
#'
#' @param base_family Base font family using Africa CDC fonts. Default is set
#'   by what Africa CDC font is available in the system via [set_acdc_font()].
#'   If none of the Africa CDC fonts are available, the default becomes Noto
#'   Sans.
#' @param base_size Base font size. Default is 11.5.
#' @param plot_title_family Font family to use for the plot title. Default is
#'   `base_family`.
#' @param plot_title_colour Colour of the plot title text. Default
#'   is `acdc_text`.
#' @param subtitle_family Font family to use for the plot subtitle. Default is
#'   `base_family`.
#' @param subtitle_colour Colour of the subtitle text. Default is [acdc_text].
#' @param caption_colour Colour of the caption text. Default is [acdc_text].
#' @param axis_title_colour Colour of the axis title text. Default is
#'   `acdc_text`.
#' @param legend_title_colour Colour of the legend title text. Default is
#'   `acdc_text`.
#' @param legend_text_colour Colour of the legend text. Default is [acdc_text].
#' @param plot_background_fill Fill colour for the plot background. Default is
#'   NULL.
#' @param grid_col Grid colour. Default to [acdc_gold].
#' @param axis_col Axis colours. Default to [acdc_gold].
#' @param grid Panel grid. Either `TRUE`, `FALSE`, or a combination of
#'   `X` (major x grid), `x` (minor x grid), `Y` (major y grid), and/or
#'   `y` (minor y grid). Default is TRUE.
#' @param axis Add x or y axes? `TRUE`, `FALSE`, "`xy`". Default is FALSE.
#' @param ticks Logical. Should ticks be added? Default is FALSE.
#'
#' @returns A [ggplot2] theme.
#'
#' @examples
#' \dontrun{
#'   ggplot(
#'     data = mtcars,
#'     mapping = aes(
#'       x = factor(vs, levels = c(0, 1), labels = c("v-shaped", "straight")),
#'       fill = factor(cyl))
#'   ) +
#'   geom_bar() +
#'   scale_fill_manual(
#'     name = "Cylinders",
#'     values = acdc_palettes$acdc_secondary
#'   ) +
#'   labs(
#'     title = "Engine shape by number of cylinders",
#'     subtitle = "An example plot for this package",
#'     x = "Engine Shape",
#'     y = "Counts"
#'    ) +
#'    theme_acdc_light()
#' }
#'
#' @rdname theme_acdc
#' @export
#'
theme_acdc_light <- function(base_family = set_acdc_font(),
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
                             ticks = FALSE) {
  theme_paleta(
    base_family = base_family,
    base_size = base_size,
    plot_title_family = plot_title_family,
    plot_title_colour = plot_title_colour,
    subtitle_family = subtitle_family,
    subtitle_colour = subtitle_colour,
    caption_colour = caption_colour,
    axis_title_colour = axis_title_colour,
    legend_title_colour = legend_title_colour,
    legend_text_colour = legend_text_colour,
    grid_col = grid_col,
    grid = grid,
    axis_col = axis_col,
    axis = axis,
    ticks = ticks
  )
}


#'
#' @rdname theme_acdc
#' @export
#'
theme_acdc_dark <- function(base_family = set_acdc_font(),
                            base_size = 11.5,
                            plot_title_family = base_family,
                            plot_title_colour = acdc_white,
                            subtitle_family = base_family,
                            subtitle_colour = acdc_white,
                            caption_colour = acdc_white,
                            axis_title_colour = acdc_white,
                            legend_title_colour = acdc_white,
                            legend_text_colour = acdc_white,
                            plot_background_fill = tint_colour(acdc_gold, 0.2),
                            grid_col = acdc_green,
                            grid = TRUE,
                            axis_col = acdc_green,
                            axis = FALSE,
                            ticks = FALSE) {
  theme_paleta(
    base_family = base_family,
    base_size = base_size,
    plot_title_family = plot_title_family,
    plot_title_colour = plot_title_colour,
    subtitle_family = subtitle_family,
    subtitle_colour = subtitle_colour,
    caption_colour = caption_colour,
    axis_title_colour = axis_title_colour,
    legend_title_colour = legend_title_colour,
    legend_text_colour = legend_text_colour,
    plot_background_fill = plot_background_fill,
    grid_col = grid_col,
    grid = grid,
    axis_col = axis_col,
    axis = axis,
    ticks = ticks
  )
}
