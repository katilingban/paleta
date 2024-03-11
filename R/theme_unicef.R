                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        #'
#' UNICEF colours
#'
#' @examples
#' unicef_blue
#' unicef_green
#'
#' @rdname unicef_colours
#' @export
#'
unicef_blue          <- "#1CABE2"

#' @rdname unicef_colours
#' @export
unicef_green         <- "#00833D"

#' @rdname unicef_colours
#' @export
unicef_lime_green    <- "#80BD41"

#' @rdname unicef_colours
#' @export
unicef_yellow        <- "#FFC20E"

#' @rdname unicef_colours
#' @export
unicef_orange        <- "#F26A21"

#' @rdname unicef_colours
#' @export
unicef_bright_red    <- "#E2231A"

#' @rdname unicef_colours
#' @export
unicef_dark_red      <- "#961A49"

#' @rdname unicef_colours
#' @export
unicef_purple        <- "#6A1E74"

#' @rdname unicef_colours
#' @export
unicef_warm_grey     <- "#D8D1C9"

#' @rdname unicef_colours
#' @export
unicef_cool_grey     <- "#777779"

#' @rdname unicef_colours
#' @export
unicef_black         <- "#2D2926"

#' @rdname unicef_colours
#' @export
unicef_dark_blue     <- "#374EA2"


#'
#' UNICEF palettes
#'
#' @examples
#' unicef_palettes
#'
#' @rdname unicef_palette
#' @export
#'
unicef_palettes <- list(
  unicef_primary     = "#1CABE2",
  unicef_secondary   = c(
    "#00833D", "#80BD41", "#FFC20E", "#F26A21", "#E2231A", "#961A49",
    "#6A1E74", "#D8D1C9", "#777779", "#2D2926", "#374EA2"
  ),
  unicef_brights = c("#00833D", "#80BD41", "#FFC20E", "#F26A21", "#E2231A"),
  unicef_neutrals = c(
    "#961A49", "#6A1E74", "#D8D1C9", "#777779", "#2D2926", "#374EA2"
  )
)


#'
#' UNICEF fonts
#'
#' @examples
#' unicef_fonts
#'
#' @rdname unicef_font
#' @export
#'
unicef_fonts <- list(
  unicef_univers = "Univers LT Pro",
  unicef_arial = "Arial",
  unicef_roboto = "Roboto",
  unicef_aleo = "Aleo"
)

#'
#' Set UNICEF font to use based on what is available from the system
#'
#' The function will search the system for availability of any of the UNICEF
#' fonts in heirarchical order starting with *Andes*, and then *Arial*. If
#' none of these are found in the system, the function will return *Noto Sans*
#' by default or the user can set which font to use as alternative by specifying
#' `alt`.
#'
#' @param alt A character value for font family to use if all of the UNICEF
#'   fonts are not available in the system.
#'
#' @returns A character value for font family to use as UNICEF font.
#'
#' @examples
#' set_unicef_font()
#'
#' @rdname unicef_font
#' @export
#'

set_unicef_font <- function(alt = paleta_fonts$paleta_noto) {
  ## Detect which fonts are available to the system ----
  fonts <- systemfonts::system_fonts()

  ## Check which UNICEF font is available ----
  if (any(fonts$family == unicef_fonts$unicef_univers)) {
    unicef_font <- unicef_fonts$unicef_univers
  } else {
    if (any(fonts$family == unicef_fonts$unicef_arial)) {
      unicef_font <- unicef_fonts$unicef_arial
    } else {
      if (any(fonts$family == unicef_fonts$unicef_roboto)) {
        unicef_font <- unicef_fonts$unicef_roboto
      } else {
        unicef_font <- alt
      }
    }
  }

  ## Return unicef_font ----
  unicef_font
}


#'
#' A [ggplot2] theme using UNICEF fonts, colours, and palettes
#'
#' These are wrappers for `theme_paleta()` that use colours and fonts from the
#' UNICEF visual identity guidelines.
#'
#' @section Colours:
#' The UNICEF theme is based on the colours from the `unicef_palettes`. The
#' primary palette consists of one colour: `unicef_palettes$unicef_primary`.
#' The secondary palette consists of eleven colours:
#' `unicef_palettes$unicef_secondary`.
#'
#' @section Fonts:
#' The UNICEF theme uses two fonts as prescribed by the UNICEF visual identity
#' guidelines. These fonts (in hierarchical order of preference) are
#' *Univers LT Pro*, *Arial*, *Roboto*, and *Aleo*. Any or all of these fonts
#' should be available in the user's system for them to be used in the theme.
#' If none of these fonts are available in the user's system, a freely
#' downloadable alternative called *Noto Sans* is the default fallback font and
#' can be obtained from [Google Fonts](https://fonts.google.com/).
#'
#' @param base_family Base font family using UNICEF fonts. Default is set
#'   by what UNICEF font is available in the system via `set_unicef_font()`.
#'   If none of the UNICEF fonts are available, the default becomes
#'   *Noto Sans*.
#' @param base_size Base font size. Default is 11.5.
#' @param plot_title_family Font family to use for the plot title. Default is
#'   `base_family`.
#' @param plot_title_colour Colour of the plot title text. Default
#'   is `unicef_black`.
#' @param subtitle_family Font family to use for the plot subtitle. Default is
#'   `base_family`.
#' @param subtitle_colour Colour of the subtitle text. Default is
#'   `unicef_cool_grey`.
#' @param caption_colour Colour of the caption text. Default is
#'   `unicef_cool_grey`.
#' @param axis_title_colour Colour of the axis title text. Default is
#'   `unicef_cool_grey`.
#' @param legend_title_colour Colour of the legend title text. Default is NULL.
#' @param legend_text_colour Colour of the legend text. Default is NULL.
#' @param grid_col Grid colour. Default to `unicef_warm_grey`.
#' @param axis_col Axis colours. Default to `unicef_warm_grey`.
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
#'     values = unicef_palettes$unicef_secondary
#'   ) +
#'   labs(
#'     title = "Engine shape by number of cylinders",
#'     subtitle = "An example plot for this package",
#'     x = "Engine Shape",
#'     y = "Counts"
#'    ) +
#'    theme_unicef()
#' }
#'
#' @rdname theme_unicef
#' @export
#'
theme_unicef <- function(base_family = set_unicef_font(),
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
                         ticks = FALSE) {
  theme_paleta(
    base_family = base_family,
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


