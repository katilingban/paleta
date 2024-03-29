#'
#' Africa CDC colours
#'
#' @examples
#' acdc_green
#' acdc_gold
#' acdc_brown
#'
#' @rdname acdc_colours
#' @export
#'
acdc_green      <- "#348F41"

#' @rdname acdc_colours
#' @export
acdc_gold       <- "#B4A269"

#' @rdname acdc_colours
#' @export
acdc_black      <- "#000000"

#' @rdname acdc_colours
#' @export
acdc_brown      <- "#6B4C24"

#' @rdname acdc_colours
#' @export
acdc_yellow     <- "#E08F2A"

#' @rdname acdc_colours
#' @export
acdc_orange     <- "#C45B39"

#' @rdname acdc_colours
#' @export
acdc_red        <- "#782C2D"

#' @rdname acdc_colours
#' @export
acdc_dark_green <- "#4B5430"


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
  acdc_primary     = c("#348F41", "#B4A269", "#000000"),
  acdc_secondary   = c("#6B4C24", "#E08F2A", "#C45B39", "#782C2D", "#4B5430"),
  acdc_greens      = c("#348F41", "#5CA567", "#85BB8D", "#ADD2B3", "#D6E8D9"),
  acdc_golds       = c("#B4A269", "#C3B487", "#D2C7A5", "#E1D9C3", "#F0ECE1"),
  acdc_blacks      = c("#000000", "#333333", "#666666", "#999999", "#CCCCCC"),
  acdc_browns      = c("#6B4C24", "#886F4F", "#A6937B", "#C3B7A7", "#E1DBD3"),
  acdc_yellows     = c("#E08F2A", "#E6A554", "#ECBB7F", "#F2D2A9", "#F8E8D4"),
  acdc_oranges     = c("#C45B39", "#CF7B60", "#DB9C88", "#E7BDAF", "#F3DED7"),
  acdc_reds        = c("#782C2D", "#935657", "#AE8081", "#C9AAAB", "#E4D4D5"),
  acdc_dark_greens = c("#4B5430", "#6F7659", "#939882", "#B7BAAC", "#DBDCD5"),
  acdc_triad_1     = c("#E08F2A", "#C45B39", "#782C2D"),
  acdc_triad_2     = c("#782C2D", "#E08F2A", "#348F41"),
  acdc_triad_3     = c("#782C2D", "#E08F2A", "#6B4C24"),
  acdc_triad_3     = c("#782C2D", "#C45B39", "#6B4C24"),
  acdc_triad_4     = c("#E08F2A", "#348F41", "#4B5430")
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
  acdc_acumin = "Acumin Pro",
  acdc_brandon = "Brandon Text",
  acdc_calibri = "Calibri",
  acdc_arial = "Arial"
)

#'
#' Set ACDC font to use based on what is available from the system
#'
#' The function will search the system for availability of any of the Africa
#' CDC fonts in heirarchical order starting with *Acumin Pro*, then
#' *Brandon Text*, then *Calibri*, and then finally *Arial*. If none of these
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
  if (any(fonts$family == acdc_fonts$acdc_acumin)) {
    acdc_font <- acdc_fonts$acdc_acumin
  } else {
    if (any(fonts$family == acdc_fonts$acdc_brandon)) {
      acdc_font <- acdc_fonts$acdc_brandon
    } else {
      if (any(fonts$family == acdc_fonts$acdc_calibri)) {
        acdc_font <- acdc_fonts$acdc_calibri
      } else {
        if (any(fonts$family == acdc_fonts$acdc_arial)) {
          acdc_font <- acdc_fonts$acdc_arial
        } else {
          acdc_font <- alt
        }
      }
    }
  }

  ## Return acdc_font ----
  acdc_font
}


#'
#' A [ggplot2] theme using Africa CDC fonts, colours, and palettes
#'
#' These are wrappers for `theme_paleta()` that use colours and fonts from the
#' Africa CDC visual identity guidelines.
#'
#' @section Colours:
#' The Africa CDC theme is based on the colours from the `acdc_palettes`. The
#' primary palette consists of three colours: `acdc_palettes$acdc_primary`. The
#' secondary palette consists of five colours: `acdc_palettes$acdc_secondary`.
#'
#' @section Fonts:
#' The Africa CDC theme uses one or up to two fonts from the four fonts
#' prescribed by the Africa CDC visual identity guidelines. These fonts (in
#' hierarchical order of preference) are *Acumin Pro*, *Brandon Text*,
#' *Calibri*, and/or *Arial*. Any or all of these fonts should be available in
#' the user's system for them to be used in the theme. If none of these fonts
#' are available in the user's system, a freely downloadable alternative called
#' *Noto Sans* is the default fallback font and can be obtained from
#' [Google Fonts](https://fonts.google.com/).
#'
#' @param base_family Base font family using Africa CDC fonts. Default is set
#'   by what Africa CDC font is available in the system via `set_acdc_font()`.
#'   If none of the Africa CDC fonts are available, the default becomes Noto
#'   Sans.
#' @param base_size Base font size. Default is 11.5.
#' @param plot_title_family Font family to use for the plot title. Default is
#'   `base_family`.
#' @param plot_title_colour Colour of the plot title text. Default
#'   is `acdc_green`.
#' @param subtitle_family Font family to use for the plot subtitle. Default is
#'   `base_family`.
#' @param subtitle_colour Colour of the subtitle text. Default is `acdc_gold`.
#' @param caption_colour Colour of the caption text. Default is `acdc_gold`.
#' @param axis_title_colour Colour of the axis title text. Default is
#'   `acdc_gold`.
#' @param legend_title_colour Colour of the legend title text. Default is NULL.
#' @param legend_text_colour Colour of the legend text. Default is NULL.
#' @param plot_background_fill Fill colour for the plot background. Default is
#'   NULL.
#' @param grid_col Grid colour. Default to `acdc_gold`.
#' @param axis_col Axis colours. Default to `acdc_gold`.
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
                             plot_title_colour = acdc_green,
                             subtitle_family = base_family,
                             subtitle_colour = acdc_gold,
                             caption_colour = acdc_gold,
                             axis_title_colour = acdc_gold,
                             legend_title_colour = acdc_gold,
                             legend_text_colour = acdc_gold,
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
                            plot_title_colour = acdc_green,
                            subtitle_family = base_family,
                            subtitle_colour = acdc_black,
                            caption_colour = acdc_black,
                            axis_title_colour = acdc_black,
                            legend_title_colour = acdc_black,
                            legend_text_colour = acdc_black,
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


