#'
#' GNC colours
#'
#' @examples
#' gnc_green
#' gnc_grey
#' gnc_sage
#'
#' @rdname gnc_colours
#' @export
#'
gnc_green <- "#9CCB38"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_grey  <- "#808080"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_sage <- "#94A17F"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_dark_green <- "#63A537"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_lime <- "#37A76F"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_aqua_green <- "#44C1A3"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_cyan <- "#4Eb3CF"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_blue <- "#51C3F9"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_orange <- "#EE7B08"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_moss_green <- "#455F51"

#'
#' @rdname gnc_colours
#' @export
#' 
gnc_gold <- "#89824E"


#'
#' GNC palettes
#'
#' @examples
#' gnc_palettes
#'
#' @rdname gnc_palette
#' @export
#'
gnc_palettes <- list(
  gnc_primary = gnc_green,
  gnc_secondary = c(
    gnc_grey, gnc_sage, gnc_dark_green, gnc_lime, gnc_aqua_green, gnc_cyan, 
    gnc_blue, gnc_orange, gnc_moss_green, gnc_gold
  ),
  gnc_greens = c(
    "#F5F9EB", "#EBF4D7", "#E1EFC3", "#D7EAAF", "#CDE59B", 
    "#C3DF87", "#B9DA73", "#AFD55F", "#A5D04B", "#9CCB38"
  ),
  gnc_greys = c(
    "#F2F2F2", "#E5E5E5", "#D8D8D8", "#CCCCCC", "#BFBFBF", 
    "#B2B2B2", "#A6A6A6", "#999999", "#8C8C8C", "#808080"
  ),
  gnc_sages = c(
    "#F4F5F2", "#E9ECE5", "#DEE2D8", "#D4D9CB", "#C9D0BF",
    "#BEC6B2", "#B4BDA5", "#A9B398", "#9EAA8B", "#94A17F"
  ),
  gnc_dark_greens = c(
    "#DFEDD7", "#C0DBAF", "#A1C987", "#82B75F", "#63A537"
  ),
  gnc_limes = c(
    "#D7EDE2", "#AFDBC5", "#87CAA8", "#5FB88B", "#37A76F"
  ),
  gnc_aqua_greens = c(
    "#D9F2EC", "#B4E6DA", "#8ED9C7", "#69CDB5", "#44C1A3"
  ),
  gnc_cyans = c(
    "#DBEFF5", "#B8E0EB", "#94D1E2", "#71C2D8", "#4EB3CF"
  ),
  gnc_blues = c(
    "#DCF3FD", "#B9E7FC", "#96DBFB", "#73CFFA", "#51C3F9"
  ),
  gnc_oranges = c(
    "#FBE4CD", "#F8CA9C", "#F4AF6A", "#F19539", "#EE7B08"
  ),
  gnc_moss_greens = c(
    "#D9DFDC", "#B4BFB9", "#8F9F96", "#6A7F73", "#455F51"
  ),
  gnc_golds = c(
    "#E7E6DB", "#CFCDB8", "#B8B494", "#A09B71", "#89824E"
  )
)


#'
#' GNC fonts
#'
#' @examples
#' gnc_fonts
#'
#' @rdname gnc_font
#' @export
#'

gnc_fonts <- list(
  gnc_arial = "Arial",
  gnc_calibri = "Calibri Regular",
  acdc_source_sans_pro = "Source Sans Pro",
  acdc_bebas_neue = "Bebas Neue"
)


#'
#' Set GNC font to use based on what is available from the system
#'
#' The function will search the system for availability of any of the GNC fonts 
#' in hierarchical order starting with *Arial*, then *Calibri Regular*, then
#' *Source Sans Pro*, and then finally *Arial*. If none of these are found in 
#' the system, the function will return *Noto Sans* by default or the user can 
#' set which font to use as alternative by specifying `alt`.
#'
#' @param alt A character value for font family to use if all of the GNC fonts 
#'   are not available in the system.
#'
#' @returns A character value for font family to use as GNC font.
#'
#' @examples
#' set_gnc_font()
#'
#' @rdname gnc_font
#' @export
#'

set_gnc_font <- function(alt = paleta_fonts$paleta_noto) {
  ## Detect which fonts are available to the system ----
  fonts <- systemfonts::system_fonts()

  ## Check which GNC font is available ----
  if (any(fonts$family == gnc_fonts$gnc_arial)) {
    gnc_font <- gnc_fonts$gnc_arial
  } else {
    if (any(fonts$family == gnc_fonts$gnc_calibri)) {
      gnc_font <- gnc_fonts$gnc_calibri
    } else {
      if (any(fonts$family == gnc_fonts$gnc_source_sans_pro)) {
          gnc_font <- gnc_fonts$gnc_source_sans_pro
      } else {
        if (any(fonts$family == gnc_fonts$gnc_bebas_neue)) {
          gnc_font <- gnc_fonts$gnc_bebas_neue
        } else {
          gnc_font <- alt
        }
      }
    }
  }

  ## Return gnc_font ----
  gnc_font
}


#'
#' A [ggplot2] theme using GNC fonts, colours, and palettes
#'
#' These are wrappers for [theme_paleta()] that use colours and fonts from the
#' GNC visual identity guidelines.
#'
#' @section Colours:
#' The GNC theme is based on the colours from the [gnc_palettes]. The
#' primary palette consists of one colour: `gnc_palettes$gnc_primary`.
#' The secondary palette consists of ten colours:
#' `gnc_palettes$gnc_secondary`.
#'
#' @section Fonts:
#' The GNC theme uses four fonts as prescribed by the GNC visual identity
#' guidelines. These fonts (in hierarchical order of preference) are
#' *Arial*, *Calibri*, *Source Sans Pro*, and *Bebas Neue*. Any or all of these
#' fonts should be available in the user's system for them to be used in the
#' theme. If none of these fonts are available in the user's system, a freely
#' downloadable alternative called *Noto Sans* is the default fallback font and
#' can be obtained from [Google Fonts](https://fonts.google.com/).
#'
#' @param base_family Base font family using GNC fonts. Default is set
#'   by what GNC font is available in the system via [set_gnc_font()].
#'   If none of the GNC fonts are available, the default becomes
#'   *Noto Sans*.
#' @param base_size Base font size. Default is 11.5.
#' @param plot_title_family Font family to use for the plot title. Default is
#'   `base_family`.
#' @param plot_title_colour Colour of the plot title text. Default
#'   is [gnc_grey].
#' @param subtitle_family Font family to use for the plot subtitle. Default is
#'   `base_family`.
#' @param subtitle_colour Colour of the subtitle text. Default is
#'   [gnc_grey].
#' @param caption_colour Colour of the caption text. Default is
#'   [gnc_grey].
#' @param axis_title_colour Colour of the axis title text. Default is
#'   [gnc_grey].
#' @param legend_title_colour Colour of the legend title text. Default is NULL.
#' @param legend_text_colour Colour of the legend text. Default is NULL.
#' @param grid_col Grid colour. Default to [gnc_grey].
#' @param axis_col Axis colours. Default to [gnc_grey].
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
#'     values = gnc_palettes$gnc_secondary
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
theme_gnc <- function(base_family = set_gnc_font(),
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