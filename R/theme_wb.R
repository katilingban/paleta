#'
#' World Bank colours
#'
#' @examples
#' wb_blue
#' wb_cyan
#'
#' @rdname wb_colours
#' @export
#'
wb_blue          <- "#002244"

#' @rdname wb_colours
#' @export
wb_cyan          <- "#009FDA"

#' @rdname wb_colours
#' @export
wb_black         <- "#000000"

#' @rdname wb_colours
#' @export
wb_white         <- "#FFFFFF"

#' @rdname wb_colours
#' @export
wb_bright_orange <- "#F05023"

#' @rdname wb_colours
#' @export
wb_bright_yellow <- "#FDB714"

#' @rdname wb_colours
#' @export
wb_bright_red    <- "#EB1C2D"

#' @rdname wb_colours
#' @export
wb_light_orange  <- "#F78D28"

#' @rdname wb_colours
#' @export
wb_bright_aqua   <- "#009CA7"

#' @rdname wb_colours
#' @export
wb_bright_green  <- "#00AB51"

#' @rdname wb_colours
#' @export
wb_bright_purple <- "#872B90"

#' @rdname wb_colours
#' @export
wb_light_aqua    <- "#00A996"

#' @rdname wb_colours
#' @export
wb_dark_red      <- "#98252B"

#' @rdname wb_colours
#' @export
wb_dark_orange   <- "#E16A2D"

#' @rdname wb_colours
#' @export
wb_brown         <- "#B88C1D"

#' @rdname wb_colours
#' @export
wb_dark_purple   <- "#614776"

#' @rdname wb_colours
#' @export
wb_dark_aqua     <- "#006068"

#' @rdname wb_colours
#' @export
wb_dark_green    <- "#006450"


#'
#' World Bank palettes
#'
#' @examples
#' wb_palettes
#'
#' @rdname wb_palette
#' @export
#'
wb_palettes <- list(
  wb_primary     = c("#002244", "#009FDA", "#000000", "#FFFFFF"),
  wb_secondary   = c(
    "#F05023", "#FDB714", "#EB1C2D", "#F78D28", "#009CA7", "#00AB51", "#872B90",
    "#00A996", "#98252B", "#E16A2D", "#B88C1D", "#614776", "#006068", "#006450"
  ),
  wb_brights = c(
    "#F05023", "#FDB714", "#EB1C2D", "#F78D28", "#009CA7", "#00AB51", "#872B90",
    "#00A996"
  ),
  wb_neutrals = c(
    "#98252B", "#E16A2D", "#B88C1D", "#614776", "#006068", "#006450"
  ),
  wb_bright_oranges = c("#F05023", "#F3734F", "#F6967B", "#F9B9A7", "#FCDCD3"),
  wb_bright_yellows = c("#FDB714", "#FDC543", "#FDD372", "#FEE2A1", "#FEF0D0"),
  wb_bright_reds = c("#EB1C2D", "#EF4957", "#F37681", "#F7A4AB", "#FBD1D5"),
  wb_light_oranges = c("#F78D28", "#F8A353", "#FABA7E", "#FBD1A9", "#FDE8D4"),
  wb_bright_aquas = c("#009CA7", "#33AFB8", "#66C3CA", "#99D7DB", "#CCEBED"),
  wb_bright_greens = c("#00AB51", "#33BB73", "#66CC96", "#99DDB9", "#CCEEDC"),
  wb_bright_purples = c("#872B90", "#9F55A6", "#B77FBC", "#CFAAD2", "#E7D4E8"),
  wb_light_aquas = c("#00A996", "#33BAAB", "#66CBC0", "#99DCD5", "#CCEDEA"),
  wb_dark_reds = c("#98252B", "#AC5055", "#C17C7F", "#D5A7AA", "#EAD3D4"),
  wb_dark_oranges = c("#E16A2D","#E78757", "#EDA581", "#F3C3AB", "#F9E1D5"),
  wb_browns = c("#B88C1D", "#C6A34A", "#D4BA77", "#E2D1A4", "#F0E8D1"),
  wb_dark_purples = c("#614776", "#806B91", "#A090AC", "#BFB5C8", "#DFDAE3"),
  wb_dark_aquas = c("#006068", "#337F86", "#669FA4", "#99BFC2", "#CCDFE0"),
  wb_dark_greens = c("#006450", "#338373", "#66A296", "#99C1B9", "#CCE0DC")
)


#'
#' Africa CDC fonts
#'
#' @examples
#' wb_fonts
#'
#' @rdname wb_font
#' @export
#'
wb_fonts <- list(
  wb_andes = "Andes",
  wb_brandon = "Arial"
)

#'
#' Set World Bank font to use based on what is available from the system
#'
#' The function will search the system for availability of any of the World Bank
#' fonts in heirarchical order starting with *Andes*, and then *Arial*. If
#' none of these are found in the system, the function will return *Noto Sans*
#' by default or the user can set which font to use as alternative by specifying
#' `alt`.
#'
#' @param alt A character value for font family to use if all of the World Bank
#'   fonts are not available in the system.
#'
#' @returns A character value for font family to use as World Bank font.
#'
#' @examples
#' set_wb_font()
#'
#' @rdname wb_font
#' @export
#'

set_wb_font <- function(alt = paleta_fonts$paleta_noto) {
  ## Detect which fonts are available to the system ----
  fonts <- systemfonts::system_fonts()

  ## Check which World Bank font is available ----
  if (any(fonts$family == wb_fonts$wb_andes)) {
    wb_font <- wb_fonts$wb_andes
  } else {
    if (any(fonts$family == wb_fonts$wb_arial)) {
      wb_font <- wb_fonts$wb_arial
    } else {
      wb_font <- alt
    }
  }

  ## Return wb_font ----
  wb_font
}


#'
#' A [ggplot2] theme using World Bank fonts, colours, and palettes
#'
#' These are wrappers for `theme_paleta()` that use colours and fonts from the
#' World Bank visual identity guidelines.
#'
#' @section Colours:
#' The World Bank theme is based on the colours from the `wb_palettes`. The
#' primary palette consists of four colours: `wb_palettes$wb_primary`. The
#' secondary palette consists of fourteen colours: `wb_palettes$wb_secondary`.
#'
#' @section Fonts:
#' The World Bank theme uses two fonts as prescribed by the World Bank visual
#' identity guidelines. These fonts (in hierarchical order of preference) are
#' *Andes* and *Arial*. Any or all of these fonts should be available in
#' the user's system for them to be used in the theme. If none of these fonts
#' are available in the user's system, a freely downloadable alternative called
#' *Noto Sans* is the default fallback font and can be obtained from
#' [Google Fonts](https://fonts.google.com/).
#'
#' @param base_family Base font family using World Bank fonts. Default is set
#'   by what World Bank font is available in the system via `set_wb_font()`.
#'   If none of the World Bank fonts are available, the default becomes
#'   *Noto Sans*.
#' @param plot_title_family Font family to use for the plot title. Default is
#'   `base_family`.
#' @param plot_title_colour Colour of the plot title text. Default
#'   is `wb_blue`.
#' @param subtitle_family Font family to use for the plot subtitle. Default is
#'   `base_family`.
#' @param subtitle_colour Colour of the subtitle text. Default is `wb_cyan`.
#' @param caption_colour Colour of the caption text. Default is `wb_cyan`.
#' @param axis_title_colour Colour of the axis title text. Default is
#'   `wb_cyan`.
#' @param legend_title_colour Colour of the legend title text. Default is NULL.
#' @param legend_text_colour Colour of the legend text. Default is NULL.
#' @param grid_col Grid colour. Default to `wb_cyan`.
#' @param axis_col Axis colours. Default to `wb_cyan`.
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
#'     values = wb_palettes$wb_secondary
#'   ) +
#'   labs(
#'     title = "Engine shape by number of cylinders",
#'     subtitle = "An example plot for this package",
#'     x = "Engine Shape",
#'     y = "Counts"
#'    ) +
#'    theme_wb()
#' }
#'
#' @rdname theme_wb
#' @export
#'
theme_wb <- function(base_family = set_wb_font(),
                     plot_title_family = base_family,
                     plot_title_colour = wb_blue,
                     subtitle_family = base_family,
                     subtitle_colour = wb_cyan,
                     caption_colour = wb_blue,
                     axis_title_colour = wb_blue,
                     legend_title_colour = wb_blue,
                     legend_text_colour = wb_blue,
                     grid_col = wb_cyan,
                     grid = TRUE,
                     axis_col = wb_cyan,
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


