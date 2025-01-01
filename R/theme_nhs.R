#'
#' NHS colours
#'
#' @examples
#' nhs_blue
#' nhs_dark_blue
#'
#' @rdname nhs_colours
#' @export
#'
nhs_blue        <- "#005EB8"

#' @rdname nhs_colours
#' @export
nhs_white       <- "#FFFFFF"

#' @rdname nhs_colours
#' @export
nhs_dark_blue   <- "#003087"

#' @rdname nhs_colours
#' @export
nhs_bright_blue <- "#0072CE"

#' @rdname nhs_colours
#' @export
nhs_light_blue  <- "#41B6E6"

#' @rdname nhs_colours
#' @export
nhs_aqua_blue   <- "#00A9CE"

#' @rdname nhs_colours
#' @export
nhs_black       <- "#231f20"

#' @rdname nhs_colours
#' @export
nhs_dark_grey   <- "#425563"

#' @rdname nhs_colours
#' @export
nhs_mid_grey    <- "#768692"

#' @rdname nhs_colours
#' @export
nhs_pale_grey   <- "#E8EDEE"

#' @rdname nhs_colours
#' @export
nhs_dark_green  <- "#006747"

#' @rdname nhs_colours
#' @export
nhs_green       <- "#009639"

#' @rdname nhs_colours
#' @export
nhs_light_green <- "#78BE20"

#' @rdname nhs_colours
#' @export
nhs_aqua_green  <- "#00A499"

#' @rdname nhs_colours
#' @export
nhs_purple      <- "#330072"

#' @rdname nhs_colours
#' @export
nhs_dark_pink   <- "#7C2855"

#' @rdname nhs_colours
#' @export
nhs_pink        <- "#AE2573"

#' @rdname nhs_colours
#' @export
nhs_dark_red    <- "#8A1538"

#' @rdname nhs_colours
#' @export
nhs_orange      <- "#ED8B00"

#' @rdname nhs_colours
#' @export
nhs_warm_yellow <- "#FFB81C"

#' @rdname nhs_colours
#' @export
nhs_yellow      <- "#FAE100"


#'
#' NHS palettes
#'
#' @examples
#' nhs_palettes
#'
#' @rdname nhs_palette
#' @export
#'
nhs_palettes <- list(
  nhs_primary =        c("#005EB8", "#FFFFFF"),
  nhs_blue_tones =     c("#003087", "#005EB8", "#0072CE", "#41B6E6", "#00A9CE"),
  nhs_neutrals =       c("#231f20", "#425563", "#768692", "#E8EDEE"),
  nhs_support_greens = c("#006747", "#009639", "#78BE20", "#00A499"),
  nhs_highlights = c(
    "#330072", "#7C2855", "#AE2573", "#8A1538", "#ED8B00", "#FFB81C", "#FAE100"
  ),
  nhs_dark_blues =   c("#003087", "#33599F", "#6682B7", "#99ACCF", "#CCD5E7"),
  nhs_blues =        c("#005EB8", "#337EC6", "#669ED4", "#99BEE2", "#CCDEF0"),
  nhs_bright_blues = c("#0072CE", "#338ED7", "#66AAE1", "#99C6EB", "#CCE2F5"),
  nhs_light_blues =  c("#41B6E6", "#67C4EB", "#8DD3F0", "#B3E1F5", "#D9F0FA"),
  nhs_aqua_blues =   c("#00A9CE", "#33BAD7", "#66CBE1", "#99DCEB", "#CCEDF5"),
  nhs_blacks =       c("#231f20", "#4F4B4C", "#7B7879", "#A7A5A5", "#D3D2D2"),
  nhs_dark_greys =   c("#425563", "#677782", "#8D99A1", "#B3BBC0", "#D9DDDF"),
  nhs_mid_greys =    c("#768692", "#919EA7", "#ACB6BD", "#C8CED3", "#E3E6E9"),
  nhs_pale_greys =   c("#E8EDEE", "#ECF0F1", "#F1F4F4", "#F5F7F8", "#FAFBFB"),
  nhs_dark_greens =  c("#006747", "#33856B", "#66A390", "#99C2B5", "#CCE0DA"),
  nhs_greens =       c("#009639", "#33AB60", "#66C088", "#99D5AF", "#CCEAD7"),
  nhs_light_greens = c("#78BE20", "#93CB4C", "#AED879", "#C9E5A5", "#E4F2D2"),
  nhs_aqua_greens =  c("#00A499", "#33B6AD", "#66C8C1", "#99DAD6", "#CCECEA"),
  nhs_purples =      c("#330072", "#5B338E", "#8466AA", "#AD99C6", "#D6CCE2"),
  nhs_dark_pinks =   c("#7C2855", "#965377", "#B07E99", "#CAA9BB", "#E4D4DD"),
  nhs_pinks =        c("#AE2573", "#BE508F", "#CE7CAB", "#DEA7C7", "#EED3E3"),
  nhs_dark_reds =    c("#8A1538", "#A1435F", "#B87287", "#D0A1AF", "#E7D0D7"),
  nhs_oranges =      c("#ED8B00", "#F0A233", "#F4B966", "#F7D099", "#FBE7CC"),
  nhs_warm_yellows = c("#FFB81C", "#FFC649", "#FFD476", "#FFE2A4", "#FFF0D1"),
  nhs_yellows =      c("#FAE100", "#FBE733", "#FCED66", "#FDF399", "#FEF9CC")
)


#'
#' NHS fonts
#'
#' @examples
#' nhs_fonts
#'
#' @rdname nhs_font
#' @export
#'
nhs_fonts <- list(
  nhs_frutiger = "Frutiger",
  nhs_arial = "Arial"
)

#'
#' Set NHS font to use based on what is available from the system
#'
#' The function will search the system for availability of any of the NHS
#' fonts in hierarchical order starting with *Frutiger*, and then *Arial*. If
#' none of these are found in the system, the function will return *Noto Sans*
#' by default or the user can set which font to use as alternative by specifying
#' `alt`.
#'
#' @param alt A character value for font family to use if all of the NHS
#'   fonts are not available in the system.
#'
#' @returns A character value for font family to use as NHS font.
#'
#' @examples
#' set_nhs_font()
#'
#' @rdname nhs_font
#' @export
#'

set_nhs_font <- function(alt = paleta_fonts$paleta_noto) {
  ## Detect which fonts are available to the system ----
  fonts <- systemfonts::system_fonts()

  ## Check which NHS font is available ----
  if (any(fonts$family == nhs_fonts$nhs_frutiger)) {
    nhs_font <- nhs_fonts$nhs_frutiger
  } else {
    if (any(fonts$family == nhs_fonts$nhs_arial)) {
      nhs_font <- nhs_fonts$nhs_arial
    } else {
      nhs_font <- alt
    }
  }

  ## Return nhs_font ----
  nhs_font
}


#'
#' A [ggplot2] theme using NHS fonts, colours, and palettes
#'
#' These are wrappers for [theme_paleta()] that use colours and fonts from the
#' NHS visual identity guidelines.
#'
#' @section Colours:
#' The NHS theme is based on the colours from the [nhs_palettes]. The
#' primary palette consists of two colours: `nhs_palettes$nhs_primary`. The
#' secondary palette consists of nineteen colours:.
#'
#' @section Fonts:
#' The NHS theme uses two fonts as prescribed by the NHS visual
#' identity guidelines. These fonts (in hierarchical order of preference) are
#' *Frutiger* and *Arial*. Any or all of these fonts should be available in
#' the user's system for them to be used in the theme. If none of these fonts
#' are available in the user's system, a freely downloadable alternative called
#' *Noto Sans* is the default fallback font and can be obtained from
#' [Google Fonts](https://fonts.google.com/).
#'
#' @param base_family Base font family using NHS fonts. Default is set
#'   by what NHS font is available in the system via [set_nhs_font()].
#'   If none of the NHS fonts are available, the default becomes
#'   *Noto Sans*.
#' @param base_size Base font size. Default is 11.5.
#' @param plot_title_family Font family to use for the plot title. Default is
#'   `base_family`.
#' @param plot_title_colour Colour of the plot title text. Default
#'   is `nhs_blue`.
#' @param subtitle_family Font family to use for the plot subtitle. Default is
#'   `base_family`.
#' @param subtitle_colour Colour of the subtitle text. Default is [nhs_mid_grey].
#' @param caption_colour Colour of the caption text. Default is [nhs_mid_grey].
#' @param axis_title_colour Colour of the axis title text. Default is
#'   `nhs_cyan`.
#' @param legend_title_colour Colour of the legend title text. Default is NULL.
#' @param legend_text_colour Colour of the legend text. Default is NULL.
#' @param grid_col Grid colour. Default to [nhs_pale_grey].
#' @param axis_col Axis colours. Default to [nhs_pale_grey].
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
#'     values = nhs_palettes$nhs_support_greens
#'   ) +
#'   labs(
#'     title = "Engine shape by number of cylinders",
#'     subtitle = "An example plot for this package",
#'     x = "Engine Shape",
#'     y = "Counts"
#'    ) +
#'    theme_nhs()
#' }
#'
#' @rdname theme_nhs
#' @export
#'
theme_nhs <- function(base_family = set_nhs_font(),
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
