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
  acdc_primary = c("#348F41", "#B4A269", "#000000"),
  acdc_secondary = c("#6B4C24", "#E08F2A", "#C45B39", "#782C2D", "#4B5430")
)


#'
#' Africa CDC fonts
#'
#' @examples
#' acdc_fonts
#'
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
#' @return A character value for font family to use as Africa CDC font.
#'
#' @examples
#' set_acdc_font()
#'
#' @rdname acdc_font
#' @export
#'

set_acdc_font <- function(alt = motif_fonts$motif_noto) {
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
#' @param base_family Base font family using Africa CDC fonts. Default is set
#'   by what Africa CDC font is available in the sytem via `set_acdc_font()`. If
#'   none of the Africa CDC fonts are available, the default becomes Noto Sans.
#' @param base_size Base font size. Default is 11.5.
#' @param plot_title_family Font family to use for the plot title. Default is
#'   `base_family`.
#' @param plot_title_face Font face ("plain", "italic", "bold", "bold.italic")
#'   for plot title. Default is "bold".
#' @param plot_title_size Plot title text size in pts. Default is 18.
#' @param plot_title_margin Margin at the bottom of the plot title. Default
#'   set at 10.
#' @param plot_title_colour Colour of the plot title text. Default
#'   is `acdc_green`.
#' @param subtitle_family Font family to use for the plot subtitle. Default is
#'   `base_family`.
#' @param subtitle_face Font face ("plain", "italic", "bold", "bold.italic")
#'   for plot subtitle. Default is "plain".
#' @param subtitle_size Plot subtitle text size in pts. Default is 12.
#' @param subtitle_colour Colour of the subtitle text. Default is `acdc_gold`.
#' @param subtitle_margin Margin at the bottom of the plot subtitle. Default
#'   set at 15.
#' @param strip_text_family Font family to use for the facet label. Default is
#'   `base_family`.
#' @param strip_text_face Font face ("plain", "italic", "bold", "bold.italic")
#'   for facet label. Default is "plain".
#' @param strip_text_size Facet label text size in pts. Default is 12.
#' @param caption_family Font family to use for the caption text. Default is
#'   `base_family`.
#' @param caption_face Font face ("plain", "italic", "bold", "bold.italic") for
#'   caption text. Default is "plain".
#' @param caption_size Caption text size in pts. Default is 9.
#' @param caption_margin Margin at the top of the plot caption text. Default is
#'   set at 10.
#' @param caption_colour Colour of the caption text. Default is `acdc_gold`.
#' @param axis_title_family Font family to use for the axis title. Default is
#'   `base_family`.
#' @param axis_title_face Font face ("plain", "italic", "bold", "bold.italic")
#'   for axis title. Default is "plain".
#' @param axis_title_size Axis title text size in pts. Default is 9.
#' @param axis_title_colour Colour of the axis title text. Default is
#'   `acdc_gold`.
#' @param axis_title_just Axis title font justification, one of
#'   "bl" (bottom-left), "m" (middle), "rt" (right-top). Default is "rt".
#' @param plot_margin Plot margins (specify with `ggplot2::margin()`)
#' @param grid_col Grid colour. Default to `acdc_gold`.
#' @param axis_col Axis colors. Default to `acdc_gold`.
#' @param grid Panel grid. Either `TRUE`, `FALSE`, or a combination of
#'   `X` (major x grid), `x` (minor x grid), `Y` (major y grid), and/or
#'   `y` (minor y grid). Default is TRUE.
#' @param axis_text_size Axis text size in pts. Default is `base_size`.
#' @param axis Add x or y axes? `TRUE`, `FALSE`, "`xy`". Default is FALSE.
#' @param ticks Logical. Should ticks be added? Default is FALSE.
#'
#' @return A [ggplot2] theme.
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
                             plot_title_size = 18,
                             plot_title_face = "bold",
                             plot_title_colour = acdc_green,
                             plot_title_margin = 10,
                             subtitle_family = base_family,
                             subtitle_size = 12,
                             subtitle_face = "plain",
                             subtitle_colour = acdc_gold,
                             subtitle_margin = 15,
                             strip_text_family = base_family,
                             strip_text_size = 12,
                             strip_text_face = "plain",
                             caption_family = base_family,
                             caption_size = 9,
                             caption_face = "italic",
                             caption_colour = acdc_gold,
                             caption_margin = 10,
                             axis_text_size = base_size,
                             axis_title_family = subtitle_family,
                             axis_title_size = 9,
                             axis_title_colour = acdc_gold,
                             axis_title_face = "plain",
                             axis_title_just = "rt",
                             plot_margin = ggplot2::margin(30, 30, 30, 30),
                             grid_col = acdc_gold,
                             grid = TRUE,
                             axis_col = acdc_gold,
                             axis = FALSE,
                             ticks = FALSE) {
  ## Set theme_minial ----
  design <- ggplot2::theme_minimal(
    base_family = base_family, base_size = base_size
  )

  ## Set legend design ----
  design <- design +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(
        family = base_family, colour = acdc_gold
      ),
      legend.text = ggplot2::element_text(
        family = base_family, colour = acdc_gold
      )
    )

  ## Set grid design ----
  if (inherits(grid, "character") | grid == TRUE) {
    design <- design +
      ggplot2::theme(
        panel.grid = ggplot2::element_line(color = grid_col, size = 0.2)
      )

    design <- design +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = grid_col, size = 0.2)
      )

    design <- design +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_line(color = grid_col, size = 0.05)
      )

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

      if (regexpr("Y", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

      if (regexpr("x", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())

      if (regexpr("y", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  } else {
    design <- design +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  ## Set axis design ----
  if (inherits(axis, "character") | axis == TRUE) {
    design <- design +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = acdc_gold, size = 0.15)
      )

    if (inherits(axis, "character")) {
      axis <- tolower(axis)

      if (regexpr("x", axis)[1] < 0) {
        design <- design +
          ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        design <- design +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(color = axis_col, size = 0.15)
          )
      }

      if (regexpr("y", axis)[1] < 0) {
        design <- design +
          ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        design <- design +
          ggplot2::theme(
            axis.line.y = ggplot2::element_line(color = axis_col, size = 0.15)
          )
      }
    } else {
      design <- design +
        ggplot2::theme(
          axis.line.x = ggplot2::element_line(color = axis_col, size = 0.15)
        )

      design <- design +
        ggplot2::theme(
          axis.line.y = ggplot2::element_line(color = axis_col, size = 0.15)
        )
    }
  } else {
    design <- design + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  ## Set ticks design ----
  if (!ticks) {
    design <- design + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    design <- design + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    design <- design + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    design <- design +
      ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth = 0.15))
    design <- design +
      ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth = 0.15))
    design <- design +
      ggplot2::theme(axis.ticks.y = ggplot2::element_line(linewidth = 0.15))
    design <- design +
      ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  ## Set axis text design ----
  xj <- switch(
    tolower(substr(axis_title_just, 1, 1)),
    b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1
  )
  yj <- switch(
    tolower(substr(axis_title_just, 2, 2)),
    b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1
  )

  ### x-axis text ----
  design <- design +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = axis_text_size, margin = ggplot2::margin(t = 0)
      )
    )

  ### y-axis text ----
  design <- design +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = axis_text_size, margin = ggplot2::margin(r = 0)
      )
    )

  ### axis titles ----
  design <- design +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        size = axis_title_size, family = axis_title_family,
        colour = axis_title_colour
      )
    )

  ### axis title adjustment ----
  design <- design +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        hjust = xj, size = axis_title_size,
        family = axis_title_family, face = axis_title_face
    )
  )

  design <- design +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        hjust = yj, size = axis_title_size,
        family = axis_title_family, face = axis_title_face
      )
    )

  ### y-axis on the right ----
  design <- design +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(
        hjust = yj, size = axis_title_size, angle = 90,
        family = axis_title_family, face = axis_title_face
      )
    )

  ### Set facet label design ----
  design <- design +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        hjust = 0, size = strip_text_size,
        face = strip_text_face, family = strip_text_family
      )
    )

  ### Set facet design ----
  design <- design +
    ggplot2::theme(panel.spacing = grid::unit(2, "lines"))

  ### Set plot title design ----
  design <- design +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0, size = plot_title_size, colour = plot_title_colour,
        margin = ggplot2::margin(b = plot_title_margin),
        family = plot_title_family, face = plot_title_face)
    )

  ### Set plot subtitle design ----
  design <- design +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(
        hjust = 0, size = subtitle_size, colour = subtitle_colour,
        margin = ggplot2::margin(b = subtitle_margin),
        family = subtitle_family, face = subtitle_face
      )
    )

  ### Set plot caption design ----
  design <- design +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 1, size = caption_size, colour = caption_colour,
        margin = ggplot2::margin(t = caption_margin),
        family = caption_family, face = caption_face
      )
    )

  ### Set plot margins ----
  design <- design + ggplot2::theme(plot.margin = plot_margin)

  ## return design ----
  design
}


#'
#' @rdname theme_acdc
#' @export
#'
theme_acdc_dark <- function(base_family = set_acdc_font(),
                            base_size = 11.5,
                            plot_title_family = base_family,
                            plot_title_size = 18,
                            plot_title_face = "bold",
                            plot_title_colour = acdc_green,
                            plot_title_margin = 10,
                            subtitle_family = base_family,
                            subtitle_size = 12,
                            subtitle_face = "plain",
                            subtitle_colour = acdc_black,
                            subtitle_margin = 15,
                            strip_text_family = base_family,
                            strip_text_size = 12,
                            strip_text_face = "plain",
                            caption_family = base_family,
                            caption_size = 9,
                            caption_face = "italic",
                            caption_colour = acdc_black,
                            caption_margin = 10,
                            axis_text_size = base_size,
                            axis_title_family = subtitle_family,
                            axis_title_size = 9,
                            axis_title_colour = acdc_black,
                            axis_title_face = "plain",
                            axis_title_just = "rt",
                            plot_margin = ggplot2::margin(30, 30, 30, 30),
                            grid_col = acdc_green,
                            grid = TRUE,
                            axis_col = acdc_green,
                            axis = FALSE,
                            ticks = FALSE) {
  ## Set theme_minial ----
  design <- ggplot2::theme_minimal(
    base_family = base_family, base_size = base_size
  )

  ## Set plot background design ----
  design <- design +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = acdc_gold))

  ## Set legend design ----
  design <- design +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(
        family = base_family, colour = acdc_black
      ),
      legend.text = ggplot2::element_text(
        family = base_family, colour = acdc_black
      )
    )

  ## Set grid design ----
  if (inherits(grid, "character") | grid == TRUE) {
    design <- design +
      ggplot2::theme(
        panel.grid = ggplot2::element_line(color = grid_col, size = 0.2)
      )

    design <- design +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = grid_col, size = 0.2)
      )

    design <- design +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_line(color = grid_col, size = 0.05)
      )

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())

      if (regexpr("Y", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())

      if (regexpr("x", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())

      if (regexpr("y", grid)[1] < 0)
        design <- design +
          ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  } else {
    design <- design +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  ## Set axis design ----
  if (inherits(axis, "character") | axis == TRUE) {
    design <- design +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = acdc_green, size = 0.15)
      )

    if (inherits(axis, "character")) {
      axis <- tolower(axis)

      if (regexpr("x", axis)[1] < 0) {
        design <- design +
          ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        design <- design +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(color = axis_col, size = 0.15)
          )
      }

      if (regexpr("y", axis)[1] < 0) {
        design <- design +
          ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        design <- design +
          ggplot2::theme(
            axis.line.y = ggplot2::element_line(color = axis_col, size = 0.15)
          )
      }
    } else {
      design <- design +
        ggplot2::theme(
          axis.line.x = ggplot2::element_line(color = axis_col, size = 0.15)
        )

      design <- design +
        ggplot2::theme(
          axis.line.y = ggplot2::element_line(color = axis_col, size = 0.15)
        )
    }
  } else {
    design <- design + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  ## Set ticks design ----
  if (!ticks) {
    design <- design + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    design <- design + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    design <- design + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    design <- design +
      ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth = 0.15))
    design <- design +
      ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth = 0.15))
    design <- design +
      ggplot2::theme(axis.ticks.y = ggplot2::element_line(linewidth = 0.15))
    design <- design +
      ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  ## Set axis text design ----
  xj <- switch(
    tolower(substr(axis_title_just, 1, 1)),
    b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1
  )
  yj <- switch(
    tolower(substr(axis_title_just, 2, 2)),
    b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1
  )

  design <- design +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = axis_text_size, margin = ggplot2::margin(t = 0)
      )
    )

  design <- design +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = axis_text_size, margin = ggplot2::margin(r = 0)
      )
    )

  ## Set axis title text design ----
  design <- design +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        size = axis_title_size, family = axis_title_family,
        colour = axis_title_colour
      )
    )

  ### Set x-axis placement ----
  design <- design +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        hjust = xj, size = axis_title_size,
        family = axis_title_family, face = axis_title_face
      )
    )

  ### Set y-axis placement ----
  design <- design +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(
        hjust = yj, size = axis_title_size,
        family = axis_title_family, face = axis_title_face
      )
    )

  ### Set y-axis right side placement ----
  design <- design +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(
        hjust = yj, size = axis_title_size, angle = 90,
        family = axis_title_family, face = axis_title_face
      )
    )

  ## Set facet label design ----
  design <- design +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        hjust = 0, size = strip_text_size,
        face = strip_text_face, family = strip_text_family
      )
    )

  ## Set facet spacing design ----
  design <- design +
    ggplot2::theme(panel.spacing = grid::unit(2, "lines"))

  ## Set plot title design ----
  design <- design +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0, size = plot_title_size, colour = plot_title_colour,
        margin = ggplot2::margin(b = plot_title_margin),
        family = plot_title_family, face = plot_title_face)
    )

  ## Set plot subtitle design ----
  design <- design +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(
        hjust = 0, size = subtitle_size, colour = subtitle_colour,
        margin = ggplot2::margin(b = subtitle_margin),
        family = subtitle_family, face = subtitle_face
      )
    )

  ## Set plot caption design ----
  design <- design +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 1, size = caption_size, colour = caption_colour,
        margin = ggplot2::margin(t = caption_margin),
        family = caption_family, face = caption_face
      )
    )

  ## Set plot margin ----
  design <- design + ggplot2::theme(plot.margin = plot_margin)

  ## return design ----
  design
}


