% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_wb.R
\name{theme_wb}
\alias{theme_wb}
\title{A \link{ggplot2} theme using World Bank fonts, colours, and palettes}
\usage{
theme_wb(
  base_family = set_wb_font(),
  base_size = 11.5,
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
  ticks = FALSE
)
}
\arguments{
\item{base_family}{Base font family using World Bank fonts. Default is set
by what World Bank font is available in the system via \code{set_wb_font()}.
If none of the World Bank fonts are available, the default becomes
\emph{Noto Sans}.}

\item{base_size}{Base font size. Default is 11.5.}

\item{plot_title_family}{Font family to use for the plot title. Default is
\code{base_family}.}

\item{plot_title_colour}{Colour of the plot title text. Default
is \code{wb_blue}.}

\item{subtitle_family}{Font family to use for the plot subtitle. Default is
\code{base_family}.}

\item{subtitle_colour}{Colour of the subtitle text. Default is \code{wb_cyan}.}

\item{caption_colour}{Colour of the caption text. Default is \code{wb_cyan}.}

\item{axis_title_colour}{Colour of the axis title text. Default is
\code{wb_cyan}.}

\item{legend_title_colour}{Colour of the legend title text. Default is NULL.}

\item{legend_text_colour}{Colour of the legend text. Default is NULL.}

\item{grid_col}{Grid colour. Default to \code{wb_cyan}.}

\item{grid}{Panel grid. Either \code{TRUE}, \code{FALSE}, or a combination of
\code{X} (major x grid), \code{x} (minor x grid), \code{Y} (major y grid), and/or
\code{y} (minor y grid). Default is TRUE.}

\item{axis_col}{Axis colours. Default to \code{wb_cyan}.}

\item{axis}{Add x or y axes? \code{TRUE}, \code{FALSE}, "\code{xy}". Default is FALSE.}

\item{ticks}{Logical. Should ticks be added? Default is FALSE.}
}
\value{
A \link{ggplot2} theme.
}
\description{
These are wrappers for \code{theme_paleta()} that use colours and fonts from the
World Bank visual identity guidelines.
}
\section{Colours}{

The World Bank theme is based on the colours from the \code{wb_palettes}. The
primary palette consists of four colours: \code{wb_palettes$wb_primary}. The
secondary palette consists of fourteen colours: \code{wb_palettes$wb_secondary}.
}

\section{Fonts}{

The World Bank theme uses two fonts as prescribed by the World Bank visual
identity guidelines. These fonts (in hierarchical order of preference) are
\emph{Andes} and \emph{Arial}. Any or all of these fonts should be available in
the user's system for them to be used in the theme. If none of these fonts
are available in the user's system, a freely downloadable alternative called
\emph{Noto Sans} is the default fallback font and can be obtained from
\href{https://fonts.google.com/}{Google Fonts}.
}

\examples{
\dontrun{
  ggplot(
    data = mtcars,
    mapping = aes(
      x = factor(vs, levels = c(0, 1), labels = c("v-shaped", "straight")),
      fill = factor(cyl))
  ) +
  geom_bar() +
  scale_fill_manual(
    name = "Cylinders",
    values = wb_palettes$wb_secondary
  ) +
  labs(
    title = "Engine shape by number of cylinders",
    subtitle = "An example plot for this package",
    x = "Engine Shape",
    y = "Counts"
   ) +
   theme_wb()
}

}
