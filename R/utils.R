#'
#' Print function for palettes
#'
#' @param x An object of class `palette`
#' @param ... Additional arguments to print
#'
#' @export
#'
print.palette <- function(x, ...) {
  n <- length(x)

  withr::with_par(
    new = list(mar = c(0.5, 0.5, 0.5, 0.5)),
    code = {
      image(1:n, 1, as.matrix(1:n), col = x,
            ylab = "", xaxt = "n", yaxt = "n", bty = "n")

      rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
      text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
    }
  )
}


#'
#' Get named colours vector
#'
#' @param pattern Optional. A character value or vector to use as a search term.
#'   Default is NULL in which case all the Oxford colours are returned.
#' @param model A character vector of colour model. Can be "rgb", "cmyk", "hex",
#'   or "pantone". Default is "hex".
#' @param named Logical. Should the output be a named character value or
#'   vector? Default is FALSE.
#'
#' @return A character value or vector of colour/s as per `model` specification.
#'   If `named` is TRUE, returns a named character value or vector.
#'
#' @examples
#' get_colours()
#' get_colours(model = "rgb")
#' get_colours(pattern = "orange")
#' get_colours(pattern = c("orange", "brown"), named = TRUE)
#'
#' @rdname get_colour
#' @export
#'
get_colour <- function(pattern = NULL,
                       model = c("hex", "rgb", "cmyk", "pantone"),
                       named = FALSE) {
  ## Get type ----
  model <- match.arg(model)

  ## Get df ----
  df <- paleta::paleta_colours

  ## Determine if there is something specific to search for ----
  if (!is.null(pattern)) {
    ## Get colours vector ----
    paleta_cols <- df[c("name", model)][stringr::str_detect(df$name, pattern = pattern), ]


    if (named) {
      paleta_cols <- paleta_cols |>
        (\(x)
         {
           cols <- x[[model]]
           names(cols) <- x$name
           cols
        }
        )()
    } else {
      paleta_cols <- paleta_cols[[model]]
    }
  } else {
    ## Get colours vector ----
    paleta_cols <- df[[model]]

    if (named) {
      names(paleta_cols) <- df[["name"]]
    }
  }

  paleta_cols
}

#'
#' @rdname get_colour
#' @export
#'

get_colours <- function(pattern = NULL,
                        model = c("hex", "rgb", "cmyk", "pantone"),
                        named = FALSE) {
  ## Return all or just specific colours ----
  if (is.null(pattern)) {
    paleta_cols <- get_colour(pattern = pattern, model = model, named = named)
  } else {
    paleta_cols <- lapply(
      X = pattern, FUN = get_colour, model = model, named = named
    ) |>
      unlist()
  }

  ## Return paleta_cols ----
  paleta_cols
}


#'
#' Get tint of colours
#'
#' @param hex A character value or vector of character of values for hex code
#'   of colour/s to tint.
#' @param p A numeric value or vector of numeric values for proportion/s
#'   (range from 0 to 1) to tint the colour/s with.
#'
#' @returns A character value or vector of character values of hex code/s
#'   tinted to the desired proportion.
#'
#' @examples
#' tint_colour(acdc_green, p = 0.2)
#' tint_colours(acdc_palettes$acdc_secondary, p = 0.4)
#'
#' @rdname tint_colour
#' @export
#'
tint_colour <- function(hex, p) {
  col_rgb <- grDevices::col2rgb(col = hex)

  (255 - col_rgb) |>
    (\(x) x * p)() |>
    (\(x) col_rgb + x)() |>
    (\(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))()
}


#'
#' @rdname tint_colour
#' @export
#'
tint_colours_ <- function(hex, p) {
  pal <- Map(
    f = tint_colour,
    hex = rep(list(hex), length(p)),
    p = as.list(p)
  )

  #names(pal) <- paste0(p * 100, "%")

  unlist(pal)
}

#'
#' @rdname tint_colour
#' @export
#'
tint_colours <- function(hex, p) {
  pal <- Map(
    f = tint_colours_,
    hex = as.list(hex),
    p = rep(list(p), length(hex))
  )

  #names(pal) <- hex

  pal
}



#'
#' Get shade of colours
#'
#' @param hex A character value or vector of character of values for hex code
#'   of colour/s to shade.
#' @param p Range from 0 to 1 for proportion to shade the colour/s with.
#'
#' @returns A character value or vector of character values of hex code/s
#'   shaded to the desired proportion.
#'
#' @examples
#' shade_colour(acdc_green, p = 0.2)
#' shade_colours(acdc_palettes$acdc_secondary, p = 0.4)
#'
#' @rdname shade_colour
#' @export
#'
shade_colour <- function(hex, p) {
  col_rgb <- grDevices::col2rgb(col = hex)

  (col_rgb * p) |>
    (\(x) col_rgb - x)() |>
    round() |>
    (\(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))()
}

#'
#' @rdname shade_colour
#' @export
#'
shade_colours <- function(hex, p) {
  lapply(X = hex, FUN = shade_colour, p = p) |>
    unlist()
}
