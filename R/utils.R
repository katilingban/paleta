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
  df <- motif::motif_colours

  ## Determine if there is something specific to search for ----
  if (!is.null(pattern)) {
    ## Get colours vector ----
    motif_cols <- df[c("name", model)][stringr::str_detect(df$name, pattern = pattern), ]


    if (named) {
      motif_cols <- motif_cols |>
        (\(x)
         {
           cols <- x[[model]]
           names(cols) <- x$name
           cols
        }
        )()
    } else {
      motif_cols <- motif_cols[[model]]
    }
  } else {
    ## Get colours vector ----
    motif_cols <- df[[model]]

    if (named) {
      names(motif_cols) <- df[["name"]]
    }
  }

  motif_cols
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
    motif_cols <- get_colour(pattern = pattern, model = model, named = named)
  } else {
    motif_cols <- lapply(
      X = pattern, FUN = get_colour, model = model, named = named
    ) |>
      unlist()
  }

  ## Return motif_cols ----
  motif_cols
}

