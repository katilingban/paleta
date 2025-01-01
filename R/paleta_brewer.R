#'
#' Create new palettes based on organisational palettes
#'
#' These functions apply a similar approach used and demonstrated by
#' [ColorBrewer](https://colorbrewer2.org) and has been patterned after the
#' syntax of the `RColorBrewer` package
#'
#' @param org Name of organisation. Currently supports only *"acdc"* for the
#'   Africa CDC colour palettes.
#' @param name Name of the organisational palette to use
#' @param n Number of colours desired/required. Organisational palettes should
#'   have at least 3 colours and up to 9 colours maximum. All colour schemes are
#'   derived from an organisation's brand/style guidelines.
#' @param type A character value for type of palette to use. Can be either
#'   *"sequential"*, *"divergent"*, or *"qualitative"*.
#'
#' @returns A character vector of desired/required colours with length
#'   equivalent to `n`
#'
#' @examples
#' paleta_create_sequential(n = 5, org = "acdc", name = "blues")
#' paleta_create_divergent(n = 5, org = "acdc", name = "rdylgn")
#'
#' @rdname create_paleta
#' @export
#'

paleta_create_sequential <- function(n, 
                                     org = c("acdc", "nhs"), 
                                     name) {
  ## Get org ----
  org <- match.arg(org)
  
  ## Check if specified palette is found in specified org palette ----
  paleta_check_colour(name = name, org = org)

  ## Check if specified palette is sequential ----
  paleta_check_type(name = name, pal_type = "sequential")

  ## Check if number of colours is compatible with sequential ----
  if (n < 3) {
    cli::cli_bullets(
      c(
        "!" = "Sequential palettes have minimum 3 colours",
        "i" = "Returning 3 colours"
      )
    )

    n <- 3
  }

  if (n > 9) {
    cli::cli_bullets(
      c(
        "!" = "Sequential palettes have maximum 9 colours",
        "i" = "Returning 9 colours"
      )
    )

    n <- 9
  }

  ## Get base palette ----
  pal <- get(paste0(org, "_brewer_palettes"))[[name]]

  ## Update palette to n ----
  pal <- grDevices::colorRampPalette(pal)(n)

  cli::cli_bullets(
    c(
      c(
        "v" = "Sequential colour palette successfully created",
        "i" = "Sequential palette: {pal}"
      )
    )
  )

  ## Create palette class ----
  class(pal) <- "palette"

  ## Return palette ----
  pal
}


#'
#' @rdname create_paleta
#' @export
#'
paleta_create_divergent <- function(n, name, org) {
  ## Check if specified palette is found in specified org palette ----
  paleta_check_colour(name = name, org = org)

  ## Check if specified palette is divergent ----
  paleta_check_type(name = name, pal_type = "divergent")

  ## Check if number of colours is compatible with divergent ----
  if (n < 3) {
    cli::cli_bullets(
      c(
        "!" = "Divergent palettes have minimum 3 colours",
        "i" = "Returning 3 colours"
      )
    )

    n <- 3
  }

  if (n > 11) {
    cli::cli_bullets(
      c(
        "!" = "Divergent palettes have maximum 11 colours",
        "i" = "Returning 11 colours"
      )
    )

    n <- 11
  }

  ## Get base palette ----
  pal <- get(paste0(org, "_brewer_palettes"))[[name]]

  ## Update palette to n ----
  pal <- grDevices::colorRampPalette(pal)(n)

  cli::cli_bullets(
    c(
      "v" = "Divergent colour palette successfully created",
      "i" = "Divergent palette: {pal}"
    )
  )

  ## Create palette class ----
  class(pal) <- "palette"

  ## Return palette ----
  pal
}

#'
#' @rdname create_paleta
#' @export
#'
paleta_create_qualitative <- function(n, name, org) {
  ## Check if specified palette is found in specified org palette ----
  paleta_check_colour(name = name, org = org)

  ## Check if specified palette is divergent ----
  paleta_check_type(name = name, pal_type = "qualitative")

  ## Get base palette ----
  pal <- get(paste0(org, "_brewer_palettes"))[[name]]

  ## Check that n is not more than length(pal) ----
  if (n > length(pal)) {
    cli::cli_bullets(
      c(
        "!" = "{.code n = {n}} is greater than available colours in {name} palette",
        "i" = "Returning all colours in {name} colour palette"
      )
    )

    n <- length(pal)
  }

  ## Update palette to n ----
  pal <- pal[seq_len(n)]

  ## Create palette class ----
  class(pal) <- "palette"

  ## Return palette ----
  pal
}


#'
#' @rdname create_paleta
#' @export
#'
paleta_create_brewer <- function(n, name, org,
                                 type = c("sequential", 
                                          "divergent", 
                                          "qualitative")) {
  ## Determine type of palette ----
  type <- match.arg(type)

  pal <- parse(
    text = paste0("paleta_create_", type, "(n = n, name = name, org = org)")
  ) |>
    eval()

  ## Return palette ----
  pal
}

#'
#' Palette types
#' 
#' @keywords internal
#' 

paleta_brewer_types <- list(
  sequential = c(
    "blues", "bugn", "bupu", "gnbu", "greens", "greys", "pubu", "pubugn", 
    "purd", "rdpu", "reds", "ylgn", "ylgnbu", "ylorbr", "ylorrd"
  ),
  divergent = c(
    "brbg", "piylgn", "prgn", "puor", "rdbu", "rdgy","rdylbu", "rdylgn"
  ),
  qualitative = c(
    "pastel1", "pastel2", "pastel3", "dark", "light", "bright"
  )
)

#'
#' Check if a colour palette name is from a specified organisation
#' 
#' @keywords internal
#' 

paleta_check_colour <- function(name, org) {
  x <- get(paste0(org, "_brewer_palettes"))[[name]]

  if (is.null(x)) {
    cli::cli_abort(
      "Colour palette {.val {name}} is not a {org} colour palette"
    )
  } else {
    cli::cli_alert_success(
      "Colour palette {.val {name}} is a {org} colour palette"
    )
  }

  ## Return colour palette ----
  x
}

#'
#' Check if a colour palette is divergent, sequential, or qualitative
#' 
#' @keywords internal
#' 

paleta_check_type <- function(name, 
                              pal_type = c("sequential", 
                                           "divergent", 
                                           "qualitative")) {
  pal_type <- match.arg(pal_type)
  
  type_check <- name %in% paleta_brewer_types[[pal_type]]

  if (!type_check) {
    cli::cli_abort(
      "{name} is not a {pal_type} colour palette"
    )
  } else {
    cli::cli_alert_success(
      "{name} is a {pal_type} colour palette"
    )
  }
}
