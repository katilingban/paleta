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
