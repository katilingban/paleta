#'
#' 
#' 
#' 
#' 

paleta_get_colours <- function(n, org) {

}


paleta_colours_acdc <- function(n) {
  if (n > 11)
    cli::cli_abort(
      "Only 11 discrete colours are available from the ACDC palette."
    )
  
  acdc_palettes$acdc_secondary[seq_len(n)]
}


paleta_colours_gnc <- function(n) {
  
}