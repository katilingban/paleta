# Create colours dataset -------------------------------------------------------

## Africa CDC colours ----

acdc_colours <- tibble::tibble(
  organisation = "Africa CDC",
  name = c(
    "ACDC Green", "ACDC Gold", "ACDC Black", "ACDC Brown",
    "ACDC Yellow", "ACDC Orange", "ACDC Red", "ACDC Dark Green"
  ),
  code = c(
    "acdc_green", "acdc_gold", "acdc_black", "acdc_brown",
    "acdc_yellow", "acdc_orange", "acdc_red", "acdc_dark_green"
  ),
  rgb = c(
    "52, 143, 65", "180, 162, 105", "0, 0, 0", "107, 76, 36",
    "224, 143, 42", "196, 91, 57", "120, 44, 45", "75, 84, 48"
  ),
  cmyk = c(
    "81, 20, 100, 6", "31, 31, 69, 2", "0, 0, 0, 100", "43, 61, 93, 39",
    "11, 50, 98, 0", "18, 76, 86, 5", "33, 87, 76, 39", "69, 43, 95, 37"
  ),
  hex = c(
    "#348F41", "#B4A269", "#000000", "#6B4C24",
    "#E08F2A", "#C45B39", "#782C2D", "#4B5430"
  ),
  pantone = c(
    "7740C", "4515C", "Black C", "1405C",
    "129C", "158C", "1807C", "364C"
  )
)


motif_colours <- rbind(acdc_colours)

usethis::use_data(motif_colours, overwrite = TRUE, compress = "xz")
