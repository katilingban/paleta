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


## World Bank Colours ----

wb_colours <- tibble::tibble(
  organisation = "World Bank",
  name = c(
    "WB Sapphire Blue", "WB Cyan", "WB Black", "WB White", "WB Bright Orange",
    "WB Bright Yellow", "WB Bright Red", "WB Light Orange", "WB Bright Aqua",
    "WB Bright Green", "WB Bright Purple", "WB Light Aqua", "WB Dark Red",
    "WB Dark Orange", "WB Brown", "WB Dark Purple", "WB Dark Aqua",
    "WB Dark Green"
  ),
  code = c(
    "wb_blue", "wb_cyan", "wb_black", "wb_white", "wb_bright_orange",
    "wb_bright_yellow", "wb_bright_red", "wb_light_orange", "wb_bright_aqua",
    "wb_brith_green", "wb_bright_purple", "wb_light_aqua", "wb_dark_red",
    "wb_dark_orange", "wb_brown", "wb_dark_purple", "wb_dark_aqua",
    "wb_dark_green"
  ),
  rgb = c(
    "0 35 69", "0 173 228", "0, 0, 0", "225, 225, 225", "240, 80, 35",
    "253, 183, 20", "235, 28, 45", "247, 141, 40", "0, 156, 167", "0, 171, 81",
    "135, 43, 144", "0, 169, 150", "152, 37, 43", "225, 106, 45", "184, 140, 29",
    "97, 71, 118", "0, 96, 104", "0, 100, 80"
  ),
  cmyk = c(
    "96, 39, 0, 73", "100, 0, 0, 0", "0, 0, 0, 100", "0, 0, 0, 0",
    "0, 85, 100, 0", "0, 31, 100, 0", "91, 100, 92, 1", "0, 54, 95, 0",
    "100, 11, 38, 1", "91, 0, 97, 0", "56, 99, 0, 0", "100, 0, 54, 0",
    "26, 96, 86, 24", "8, 71, 97, 1", "27, 43, 100, 5", "71, 80, 28, 12",
    "100, 44, 52, 22", "99, 36, 75, 27"
  ),
  hex = c(
    "#002244", "#009FDA", "#000000", "#FFFFFF", "#F05023", "#FDB714",
    "#EB1C2D", "#F78D28", "#009CA7", "#00AB51", "#872B90", "#00A996",
    "#98252B", "#E16A2D", "#B88C1D", "#614776", "#006068", "#006450"
  ),
  pantone = c(
    "5003C", "5015C", "Black C", "9010C", "1655C", "7549C", "185C", "1495C",
    "320C", "7481C", "2602C", "3275C", "7622C", "7578C", "7556C", "7447C",
    "7715C", "336C"
  )
)


## UNICEF colours ----

unicef_colours <- tibble::tibble(
  organisation = "UNICEF",
  name = c(
    "UNICEF Blue", "UNICEF Green", "UNICEF Lime Green", "UNICEF Yellow",
    "UNICEF Orange", "UNICEF Bright Red", "UNICEF Dark Red", "UNICEF Purple",
    "UNICEF Warm Grey", "UNICEF Cool Grey", "UNICEF Black", "UNICEF Dark Blue"
  ),
  code = c(
    "unicef_blue", "unicef_green", "unicef_lime_green", "unicef_yellow",
    "unicef_orange", "unicef_bright_red", "unicef_dark_red", "unicef_purple",
    "unicef_warm_grey", "unicef_cool_grey", "unicef_black", "unicef_dark_blue"
  ),
  rgb = c(
    "0, 174, 239", "0, 131, 62", "128, 189, 65", "255, 194, 14", "242, 106, 33",
    "226, 35, 26", "150, 26, 73", "107, 30, 116", "216, 209, 202",
    "119, 119, 122", "45, 41, 38", "55, 78, 162"
  ),
  cmyk = c(
    "100, 0, 0, 0", "97, 22, 100, 9", "55, 3, 100, 0", "0, 25, 100, 0",
    "0, 72, 100, 0", "5, 100, 100, 0", "31, 100, 53, 20", "70, 100, 20, 7",
    "14, 14, 17, 0", "55, 47, 44, 10", "63, 62, 59, 94", "90, 80, 0, 0"
  ),
  hex = c(
    "#1CABE2", "#00833D", "#80BD41", "#FFC20E", "#F26A21", "#E2231A", "#961A49",
    "#6A1E74", "#D8D1C9", "#777779", "#2D2926", "#374EA2"
  ),
  pantone = c(
    "Process Cyan", "356", "376", "7548", "1505", "485", "221", "2613",
    "Warm Grey 1", "Cool Grey 9", "Black", "7685"
  )
)




paleta_colours <- rbind(acdc_colours, wb_colours, unicef_colours)

usethis::use_data(paleta_colours, overwrite = TRUE, compress = "xz")
