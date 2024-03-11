

nhs_url <- "https://www.england.nhs.uk/nhsidentity/identity-guidelines/colours/"

session <- rvest::session(nhs_url)

nhs_colours <- session |>
  rvest::html_elements(".bpxs-col-4") |>
  rvest::html_text() |>
  stringr::str_split(pattern = "\n", simplify = TRUE) |>
  data.frame() |>
  dplyr::slice(c(-10, -19, -23)) |>
  dplyr::select(-X1, -X2, -X8) |>
  dplyr::rename(name = X3, pantone = X4, cmyk = X5, rgb = X6, hex = X7) |>
  dplyr::mutate(
    organisation = "NHS",
    name = stringr::str_remove_all(name, pattern = "                "),
    code = tolower(name) |>
      stringr::str_replace_all(pattern = " ", replacement = "_"),
    pantone = stringr::str_remove_all(pantone, "                Pantone: |\\r"),
    cmyk = stringr::str_remove_all(cmyk, pattern = "CMYK: |\\r") |>
      stringr::str_replace_all(pattern = "\\/", replacement = ", "),
    rgb = stringr::str_remove_all(rgb, pattern = "RGB: |\\r") |>
      stringr::str_replace_all(pattern = "\\/", replacement = ", "),
    hex = stringr::str_remove_all(hex, pattern = "              ")
  ) |>
  dplyr::select(organisation, name, code, rgb, cmyk, hex, pantone) |>
  rbind(
    data.frame(
      organisation = "NHS",
      name = "NHS White",
      code = "nhs_white",
      rgb = "255, 255, 255",
      cmyk = "0, 0, 0, 0",
      hex = "#FFFFFF",
      pantone = ""
    )
  ) |>
  (\(x) x[c(2, nrow(x), 1, 3:nrow(x) - 1), ])()

