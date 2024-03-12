# Test paleta ------------------------------------------------------------------

## Test paleta_fonts class
testthat::expect_type(paleta_fonts, "list")

## Test length of paleta fonts
testthat::expect_equal(length(paleta_fonts), 3)

## Test output of set_paleta_font
testthat::expect_type(set_paleta_font(), "character")
testthat::expect_equal(length(set_paleta_font()), 1)


