# Tests for utility functions --------------------------------------------------

## Test print function ----
testthat::expect_silent(print(structure(acdc_palettes$acdc_primary, class = "palette")))

## Test get colours function ----
testthat::expect_equal(get_colours() |> length(), nrow(paleta_colours))
testthat::expect_equal(get_colour() |> length(), nrow(paleta_colours))
testthat::expect_type(get_colour(pattern = "Blue", named = TRUE), "character")
testthat::expect_gt(
  get_colour(pattern = "Blue", named = TRUE) |> names() |> length(),
  0
)
testthat::expect_type(get_colours(pattern = "Blue"), "character")
testthat::expect_type(get_colours(pattern = "Blue", named = TRUE), "character")


## Test tint colours function ----
testthat::expect_type(tint_colour(acdc_green, p = 0.2), "character")
testthat::expect_type(tint_colours(c(acdc_green, acdc_gold), p = 0.2), "list")
testthat::expect_type(tint_colours(c(acdc_green, acdc_gold), p = 0.2, label = TRUE), "list")
testthat::expect_equal(
  tint_colours(c(acdc_green, acdc_gold), p = 0.2, label = TRUE) |>
    names() |>
    length(),
  2
)
testthat::expect_type(tint_colours(c(acdc_green, acdc_gold), p = c(0.2, 0.4)), "list")
testthat::expect_equal(
  tint_colours(
    c(acdc_green, acdc_gold),
    p = c(0.2, 0.4)
  ) |>
    (\(x) x[[1]])() |>
    length(),
  2
)


## Test shade colours function ----
testthat::expect_type(shade_colours(c(acdc_green, acdc_gold), p = 0.2), "list")
testthat::expect_type(shade_colours(c(acdc_green, acdc_gold), p = c(0.2, 0.4)), "list")
testthat::expect_type(shade_colours(c(acdc_green, acdc_gold), p = 0.2, label = TRUE), "list")
testthat::expect_equal(
  shade_colours(c(acdc_green, acdc_gold), p = 0.2, label = TRUE) |>
    names() |>
    length(),
  2
)
testthat::expect_equal(
  shade_colours(
    c(acdc_green, acdc_gold),
    p = c(0.2, 0.4)
  ) |>
    (\(x) x[[1]])() |>
    length(),
  2
)
