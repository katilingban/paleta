# Tests for utility functions --------------------------------------------------

## Test get colours function ----
testthat::expect_type(get_colours(pattern = "Blue"), "character")
testthat::expect_type(get_colours(pattern = "Blue", named = TRUE), "character")
testthat::expect_type(tint_colour(acdc_green, p = 0.2), "character")


## Test tint colours function ----
testthat::expect_type(tint_colours(c(acdc_green, acdc_gold), p = 0.2), "list")
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
testthat::expect_equal(
  shade_colours(
    c(acdc_green, acdc_gold),
    p = c(0.2, 0.4)
  ) |>
    (\(x) x[[1]])() |>
    length(),
  2
)
