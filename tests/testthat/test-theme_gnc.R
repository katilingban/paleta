# Tests for theme_gnc ----------------------------------------------------------

library(ggplot2)

test_that("theme_gnc works as expected", {
  ## Test acdc_fonts class ----
  testthat::expect_type(gnc_fonts, "list")

  ## Test length of acdc fonts ----
  testthat::expect_equal(length(gnc_fonts), 4)

  ## Test output of set_acdc_font ----
  testthat::expect_type(set_gnc_font(), "character")
  testthat::expect_equal(length(set_gnc_font()), 1)

  ## test theme_gnc() ----

  ## scatterplot
  p <- ggplot(
    data = mtcars, mapping = aes(x = mpg, y = disp, colour = factor(cyl))
  ) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Cylinders",
    values = gnc_palettes$gnc_secondary
  ) +
  labs(
    title = "GNC Theme",
    subtitle = "Using the GNC secondary palette",
  )

  ### Test main plotting api ----
  testthat::expect_silent(p + theme_gnc())
  testthat::expect_silent(p + theme_gnc())

  ### Test grid ----
  testthat::expect_silent(p + theme_gnc(grid = "X"))
  testthat::expect_silent(p + theme_gnc(grid = "Xx"))
  testthat::expect_silent(p + theme_gnc(grid = "Y"))
  testthat::expect_silent(p + theme_gnc(grid = "Yy"))
  testthat::expect_silent(p + theme_gnc(grid = "XxYy"))
  testthat::expect_silent(p + theme_gnc(grid = FALSE))

  ### Test axis ----
  testthat::expect_silent(p + theme_gnc(axis = TRUE))
  testthat::expect_silent(p + theme_gnc(axis = "x"))
  testthat::expect_silent(p + theme_gnc(axis = "y"))
  testthat::expect_silent(p + theme_gnc(axis = "xy"))

  ### Test ticks ----
  testthat::expect_silent(p + theme_gnc(ticks = TRUE))
})