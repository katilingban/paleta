# Tests for theme_unicef() -----------------------------------------------------

library(ggplot2)

test_that("theme_unicef works as expected", {
  ## Test acdc_fonts class ----
  testthat::expect_type(unicef_fonts, "list")

  ## Test length of acdc fonts ----
  testthat::expect_equal(length(unicef_fonts), 4)

  ## Test output of set_acdc_font ----
  testthat::expect_type(set_unicef_font(), "character")
  testthat::expect_equal(length(set_unicef_font()), 1)

  ## test theme_unicef() ----

  ## scatterplot
  p <- ggplot(
    data = mtcars, mapping = aes(x = mpg, y = disp, colour = factor(cyl))
  ) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Cylinders",
    values = unicef_palettes$unicef_secondary
  ) +
  labs(
    title = "UNICEF Theme",
    subtitle = "Using the UNICEF secondary palette",
  )

  ### Test main plotting api ----
  testthat::expect_silent(p + theme_unicef())
  testthat::expect_silent(p + theme_unicef())

  ### Test grid ----
  testthat::expect_silent(p + theme_unicef(grid = "X"))
  testthat::expect_silent(p + theme_unicef(grid = "Xx"))
  testthat::expect_silent(p + theme_unicef(grid = "Y"))
  testthat::expect_silent(p + theme_unicef(grid = "Yy"))
  testthat::expect_silent(p + theme_unicef(grid = "XxYy"))
  testthat::expect_silent(p + theme_unicef(grid = FALSE))

  ### Test axis ----
  testthat::expect_silent(p + theme_unicef(axis = TRUE))
  testthat::expect_silent(p + theme_unicef(axis = "x"))
  testthat::expect_silent(p + theme_unicef(axis = "y"))
  testthat::expect_silent(p + theme_unicef(axis = "xy"))

  ### Test ticks ----
  testthat::expect_silent(p + theme_unicef(ticks = TRUE))
})

