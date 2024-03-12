# Tests for theme_nhs() --------------------------------------------------------

library(ggplot2)

## Test nhs_fonts class ----
testthat::expect_type(nhs_fonts, "list")

## Test length of nhs fonts ----
testthat::expect_equal(length(nhs_fonts), 2)

## Test output of set_nhs_font ----
testthat::expect_type(set_nhs_font(), "character")
testthat::expect_equal(length(set_nhs_font()), 1)

## test theme_paleta() ----

## scatterplot
p <- ggplot(data = mtcars, mapping = aes(x = mpg, y = disp, colour = factor(cyl))) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Cylinders"
  )

### Test main plotting api ----
testthat::expect_silent(p + theme_nhs())

### Test grid ----
testthat::expect_silent(p + theme_nhs(grid = "X"))
testthat::expect_silent(p + theme_nhs(grid = "Xx"))
testthat::expect_silent(p + theme_nhs(grid = "Y"))
testthat::expect_silent(p + theme_nhs(grid = "Yy"))
testthat::expect_silent(p + theme_nhs(grid = "XxYy"))
testthat::expect_silent(p + theme_nhs(grid = FALSE))

### Test axis ----
testthat::expect_silent(p + theme_nhs(axis = TRUE))
testthat::expect_silent(p + theme_nhs(axis = "x"))
testthat::expect_silent(p + theme_nhs(axis = "y"))
testthat::expect_silent(p + theme_nhs(axis = "xy"))

### Test ticks ----
testthat::expect_silent(p + theme_nhs(ticks = TRUE))
testthat::expect_silent(p + theme_nhs(ticks = TRUE))

