# Tests for theme_acdc() -------------------------------------------------------

library(ggplot2)

## Test paleta_fonts class ----
testthat::expect_type(acdc_fonts, "list")

## Test length of paleta fonts ----
testthat::expect_equal(length(acdc_fonts), 4)

## Test output of set_paleta_font ----
testthat::expect_type(set_acdc_font(), "character")
testthat::expect_equal(length(set_acdc_font()), 1)

## test theme_paleta() ----

## scatterplot
p <- ggplot(data = mtcars, mapping = aes(x = mpg, y = disp, colour = factor(cyl))) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Cylinders",
    values = acdc_palettes$acdc_secondary
  ) +
  labs(
    title = "Light ACDC Theme",
    subtitle = "Using the Africa CDC secondary palette",
  )

### Test main plotting api ----
testthat::expect_silent(p + theme_acdc_dark())
testthat::expect_silent(p + theme_acdc_light())

### Test grid ----
testthat::expect_silent(p + theme_acdc_dark(grid = "X"))
testthat::expect_silent(p + theme_acdc_dark(grid = "Xx"))
testthat::expect_silent(p + theme_acdc_dark(grid = "Y"))
testthat::expect_silent(p + theme_acdc_dark(grid = "Yy"))
testthat::expect_silent(p + theme_acdc_dark(grid = "XxYy"))
testthat::expect_silent(p + theme_acdc_dark(grid = FALSE))

testthat::expect_silent(p + theme_acdc_light(grid = "X"))
testthat::expect_silent(p + theme_acdc_light(grid = "Xx"))
testthat::expect_silent(p + theme_acdc_light(grid = "Y"))
testthat::expect_silent(p + theme_acdc_light(grid = "Yy"))
testthat::expect_silent(p + theme_acdc_light(grid = "XxYy"))
testthat::expect_silent(p + theme_acdc_light(grid = FALSE))


### Test axis ----
testthat::expect_silent(p + theme_acdc_dark(axis = TRUE))
testthat::expect_silent(p + theme_acdc_dark(axis = "x"))
testthat::expect_silent(p + theme_acdc_dark(axis = "y"))
testthat::expect_silent(p + theme_acdc_dark(axis = "xy"))

testthat::expect_silent(p + theme_acdc_light(axis = TRUE))
testthat::expect_silent(p + theme_acdc_light(axis = "x"))
testthat::expect_silent(p + theme_acdc_light(axis = "y"))
testthat::expect_silent(p + theme_acdc_light(axis = "xy"))

### Test ticks ----
testthat::expect_silent(p + theme_acdc_dark(ticks = TRUE))
testthat::expect_silent(p + theme_acdc_light(ticks = TRUE))
