# Test paleta ------------------------------------------------------------------

library(ggplot2)

## Test paleta_fonts class ----
testthat::expect_type(paleta_fonts, "list")

## Test length of paleta fonts ----
testthat::expect_equal(length(paleta_fonts), 3)

## Test output of set_paleta_font ----
testthat::expect_type(set_paleta_font(), "character")
testthat::expect_equal(length(set_paleta_font()), 1)

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
testthat::expect_silent(p + theme_paleta())

### Test grid ----
testthat::expect_silent(p + theme_paleta(grid = "X"))
testthat::expect_silent(p + theme_paleta(grid = "Xx"))
testthat::expect_silent(p + theme_paleta(grid = "Y"))
testthat::expect_silent(p + theme_paleta(grid = "Yy"))
testthat::expect_silent(p + theme_paleta(grid = "XxYy"))
testthat::expect_silent(p + theme_paleta(grid = FALSE))

### Test axis ----
testthat::expect_silent(p + theme_paleta(axis = TRUE))
testthat::expect_silent(p + theme_paleta(axis = "x"))
testthat::expect_silent(p + theme_paleta(axis = "y"))
testthat::expect_silent(p + theme_paleta(axis = "xy"))

### Test ticks ----
testthat::expect_silent(p + theme_paleta(ticks = TRUE))




