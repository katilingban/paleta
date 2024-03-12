# Tests for theme_wb() -------------------------------------------------------

library(ggplot2)

## Test wb_fonts class ----
testthat::expect_type(wb_fonts, "list")

## Test length of wb fonts ----
testthat::expect_equal(length(wb_fonts), 2)

## Test output of set_wb_font ----
testthat::expect_type(set_wb_font(), "character")
testthat::expect_equal(length(set_wb_font()), 1)

## test theme_paleta() ----

### scatterplot
p <- ggplot(data = mtcars, mapping = aes(x = mpg, y = disp, colour = factor(cyl))) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Cylinders",
    values = wb_palettes$wb_secondary
  )

### Test main plotting api ----
testthat::expect_silent(p + theme_wb())


### Test grid ----
testthat::expect_silent(p + theme_wb(grid = "X"))
testthat::expect_silent(p + theme_wb(grid = "Xx"))
testthat::expect_silent(p + theme_wb(grid = "Y"))
testthat::expect_silent(p + theme_wb(grid = "Yy"))
testthat::expect_silent(p + theme_wb(grid = "XxYy"))
testthat::expect_silent(p + theme_wb(grid = FALSE))

### Test axis ----
testthat::expect_silent(p + theme_wb(axis = TRUE))
testthat::expect_silent(p + theme_wb(axis = "x"))
testthat::expect_silent(p + theme_wb(axis = "y"))
testthat::expect_silent(p + theme_wb(axis = "xy"))

### Test ticks ----
testthat::expect_silent(p + theme_wb(ticks = TRUE))
