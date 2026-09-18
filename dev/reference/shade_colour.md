# Get shade of colours

Get shade of colours

## Usage

``` r
shade_colour(hex, p)

shade_colours_(hex, p)

shade_colours(hex, p, label = FALSE)
```

## Arguments

- hex:

  A character value or vector of character of values for hex code of
  colour/s to shade.

- p:

  Range from 0 to 1 for proportion to shade the colour/s with.

- label:

  Logical. Should the output/s be labelled? Default is FALSE.

## Value

A character value or vector of character values of hex code/s shaded to
the desired proportion.

## Examples

``` r
shade_colour(acdc_green, p = 0.2)
#> [1] "#0A1D0D"
shade_colours(acdc_palettes$acdc_secondary, p = 0.4)
#> [[1]]
#> [1] "#0A203A"
#> 
#> [[2]]
#> [1] "#460A23"
#> 
#> [[3]]
#> [1] "#24323C"
#> 
#> [[4]]
#> [1] "#66490B"
#> 
#> [[5]]
#> [1] "#0C5154"
#> 
#> [[6]]
#> [1] "#662515"
#> 
#> [[7]]
#> [1] "#391D3D"
#> 
#> [[8]]
#> [1] "#575B1B"
#> 
#> [[9]]
#> [1] "#162545"
#> 
#> [[10]]
#> [1] "#5D0C2F"
#> 
#> [[11]]
#> [1] "#003B34"
#> 
```
