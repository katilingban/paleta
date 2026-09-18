# Get tint of colours

Get tint of colours

## Usage

``` r
tint_colour(hex, p)

tint_colours_(hex, p)

tint_colours(hex, p, label = FALSE)
```

## Arguments

- hex:

  A character value or vector of character of values for hex code of
  colour/s to tint.

- p:

  A numeric value or vector of numeric values for proportion/s (range
  from 0 to 1) to tint the colour/s with.

- label:

  Logical. Should the output/s be labelled? Default is FALSE.

## Value

A character value or vector of character values of hex code/s tinted to
the desired proportion.

## Examples

``` r
tint_colour(acdc_green, p = 0.2)
#> [1] "#D6E8D9"
tint_colours(acdc_palettes$acdc_secondary, p = 0.4)
#> [[1]]
#> [1] "#A3B8D2"
#> 
#> [[2]]
#> [1] "#DEA2BB"
#> 
#> [[3]]
#> [1] "#BDCBD5"
#> 
#> [[4]]
#> [1] "#FFE2A3"
#> 
#> [[5]]
#> [1] "#A4E9ED"
#> 
#> [[6]]
#> [1] "#FFBDAE"
#> 
#> [[7]]
#> [1] "#D2B5D6"
#> 
#> [[8]]
#> [1] "#F0F3B3"
#> 
#> [[9]]
#> [1] "#AFBDDE"
#> 
#> [[10]]
#> [1] "#F5A5C8"
#> 
#> [[11]]
#> [1] "#99D3CD"
#> 
```
