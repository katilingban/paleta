# Africa CDC fonts

The function will search the system for availability of any of the
Africa CDC fonts in hierarchical order starting with *Univers*, then
*Din Next Arabic*, and then finally *Arial*. If none of these are found
in the system, the function will return *Noto Sans* by default or the
user can set which font to use as alternative by specifying `alt`.

## Usage

``` r
acdc_fonts

set_acdc_font(alt = paleta_fonts$paleta_noto)
```

## Arguments

- alt:

  A character value for font family to use if all of the Africa CDC
  fonts are not available in the system.

## Value

A character value for font family to use as Africa CDC font.

## Examples

``` r
acdc_fonts
#> $acdc_univers
#> [1] "Univers"
#> 
#> $acdc_din_next_arabic
#> [1] "Din Next Arabic"
#> 
#> $acdc_arial
#> [1] "Arial"
#> 

set_acdc_font()
#> [1] "Noto Sans"
```
