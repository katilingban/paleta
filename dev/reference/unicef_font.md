# UNICEF fonts

The function will search the system for availability of any of the
UNICEF fonts in hierarchical order starting with *Andes*, and then
*Arial*. If none of these are found in the system, the function will
return *Noto Sans* by default or the user can set which font to use as
alternative by specifying `alt`.

## Usage

``` r
unicef_fonts

set_unicef_font(alt = paleta_fonts$paleta_noto)
```

## Arguments

- alt:

  A character value for font family to use if all of the UNICEF fonts
  are not available in the system.

## Value

A character value for font family to use as UNICEF font.

## Examples

``` r
unicef_fonts
#> $unicef_univers
#> [1] "Univers LT Pro"
#> 
#> $unicef_arial
#> [1] "Arial"
#> 
#> $unicef_roboto
#> [1] "Roboto"
#> 
#> $unicef_aleo
#> [1] "Aleo"
#> 

set_unicef_font()
#> [1] "Noto Sans"
```
