# World Bank fonts

The function will search the system for availability of any of the World
Bank fonts in hierarchical order starting with *Andes*, and then
*Arial*. If none of these are found in the system, the function will
return *Noto Sans* by default or the user can set which font to use as
alternative by specifying `alt`.

## Usage

``` r
wb_fonts

set_wb_font(alt = paleta_fonts$paleta_noto)
```

## Arguments

- alt:

  A character value for font family to use if all of the World Bank
  fonts are not available in the system.

## Value

A character value for font family to use as World Bank font.

## Examples

``` r
wb_fonts
#> $wb_andes
#> [1] "Andes"
#> 
#> $wb_arial
#> [1] "Arial"
#> 

set_wb_font()
#> [1] "Noto Sans"
```
