# GNC fonts

The function will search the system for availability of any of the GNC
fonts in hierarchical order starting with *Arial*, then *Calibri
Regular*, then *Source Sans Pro*, and then finally *Arial*. If none of
these are found in the system, the function will return *Noto Sans* by
default or the user can set which font to use as alternative by
specifying `alt`.

## Usage

``` r
gnc_fonts

set_gnc_font(alt = paleta_fonts$paleta_noto)
```

## Arguments

- alt:

  A character value for font family to use if all of the GNC fonts are
  not available in the system.

## Value

A character value for font family to use as GNC font.

## Examples

``` r
gnc_fonts
#> $gnc_arial
#> [1] "Arial"
#> 
#> $gnc_calibri
#> [1] "Calibri Regular"
#> 
#> $acdc_source_sans_pro
#> [1] "Source Sans Pro"
#> 
#> $acdc_bebas_neue
#> [1] "Bebas Neue"
#> 

set_gnc_font()
#> [1] "Noto Sans"
```
