# NHS fonts

The function will search the system for availability of any of the NHS
fonts in hierarchical order starting with *Frutiger*, and then *Arial*.
If none of these are found in the system, the function will return *Noto
Sans* by default or the user can set which font to use as alternative by
specifying `alt`.

## Usage

``` r
nhs_fonts

set_nhs_font(alt = paleta_fonts$paleta_noto)
```

## Arguments

- alt:

  A character value for font family to use if all of the NHS fonts are
  not available in the system.

## Value

A character value for font family to use as NHS font.

## Examples

``` r
nhs_fonts
#> $nhs_frutiger
#> [1] "Frutiger"
#> 
#> $nhs_arial
#> [1] "Arial"
#> 

set_nhs_font()
#> [1] "Noto Sans"
```
