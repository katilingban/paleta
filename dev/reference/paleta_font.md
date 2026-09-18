# Set paleta font based on what is available in the system

The function will search the system for availability of any of the
paleta fonts in hierarchical order starting with *Roboto Condensed*,
then *Noto Sans*, and then *Roboto*.

## Usage

``` r
set_paleta_font()
```

## Value

A character value for font family to use as paleta font.

## Examples

``` r
set_paleta_font()
#> [1] "Roboto"
```
