# Colours based on visual identity guidelines of various organisations

Colours based on visual identity guidelines of various organisations

## Usage

``` r
paleta_colours
```

## Format

A tibble with 5 fields

|  |  |
|----|----|
| **Variable** | **Description** |
| *name* | Organisation colour name |
| *code* | Organisation colour code |
| *rgb* | Three integers for the red, green, blue components of the RGB colour model |
| *cmyk* | Four integers for the cyan, magenta, yellow, and black components of the CMYK colour model |
| *hex* | Hexadecimal codes for corresponding colour |
| *pantone* | Pantone colour name |

## Examples

``` r
paleta_colours
#> # A tibble: 70 × 7
#>    organisation name                 code              rgb   cmyk  hex   pantone
#>  * <chr>        <chr>                <chr>             <chr> <chr> <chr> <chr>  
#>  1 Africa CDC   ACDC Green           acdc_green        52, … 81, … #348… 7740C  
#>  2 Africa CDC   ACDC Red             acdc_red          159,… 27, … #9F2… 7420C  
#>  3 Africa CDC   ACDC Gold            acdc_gold         180,… 31, … #B4A… 4515C  
#>  4 Africa CDC   ACDC White           acdc_white        255,… 0, 0… #FFF… 11-060…
#>  5 Africa CDC   ACDC Grey            acdc_grey         88, … 65, … #585… 425C   
#>  6 Africa CDC   ACDC Corporate Green acdc_corporate_g… 26, … 86, … #1A5… 3415C  
#>  7 Africa CDC   ACDC Text            acdc_text         83, … 66, … #535… 425C   
#>  8 Africa CDC   ACDC Blue            acdc_blue         25, … 98, … #194… 7686C  
#>  9 Africa CDC   ACDC Plum            acdc_plum         174,… 26, … #AE1… 215C   
#> 10 Africa CDC   ACDC Blue Grey       acdc_blue_grey    91, … 69, … #5B7… 5415C  
#> # ℹ 60 more rows

```
