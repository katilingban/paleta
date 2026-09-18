# Get named colours vector

Get named colours vector

## Usage

``` r
get_colour(
  pattern = NULL,
  model = c("hex", "rgb", "cmyk", "pantone"),
  named = FALSE
)

get_colours(
  pattern = NULL,
  model = c("hex", "rgb", "cmyk", "pantone"),
  named = FALSE
)
```

## Arguments

- pattern:

  Optional. A character value or vector to use as a search term. Default
  is NULL in which case all the Oxford colours are returned.

- model:

  A character vector of colour model. Can be "rgb", "cmyk", "hex", or
  "pantone". Default is "hex".

- named:

  Logical. Should the output be a named character value or vector?
  Default is FALSE.

## Value

A character value or vector of colour/s as per `model` specification. If
`named` is TRUE, returns a named character value or vector.

## Examples

``` r
get_colours()
#>  [1] "#348F41" "#9F2241" "#B4A269" "#FFFFFF" "#58595B" "#1A5632" "#53575A"
#>  [8] "#194F90" "#AE1857" "#5B7E96" "#FFB71B" "#1DCAD3" "#FF5C35" "#8F4899"
#> [15] "#DAE343" "#385CAD" "#E81F76" "#009383" "#002244" "#009FDA" "#000000"
#> [22] "#FFFFFF" "#F05023" "#FDB714" "#EB1C2D" "#F78D28" "#009CA7" "#00AB51"
#> [29] "#872B90" "#00A996" "#98252B" "#E16A2D" "#B88C1D" "#614776" "#006068"
#> [36] "#006450" "#1CABE2" "#00833D" "#80BD41" "#FFC20E" "#F26A21" "#E2231A"
#> [43] "#961A49" "#6A1E74" "#D8D1C9" "#777779" "#2D2926" "#374EA2" "#005EB8"
#> [50] "#FFFFFF" "#003087" "#005EB8" "#0072CE" "#41B6E6" "#00A9CE" "#231f20"
#> [57] "#425563" "#768692" "#E8EDEE" "#006747" "#009639" "#78BE20" "#00A499"
#> [64] "#330072" "#7C2855" "#AE2573" "#8A1538" "#ED8B00" "#FFB81C" "#FAE100"
get_colours(model = "rgb")
#>  [1] "52, 143, 65"   "159, 34, 65"   "180, 162, 105" "255, 255, 255"
#>  [5] "88, 89, 91"    "26, 86, 50"    "83, 87, 90"    "25, 79, 144"  
#>  [9] "174, 24, 87"   "91, 126, 150"  "255, 183, 27"  "29, 202, 211" 
#> [13] "255, 92, 53"   "143, 72, 153"  "218, 227, 67"  "56, 92, 173"  
#> [17] "232, 31, 118"  "0, 147, 131"   "0 35 69"       "0 173 228"    
#> [21] "0, 0, 0"       "225, 225, 225" "240, 80, 35"   "253, 183, 20" 
#> [25] "235, 28, 45"   "247, 141, 40"  "0, 156, 167"   "0, 171, 81"   
#> [29] "135, 43, 144"  "0, 169, 150"   "152, 37, 43"   "225, 106, 45" 
#> [33] "184, 140, 29"  "97, 71, 118"   "0, 96, 104"    "0, 100, 80"   
#> [37] "0, 174, 239"   "0, 131, 62"    "128, 189, 65"  "255, 194, 14" 
#> [41] "242, 106, 33"  "226, 35, 26"   "150, 26, 73"   "107, 30, 116" 
#> [45] "216, 209, 202" "119, 119, 122" "45, 41, 38"    "55, 78, 162"  
#> [49] "0, 94, 184"    "255, 255, 255" "0, 48, 135"    "0, 94, 184"   
#> [53] "0, 114, 206"   "65, 182, 230"  "0, 169, 206"   "35, 31, 32"   
#> [57] "66, 85, 99"    "118, 134, 146" "232, 237, 238" "0, 103, 71"   
#> [61] "0, 150, 57"    "120, 190, 32"  "0, 164, 153"   "51, 0, 114"   
#> [65] "124, 40, 85"   "174, 37, 115"  "138, 21, 56"   "237, 139, 0"  
#> [69] "255, 184, 28"  "250, 225, 0"  
get_colours(pattern = "orange")
#>  [1] "#FF5C35" "#F05023" "#F78D28" "#E16A2D" "#F26A21" "#ED8B00" "#FF5C35"
#>  [8] "#F05023" "#F78D28" "#E16A2D" "#F26A21" "#ED8B00"
get_colours(pattern = c("orange", "brown"), named = TRUE)
#> ACDC Deep Orange WB Bright Orange  WB Light Orange   WB Dark Orange 
#>        "#FF5C35"        "#F05023"        "#F78D28"        "#E16A2D" 
#>    UNICEF Orange       NHS Orange         WB Brown ACDC Deep Orange 
#>        "#F26A21"        "#ED8B00"        "#B88C1D"        "#FF5C35" 
#> WB Bright Orange  WB Light Orange   WB Dark Orange    UNICEF Orange 
#>        "#F05023"        "#F78D28"        "#E16A2D"        "#F26A21" 
#>       NHS Orange         WB Brown 
#>        "#ED8B00"        "#B88C1D" 
get_colours(pattern = c("orange", "GREEN", "Blue"))
#>  [1] "#FF5C35" "#F05023" "#F78D28" "#E16A2D" "#F26A21" "#ED8B00" "#194F90"
#>  [8] "#5B7E96" "#002244" "#1CABE2" "#374EA2" "#005EB8" "#003087" "#005EB8"
#> [15] "#0072CE" "#41B6E6" "#00A9CE" "#348F41" "#1A5632" "#00AB51" "#006450"
#> [22] "#00833D" "#80BD41" "#006747" "#009639" "#78BE20" "#00A499" "#194F90"
#> [29] "#5B7E96" "#002244" "#1CABE2" "#374EA2" "#005EB8" "#003087" "#005EB8"
#> [36] "#0072CE" "#41B6E6" "#00A9CE" "#FF5C35" "#F05023" "#F78D28" "#E16A2D"
#> [43] "#F26A21" "#ED8B00" "#348F41" "#1A5632" "#00AB51" "#006450" "#00833D"
#> [50] "#80BD41" "#006747" "#009639" "#78BE20" "#00A499"
```
