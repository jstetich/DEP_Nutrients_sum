Initial Review of Maine DEP Nutrient Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
04/26/2021

-   [Introduction](#introduction)
-   [Folder References](#folder-references)
-   [Load Data](#load-data)
-   [Simplify Names](#simplify-names)
-   [Review Categorical Data](#review-categorical-data)
    -   [“Sample Types” and “QC Types”](#sample-types-and-qc-types)
        -   [QC TYpe](#qc-type)
        -   [Sample Type](#sample-type)
    -   [Sites and Sampled By](#sites-and-sampled-by)
    -   [Locations and Site Coding](#locations-and-site-coding)
    -   [Depth Units](#depth-units)
-   [Resolving Data With Qualifiers](#resolving-data-with-qualifiers)
    -   [Easy columns](#easy-columns)
    -   [Other Columns](#other-columns)
        -   [Turbidity](#turbidity)
        -   [Chlorophyll Data](#chlorophyll-data)
        -   [Nitrogen Data](#nitrogen-data)
-   [Review Irradiance Data](#review-irradiance-data)
-   [Remove Sonde Data?](#remove-sonde-data)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This notebook documents our initial review of DEP nutrient data,
learning what the data contains, and figuring out how to import and
recode data for our purposes. The notebook does not produce any output,
either graphics or tabular.

Results were used to inform data analysis and interpretation,
principally in the “DEP\_Nutrients\_Preparation.Rmd” and
“DEP\_Nutrients\_Review.Rmd” notebooks.

\#Load libraries

``` r
library(readxl)
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.0.5
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.0     v forcats 0.5.1
#> Warning: package 'ggplot2' was built under R version 4.0.5
#> Warning: package 'tidyr' was built under R version 4.0.5
#> Warning: package 'dplyr' was built under R version 4.0.5
#> Warning: package 'forcats' was built under R version 4.0.5
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

#library(mgcv)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Folder References

``` r
sibfldnm <- 'Original_Data'
parent <- dirname(getwd())
sibling <- file.path(parent,sibfldnm)

#dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

# Load Data

``` r
dep_data <- read_excel(file.path(sibling, "Curtis Bohlen 051021.xlsx"),
                       col_types = c("text", "text", "date", 
                                     "date", "text", "text", "text", 
                                     "numeric", "text", "numeric", "text", 
                                     "numeric", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "numeric", 
                                     "numeric", "numeric", "text", "text", 
                                     "numeric", "numeric")) 
```

``` r
dep_data <- dep_data %>%
  mutate(dt = as.Date(Date),
         month = as.numeric(format(dt, format = '%m')),
         month = factor(month, levels = 1:12, labels = month.abb),
         year =  as.numeric(format(dt, format = '%Y')),
         time = format(Time, format = '%H:%M'),
         hour = as.numeric(format(Time, format = '%H')
         )) %>%
  relocate(dt:hour, .after = Time)
```

# Simplify Names

``` r
dep_data <- dep_data %>% 
  rename(site_name = `Site ID`,
         site = `Sample Point ID`,
         depth = `Sample Depth`,
         temp = `Water Temperature (DEG C)`,
         salinity = `Salinity (PPTH)`,
         ph = `pH`,
         pctsat = `Dissolved Oxygen Saturation (%)`,
         do = `Dissolved Oxygen (MG/L)`,
         turbidity = `Turbidity (NTU)`,
         
         chl_a_sonde = `Chlorophyll (UG/L)(sonde)`,
         chl_a = `Chlorophyll A (UG/L)`,
         chl_less_phaeo = `Chlorophyll A - Phaeophytin (UG/L)`,
         phaeo = `Phaeophytin (UG/L)`,
         
         nox_n = `Nitrate + Nitrite As N (MG/L)`,
         nh4_n = `Ammonia as Nitrogen (MG/L)`,
         tkn = `Total Kjeldahl Nitrogen (MG/L)`,
         tn = `Total Nitrogen (MG/L)`,
         op_p = `Orthophosphate as Phosphorus (MG/L)`,
         tp = `Total Phosphorus as P (MG/L)`,
         tss = `Total Suspended Solids (MG/L)`,
         secchi = `Secchi (M)`,
         irr_air = `Irradiance (µmol/m2/s)(air)`, 
         irr_water = `Irradiance (µmol/m2/s) (surface water)`,
         irr_pct = `Irradiance (% of air in surface water)`)
```

We have a problem with some entries in the Excel Table including data
flags in the same column as the data, so we need to read those columns
in as text and clean them up and segregate the data quality codes.

# Review Categorical Data

## “Sample Types” and “QC Types”

``` r
xtabs(~ `Sample Type` + `QC Type`, data = dep_data, addNA = TRUE)
#>            QC Type
#> Sample Type    D    L   NA
#>          A     0    0 2694
#>          PC    0    0 3156
#>          SW   43  128 6250
```

### QC TYpe

The ‘QC Type’ values are rare, as expected. According to DEP’s EDD
template, which contains a Data Element Dictionary, this flags “quality
control data.” The value “NA” is used to indicate a normal environmental
sample. The data dictionary does not provide information on the specific
meaning of most codes.

Reviewing the data in Excel shows ‘L’ samples occur paired with other
samples, often including annotation of “RPD”. This suggests these are
laboratory duplicates. We could retain and replace with arithmetic means
of the duplicate samples, or drop these from analysis. Given the
complexity of correctly reading in data with the extraneous “RPD”
annotations in the data column, it is easier to drop the laboratory
duplicates.

The “D” values appear often to occur in strings of multiple observations
collected at more or less the same time. They include nutrients. Often
they have notes associated with them that suggest handling problems or
other reasons to mistrust analytic values. Some have sample site codes
supplemented with " - SUR’. Almost all have depth = 0.2. These also
appear to be some sort of duplicate sample - -perhaps field duplicates.
Since we do not understand their provenance and purpose, we will drop
them as well.

### Sample Type

The Sample Type code includes information on the sample medium. “SW” is
apparently used to designate “Seawater”, while “A” (according to the
data dictionary) is for “air”. “PC” is not defined in the data
dictionary, but the code is apparently used for irradiance data with
depth as a percentage of surface irradiance. Looking at the Excel file,
Each ‘PC’ sample is paired with a ‘SW’ sample one comes paired with a
data row with identical site, date, and time, but with irradiance in
(µmol/m2/s). Interestingly, in these data, we often lack the surface
(air) irradiance, so we can not regenerate these values from raw data).
For our purposes, none of these codes are useful.

## Sites and Sampled By

``` r
xtabs(~  site_name + `Sampled By` , data = dep_data, addNA = TRUE)
#>                                 Sampled By
#> site_name                        DEA ENGINEERING UNIT
#>   ANTHOINE CREEK - AC01                             0
#>   ANTHOINE CREEK - AC02                             0
#>   ANTHOINE CREEK - AC04                             0
#>   ANTHOINE CREEK - AC05                             0
#>   BANDM RAILROAD TRESTLE - BMR02                    0
#>   CLAPBOARD ISLAND - P7CBI                          0
#>   COUSINS RIVER - CR-31                             1
#>   COUSINS RIVER - CR-44                             0
#>   COUSINS RIVER - CR00                              0
#>   COUSINS RIVER - CRTRIB0                           0
#>   EAST END BEACH - EEB18                            0
#>   FORE RIVER - FR01                                 0
#>   FORE RIVER - FR02                                 0
#>   FORE RIVER - FR03                                 0
#>   FORE RIVER - FR04                                 0
#>   FORE RIVER - FR05A                                0
#>   FORE RIVER - FR05B                                0
#>   FORE RIVER - FR06                                 0
#>   FORE RIVER - FR07                                 0
#>   FORE RIVER - FR08                                 0
#>   FORE RIVER - FR09                                 0
#>   FORT GORGES - P6FGG                               0
#>   HARRASEEKET RIVER - HR01                          0
#>   HARRASEEKET RIVER - HR02                          0
#>   HARRASEEKET RIVER - HR03                          0
#>   HARRASEEKET RIVER - HR04                          0
#>   HARRASEEKET RIVER - HR05                          0
#>   LONG CREEK - LC01                                 0
#>   LONG CREEK - LC02                                 0
#>   PRESUMPSCOT RIVER - CBPR                          0
#>   PRESUMPSCOT RIVER - PR-13                         0
#>   PRESUMPSCOT RIVER - PR-17                         0
#>   PRESUMPSCOT RIVER - PR-19                         0
#>   PRESUMPSCOT RIVER - PR-28                         0
#>   PRESUMPSCOT RIVER - PR01                          2
#>   ROYAL RIVER - RR-01                               0
#>   ROYAL RIVER - RR-06                               1
#>   ROYAL RIVER - RR-13                               0
#>   ROYAL RIVER - RR-19                               1
#>   ROYAL RIVER - RR-20                               0
#>   ROYAL RIVER - RR00                                1
#>   TROUT BROOK - TB01                                0
#>   WALTON PARK - PRV70                               0
#>                                 Sampled By
#> site_name                        FRED DILLON AND GRETCHEN ANDERSON, MARINE UNIT
#>   ANTHOINE CREEK - AC01                                                       0
#>   ANTHOINE CREEK - AC02                                                       0
#>   ANTHOINE CREEK - AC04                                                       0
#>   ANTHOINE CREEK - AC05                                                       2
#>   BANDM RAILROAD TRESTLE - BMR02                                              0
#>   CLAPBOARD ISLAND - P7CBI                                                    0
#>   COUSINS RIVER - CR-31                                                       0
#>   COUSINS RIVER - CR-44                                                       0
#>   COUSINS RIVER - CR00                                                        0
#>   COUSINS RIVER - CRTRIB0                                                     0
#>   EAST END BEACH - EEB18                                                      0
#>   FORE RIVER - FR01                                                           0
#>   FORE RIVER - FR02                                                           0
#>   FORE RIVER - FR03                                                           0
#>   FORE RIVER - FR04                                                           0
#>   FORE RIVER - FR05A                                                          0
#>   FORE RIVER - FR05B                                                          0
#>   FORE RIVER - FR06                                                           0
#>   FORE RIVER - FR07                                                           0
#>   FORE RIVER - FR08                                                           0
#>   FORE RIVER - FR09                                                           0
#>   FORT GORGES - P6FGG                                                         0
#>   HARRASEEKET RIVER - HR01                                                    0
#>   HARRASEEKET RIVER - HR02                                                    0
#>   HARRASEEKET RIVER - HR03                                                    0
#>   HARRASEEKET RIVER - HR04                                                    0
#>   HARRASEEKET RIVER - HR05                                                    0
#>   LONG CREEK - LC01                                                           1
#>   LONG CREEK - LC02                                                           0
#>   PRESUMPSCOT RIVER - CBPR                                                    0
#>   PRESUMPSCOT RIVER - PR-13                                                   0
#>   PRESUMPSCOT RIVER - PR-17                                                   0
#>   PRESUMPSCOT RIVER - PR-19                                                   0
#>   PRESUMPSCOT RIVER - PR-28                                                   0
#>   PRESUMPSCOT RIVER - PR01                                                    0
#>   ROYAL RIVER - RR-01                                                         0
#>   ROYAL RIVER - RR-06                                                         0
#>   ROYAL RIVER - RR-13                                                         0
#>   ROYAL RIVER - RR-19                                                         0
#>   ROYAL RIVER - RR-20                                                         0
#>   ROYAL RIVER - RR00                                                          0
#>   TROUT BROOK - TB01                                                          2
#>   WALTON PARK - PRV70                                                         0
#>                                 Sampled By
#> site_name                        FRIENDS OF CASCO BAY, MARINE UNIT MARINE UNIT
#>   ANTHOINE CREEK - AC01                                          0           4
#>   ANTHOINE CREEK - AC02                                          0           4
#>   ANTHOINE CREEK - AC04                                          0           4
#>   ANTHOINE CREEK - AC05                                          0           5
#>   BANDM RAILROAD TRESTLE - BMR02                                 0         960
#>   CLAPBOARD ISLAND - P7CBI                                      63         773
#>   COUSINS RIVER - CR-31                                          0         190
#>   COUSINS RIVER - CR-44                                          0          94
#>   COUSINS RIVER - CR00                                           0          28
#>   COUSINS RIVER - CRTRIB0                                        0          32
#>   EAST END BEACH - EEB18                                         0         837
#>   FORE RIVER - FR01                                              5           3
#>   FORE RIVER - FR02                                              6           1
#>   FORE RIVER - FR03                                              0         163
#>   FORE RIVER - FR04                                              0         229
#>   FORE RIVER - FR05A                                             0          75
#>   FORE RIVER - FR05B                                             0          57
#>   FORE RIVER - FR06                                              4           2
#>   FORE RIVER - FR07                                              0         189
#>   FORE RIVER - FR08                                              4           2
#>   FORE RIVER - FR09                                              0        1805
#>   FORT GORGES - P6FGG                                           67         767
#>   HARRASEEKET RIVER - HR01                                       0          12
#>   HARRASEEKET RIVER - HR02                                       0         115
#>   HARRASEEKET RIVER - HR03                                       0         148
#>   HARRASEEKET RIVER - HR04                                       0         178
#>   HARRASEEKET RIVER - HR05                                       0         181
#>   LONG CREEK - LC01                                              1           1
#>   LONG CREEK - LC02                                              0         122
#>   PRESUMPSCOT RIVER - CBPR                                       0         914
#>   PRESUMPSCOT RIVER - PR-13                                      0           3
#>   PRESUMPSCOT RIVER - PR-17                                      0         897
#>   PRESUMPSCOT RIVER - PR-19                                      0           2
#>   PRESUMPSCOT RIVER - PR-28                                      0        1292
#>   PRESUMPSCOT RIVER - PR01                                       0           0
#>   ROYAL RIVER - RR-01                                            0         189
#>   ROYAL RIVER - RR-06                                            0         280
#>   ROYAL RIVER - RR-13                                            0         257
#>   ROYAL RIVER - RR-19                                            0         147
#>   ROYAL RIVER - RR-20                                            0         114
#>   ROYAL RIVER - RR00                                             0           0
#>   TROUT BROOK - TB01                                             0           0
#>   WALTON PARK - PRV70                                            0         994
#>                                 Sampled By
#> site_name                        UNIVERSITY OF MAINE <NA>
#>   ANTHOINE CREEK - AC01                            0    0
#>   ANTHOINE CREEK - AC02                            0    0
#>   ANTHOINE CREEK - AC04                            0    0
#>   ANTHOINE CREEK - AC05                            0    0
#>   BANDM RAILROAD TRESTLE - BMR02                   0    2
#>   CLAPBOARD ISLAND - P7CBI                         0    2
#>   COUSINS RIVER - CR-31                            0    0
#>   COUSINS RIVER - CR-44                            0    0
#>   COUSINS RIVER - CR00                             0    0
#>   COUSINS RIVER - CRTRIB0                          0    0
#>   EAST END BEACH - EEB18                           0    4
#>   FORE RIVER - FR01                                0    0
#>   FORE RIVER - FR02                                0    0
#>   FORE RIVER - FR03                                0    0
#>   FORE RIVER - FR04                                0    0
#>   FORE RIVER - FR05A                               0    0
#>   FORE RIVER - FR05B                               0    0
#>   FORE RIVER - FR06                                0    0
#>   FORE RIVER - FR07                                0    0
#>   FORE RIVER - FR08                                0    0
#>   FORE RIVER - FR09                                0    7
#>   FORT GORGES - P6FGG                              0    4
#>   HARRASEEKET RIVER - HR01                         0    0
#>   HARRASEEKET RIVER - HR02                         0    0
#>   HARRASEEKET RIVER - HR03                         0    0
#>   HARRASEEKET RIVER - HR04                         0    0
#>   HARRASEEKET RIVER - HR05                         0    0
#>   LONG CREEK - LC01                                0    0
#>   LONG CREEK - LC02                                0    0
#>   PRESUMPSCOT RIVER - CBPR                         0    4
#>   PRESUMPSCOT RIVER - PR-13                        0    0
#>   PRESUMPSCOT RIVER - PR-17                        0    3
#>   PRESUMPSCOT RIVER - PR-19                        0    0
#>   PRESUMPSCOT RIVER - PR-28                        0    2
#>   PRESUMPSCOT RIVER - PR01                         0    0
#>   ROYAL RIVER - RR-01                              0    0
#>   ROYAL RIVER - RR-06                              0    0
#>   ROYAL RIVER - RR-13                              0    0
#>   ROYAL RIVER - RR-19                              0    0
#>   ROYAL RIVER - RR-20                              0    0
#>   ROYAL RIVER - RR00                              10    0
#>   TROUT BROOK - TB01                               0    0
#>   WALTON PARK - PRV70                              0    2
```

We note high sample frequency attributed to “FRIENDS OF CASCO BAY,
MARINE UNIT” at two sites: \* “CLAPBOARD ISLAND \_ P7CBI” and \* “FORT
GORGES - P6FGG”

Those have familiar site codes from the FOCB data. We need to check to
make sure we are not double counting those data. The other samples
attributed to FOCB do not look like familiar sampling codes, so we will
need to check GIS locations.

## Locations and Site Coding

``` r
dep_data %>%
  select(site_name, site, Latitude, Longitude) %>%
  unique() %>%
  arrange(site, site_name)
#> # A tibble: 54 x 4
#>    site_name                      site        Latitude Longitude
#>    <chr>                          <chr>          <dbl>     <dbl>
#>  1 ANTHOINE CREEK - AC01          AC01            43.6     -70.3
#>  2 ANTHOINE CREEK - AC02          AC02            43.6     -70.3
#>  3 ANTHOINE CREEK - AC04          AC04            43.6     -70.3
#>  4 ANTHOINE CREEK - AC05          AC05            43.6     -70.3
#>  5 BANDM RAILROAD TRESTLE - BMR02 BMR02           43.7     -70.3
#>  6 BANDM RAILROAD TRESTLE - BMR02 BMR02 - SUR     43.7     -70.3
#>  7 PRESUMPSCOT RIVER - CBPR       CBPR            43.7     -70.2
#>  8 COUSINS RIVER - CR-31          CR-31           43.8     -70.2
#>  9 COUSINS RIVER - CR-44          CR-44           43.8     -70.1
#> 10 COUSINS RIVER - CR00           CR00            43.8     -70.1
#> # ... with 44 more rows
```

The `site_name` (Originally “Site ID” in the DEP data) – as is common
with DEP data – includes a text name for each site, along with a text
code for the same site, separated by a dash. The content after the dash
is **nearly** replicated in the `site` variable.

Each `site_name` encompasses (in a few cases) multiple annotated `site`
values (“Sample Point ID” in the original data). “Sample Point ID”
values are sometimes decorated with " - SUR“,” - BOT“, and” - MAX“,
apparently for”Surface“,”Bottom“, and”Maximum" samples respectively. One
sample location, at Royal River, has the annotation "\_A" instead.

The “MAX” samples, restricted to FR09, do not refer to maximum depth.
They are associated with what appear to be sonde casts, and include
chlorophyll values. It appears approximate chlorophyll values from the
sonde casts were used to identify the approximate depth of maximum
chlorophyll concentrations.

At that site, “SUR” samples include (laboratory?) analyses of
chlorophyll and nutrients, while “BOT” samples contain measurements of
nutrients.

The “RR00\_A” samples are apparently a time series of nutrient values
from 2017. Only a single other sample is reported from RROO, from
September of 2015.

## Depth Units

``` r
unique(dep_data$`Depth Unit`)
#> [1] "M" NA
```

So that data is not useful. All depths are in meters.

# Resolving Data With Qualifiers

We want to look at the letter codes embedded in otherwise quantitative
data. We use regex to extract non-numeric data and look at what we find.
This may hide details like separation by commas, dashes and period that
may appear in numbers.

``` r
tmp <- dep_data %>%
  # delete the QA samples
  filter(`QC Type` == 'NA')
  
bad_numeric_data <- tmp %>%  
  #Focus only on quantitative variables
  select ( -c(site_name:`Sample Type`)) %>%
  select(-`Sampled By`, -`Depth Unit`, -`Sample Comments`, 
         -`Validation Comments`) %>%
  # grab the ones that were read in as character vectors
  select_if(is.character)
```

We use a regex to identify digits, spaces, and decimals. We use `gsub()`
to replace them all with the empty string.

``` r
bad_numeric_data <- bad_numeric_data %>%
  mutate(across(! contains('QC'), ~ gsub('[ .[:digit:]]*', '', .x))) %>%
  rowwise() %>%
  mutate(has_bad_data = any(! is.na(c_across(! contains('QC'))))) %>%
  filter(has_bad_data) %>%
  ungroup()
```

Now, we extract unique strings in each column

``` r
test <- bad_numeric_data %>%
  mutate(across(everything(), ~factor(.x)))
         
for (n in names(test)) {
  cat(n, '\n')
  print(levels(test[[n]]))
}
#> QC Type 
#> [1] "NA"
#> salinity 
#> [1] ""  ","
#> pctsat 
#> [1] ""  ","
#> do 
#> [1] ""  ","
#> turbidity 
#> [1] ""   ","  "E-" "U<"
#> chl_a_sonde 
#> [1] "J"   "J,J" "UJ" 
#> chl_a 
#> [1] ""  "J"
#> chl_less_phaeo 
#> [1] ""   "E-" "J"  "U<"
#> phaeo 
#> [1] ""   "J"  "U<"
#> nox_n 
#> [1] ""   "E-" "J"  "JB" "U<" "UJ"
#> nh4_n 
#> [1] ""   "B"  "E-" "J"  "U<"
#> tkn 
#> [1] ""   "B"  "U<"
#> tn 
#> [1] ""  "J"
#> op_p 
#> [1] "*"  "E-" "J"  "J*" "JB" "UJ"
#> tp 
#> [1] ""   "B"  "E-" "J"  "U<"
#> tss 
#> [1] ""      "J"     "RPD,J" "U<"   
#> secchi 
#> [1] ""  ">"
#> has_bad_data 
#> [1] "TRUE"
```

## Easy columns

`salinity`, `pctsat` and `do` all contain no problems (after the QA/QC
data were removed) and can be converted to numeric values.

``` r
test <- tmp %>%
  select(salinity, pctsat, do) %>%
  mutate(across(everything(), as.numeric))
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

## Other Columns

We see several common data qualifier codes: E – Exponentiation in
original string – properly evaluated when read with `as.numeric()`  
J – Usually an “estimated” value, often data where value may be
inaccurate due to a QC problem, or a value is between the detection
limit and the quantitation limit.  
B – Often, especially with inorganic compounds, a marker for a value
between instrument detection limit and official detection limit. U –
non-detect.  
U&lt; – Non Detect.  
&gt; - Right censored (Secchi depth).

In some cases these may be combined.

### Turbidity

Turbidity includes both E codes and U codes.

#### “E” codes

The E codes are signals for powers of ten, and will be interpreted
correctly by `as.numeric()`, so we need not worry about them.

``` r
tmp %>%
  select(turbidity) %>%
  filter(grepl('E', turbidity))
#> # A tibble: 136 x 1
#>    turbidity
#>    <chr>    
#>  1 1E-3     
#>  2 1E-3     
#>  3 1E-3     
#>  4 1E-3     
#>  5 1E-3     
#>  6 1E-3     
#>  7 1E-3     
#>  8 1E-3     
#>  9 1E-3     
#> 10 1E-3     
#> # ... with 126 more rows
```

``` r
as.numeric('1E-3')
#> [1] 0.001
```

\#\#\#\#“U&lt;” Codes

``` r
tmp %>%
  select(turbidity) %>%
  filter(grepl('U', turbidity))
#> # A tibble: 3 x 1
#>   turbidity
#>   <chr>    
#> 1 U<0.01   
#> 2 U<0.01   
#> 3 U<0.01
```

That’s only three left censored values out of all turbidity values. The
frequency is so low that our handling of these values can not matter to
our analysis.

``` r
tmp %>%
  select(turbidity) %>%
  filter(! is.na(turbidity)) %>%
  pull(turbidity) %>%
  length
#> [1] 2039
```

For simplicity’s sake, we replace the non-detects with the implied
detection limits.

``` r
test <- tmp %>%
  select(turbidity) %>%
  mutate(turbidity2 = if_else(grepl('U', turbidity), 
                             as.numeric(substr(turbidity, 3, nchar(turbidity))),
                             as.numeric(turbidity))) %>%
  filter(! is.na(turbidity))
#> Warning in if_else(grepl("U", turbidity), as.numeric(substr(turbidity, 3, : NAs
#> introduced by coercion
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

The warning points to a single line which includes two values separated
by a comma.

``` r
tmp %>%
  select(site, dt, depth, turbidity) %>%
  filter(grepl(',', turbidity))
#> # A tibble: 1 x 4
#>   site  dt         depth turbidity 
#>   <chr> <date>     <dbl> <chr>     
#> 1 BMR02 2020-09-14  2.84 1.86, 2.17
```

This is a sample where all (?) sonde-related values were paired in the
original data. We do not know why the values are paired. Although we
could rescue the values with a little additional code, it costs us
little to simply drop these values, given the sample sizes involved.

### Chlorophyll Data

``` r
test <- bad_numeric_data %>%
  mutate(across(everything(), ~factor(.x))) %>%
  select(chl_a_sonde:phaeo)
         
for (n in names(test)) {
  cat(n, '\n')
  print(levels(test[[n]]))
}
#> chl_a_sonde 
#> [1] "J"   "J,J" "UJ" 
#> chl_a 
#> [1] ""  "J"
#> chl_less_phaeo 
#> [1] ""   "E-" "J"  "U<"
#> phaeo 
#> [1] ""   "J"  "U<"
```

#### Sonde Data

The Chlorophyll Sonde data includes both U and J values. The “UJ” code
is never associated with data, but corresponds to an (unknown) value
below quantitation limits. There is only a single UJ value, which we can
probably omit from analysis with little effect on results.

But there are MANY J values, and a few samples with twinned values
separated by commas. For the majority of these samples, despite the data
quality flags, there is no comment that explains why these samples were
flagged. We assume (based on the usual use of a “J” flag) they were
flagged because values qualify as “estimated” or otherwise less
accurate.

``` r
tmp %>%
  select(chl_a_sonde, `Sample Comments`, `Validation Comments`) %>%
  filter(grepl('J', chl_a_sonde)) %>%
  head(10)
#> # A tibble: 10 x 3
#>    chl_a_sonde `Sample Comments`   `Validation Comments`
#>    <chr>       <chr>               <chr>                
#>  1 J 1.11      EBB TIDE SAMPLING   <NA>                 
#>  2 J 1.14      EBB TIDE SAMPLING   <NA>                 
#>  3 J 0.95      EBB TIDE SAMPLING   <NA>                 
#>  4 J 1.09      EBB TIDE SAMPLING   <NA>                 
#>  5 J 1.5       EBB TIDE SAMPLING   <NA>                 
#>  6 J 1.14      EBB TIDE SAMPLING   <NA>                 
#>  7 J 6.56      FLOOD TIDE SAMPLING <NA>                 
#>  8 J 7.93      FLOOD TIDE SAMPLING <NA>                 
#>  9 J 9.49      FLOOD TIDE SAMPLING <NA>                 
#> 10 J 9.85      FLOOD TIDE SAMPLING <NA>
```

As we explored these data, we learned that all of them are flagged. We
need to whether and how to use these flagged values. For now, we simply
extract numeric values.

``` r
test <- tmp %>%
  select(chl_a_sonde) %>%
  mutate(chl_a_sonde_2 = as.numeric(substr(chl_a_sonde, 
                                           3, 
                                           nchar(chl_a_sonde)))) %>%
  filter(! is.na(chl_a_sonde))
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

#### Analytic Chlorophyll Data

``` r
tmp %>%
  select(chl_a) %>%
  filter(! is.na(chl_a)) %>%
  pull(chl_a) %>%
  length
#> [1] 17
```

``` r
tmp %>%
  select(chl_less_phaeo) %>%
  filter(! is.na(chl_less_phaeo)) %>%
  pull(chl_less_phaeo) %>%
  length
#> [1] 313
```

``` r
tmp %>%
  select(phaeo) %>%
  filter(! is.na(phaeo)) %>%
  pull(phaeo) %>%
  length
#> [1] 323
```

It appears that chl\_a and chl\_less\_phaeo are complements, we never
have both at the same time, but either one can occur in a pair with the
phaeophyton.

``` r
tmp %>%
  select(chl_a, chl_less_phaeo, phaeo) %>%
  filter(! (is.na(chl_a) & is.na(chl_less_phaeo)& is.na(phaeo)))
#> # A tibble: 330 x 3
#>    chl_a chl_less_phaeo      phaeo              
#>    <chr> <chr>               <chr>              
#>  1 <NA>  2.1800000000000002  0.77               
#>  2 <NA>  1.9                 1.01               
#>  3 <NA>  0.97899999999999998 0.44400000000000001
#>  4 <NA>  1.6                 0.443              
#>  5 <NA>  2.2799999999999998  0.78900000000000003
#>  6 <NA>  1.0900000000000001  <NA>               
#>  7 <NA>  2.54                0.67               
#>  8 <NA>  4.46                0.9                
#>  9 <NA>  2.59                0.79               
#> 10 <NA>  2.58                0.64               
#> # ... with 320 more rows
```

##### Combine Chlorophyll Columns

Chlorophyll A perhaps can be based on the combination of those two data
columns. We need to deal with both flags and left censored values

``` r
test <- tmp %>%
  select(chl_a, chl_less_phaeo) %>%
  
  mutate(chl_a_flag = grepl('J', chl_a),
         chl_a_cens = grepl('U', chl_a),
         chl_a_2 = if_else(chl_a_flag | chl_a_cens,
                           as.numeric(substr(chl_a, 3, nchar(chl_a))), 
                           as.numeric(chl_a)))  %>%
  
  mutate(chl_less_phaeo_flag = grepl('J', chl_less_phaeo),
         chl_less_phaeo_cens = grepl('U', chl_less_phaeo),
         chl_less_phaeo_2 = if_else(chl_less_phaeo_flag | chl_less_phaeo_cens,
                                    as.numeric(substr(chl_less_phaeo, 3, nchar(chl_less_phaeo))), 
                                    as.numeric(chl_less_phaeo)))  %>%
  mutate(chl_syn = if_else(is.na(chl_less_phaeo_2), 
                           chl_a_2,
                           chl_less_phaeo_2),
         chl_syn_flag = if_else(is.na(chl_less_phaeo_2), 
                                chl_a_flag ,
                                chl_less_phaeo_flag),
         chl_syn_cens= if_else(is.na(chl_less_phaeo_2), 
                               chl_a_cens ,
                               chl_less_phaeo_cens)) %>%
  filter(! is.na(chl_syn)) %>%
  select(chl_syn, chl_syn_flag, chl_syn_cens)
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion

#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

``` r
xtabs(~chl_syn_flag + chl_syn_cens, data = test)
#>             chl_syn_cens
#> chl_syn_flag FALSE TRUE
#>        FALSE   283    1
#>        TRUE     46    0
```

Again, we see there is only one censored value, so we simply replace it
with its nominal detection limit.

#### Phaeophyton

``` r
test <- tmp %>%
  select(phaeo) %>%
  
  mutate(phaeo_flag = grepl('J', phaeo),
         phaeo_cens = grepl('U', phaeo),
         phaeo_2 = if_else(phaeo_flag | phaeo_cens,
                           as.numeric(substr(phaeo, 3, nchar(phaeo))), 
                           as.numeric(phaeo)))  %>%
 filter(! is.na(phaeo))
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

``` r
xtabs(~phaeo_flag + phaeo_cens, data = test)
#>           phaeo_cens
#> phaeo_flag FALSE TRUE
#>      FALSE   269    8
#>      TRUE     46    0
```

Note that we again have a small number of censored data, and can handle
them appropriately by simply replacing them by their nominal detection
limits.

### Nitrogen Data

``` r
test <- bad_numeric_data %>%
  select(nox_n:tn) %>%
  mutate(across(everything(), ~factor(.x)))
         
for (n in names(test)) {
  cat(n, '\n')
  print(levels(test[[n]]))
}
#> nox_n 
#> [1] ""   "E-" "J"  "JB" "U<" "UJ"
#> nh4_n 
#> [1] ""   "B"  "E-" "J"  "U<"
#> tkn 
#> [1] ""   "B"  "U<"
#> tn 
#> [1] ""  "J"
```

#### Nitrate

``` r
tmp %>%
  select(nox_n) %>%
  filter(grepl('B', nox_n) & grepl('J', nox_n))
#> # A tibble: 18 x 1
#>    nox_n   
#>    <chr>   
#>  1 JB 0.046
#>  2 JB 0.035
#>  3 JB 0.038
#>  4 JB 0.035
#>  5 JB 0.027
#>  6 JB 0.034
#>  7 JB 0.033
#>  8 JB 0.027
#>  9 JB 0.044
#> 10 JB 0.033
#> 11 JB 0.035
#> 12 JB 0.028
#> 13 JB 0.027
#> 14 JB 0.033
#> 15 JB 0.038
#> 16 JB 0.028
#> 17 JB 0.03 
#> 18 JB 0.036
```

``` r
tmp %>%
  select(nox_n) %>%
  filter(grepl('U', nox_n))
#> # A tibble: 31 x 1
#>    nox_n  
#>    <chr>  
#>  1 U<0.002
#>  2 U<0.002
#>  3 U<0.002
#>  4 U<0.002
#>  5 U<0.002
#>  6 U<0.01 
#>  7 U<0.01 
#>  8 U<0.002
#>  9 U<0.002
#> 10 U<0.005
#> # ... with 21 more rows
```

``` r
tmp %>%
  select(nox_n) %>%
  filter (! is.na(nox_n)) %>%
  pull(nox_n) %>%
  length()
#> [1] 396
tmp %>%
  select(nox_n) %>%
  filter(grepl('U', nox_n)) %>%
  pull(nox_n) %>%
  length()
#> [1] 31
tmp %>%
  select(nox_n) %>%
  filter(grepl('J', nox_n)) %>%
  pull(nox_n) %>%
  length()
#> [1] 45
tmp %>%
  select(nox_n) %>%
  filter(grepl('B', nox_n)) %>%
  pull(nox_n) %>%
  length()
#> [1] 18
```

So about 8% of the observation are non-detects, and closer to 12% are
flagged as questionable data.

``` r
test <- tmp %>%
  select(nox_n) %>%
  
  mutate(nox_n_j_flag = grepl('J', nox_n),
         nox_n_jb_flag = grepl('JB', nox_n),
         nox_n_cens = grepl('U', nox_n),
         nox_n_2 = if_else(nox_n_j_flag | nox_n_cens,
                           as.numeric(substr(nox_n, 3, nchar(nox_n))),
                           if_else(nox_n_jb_flag,
                                   as.numeric(substr(nox_n, 4, nchar(nox_n))),
                                   as.numeric(nox_n))))  %>%
  select(-nox_n_jb_flag) %>%
 filter(! is.na(nox_n))
#> Warning in if_else(nox_n_jb_flag, as.numeric(substr(nox_n, 4, nchar(nox_n))), :
#> NAs introduced by coercion
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

#### Ammonium

``` r
tmp %>%
  select(nh4_n) %>%
  filter (! is.na(nh4_n)) %>%
  pull(nh4_n) %>%
  length()
#> [1] 377
tmp %>%
  select(nh4_n) %>%
  filter(grepl('U', nh4_n)) %>%
  pull(nh4_n) %>%
  length()
#> [1] 69
tmp %>%
  select(nh4_n) %>%
  filter(grepl('J', nh4_n)) %>%
  pull(nh4_n) %>%
  length()
#> [1] 8
tmp %>%
  select(nh4_n) %>%
  filter(grepl('B', nh4_n)) %>%
  pull(nh4_n) %>%
  length()
#> [1] 8
```

#### Total Kjeldahl Nitrogen

``` r
tmp %>%
  select(tkn) %>%
  filter (! is.na(tkn)) %>%
  pull(tkn) %>%
  length()
#> [1] 19
tmp %>%
  select(tkn) %>%
  filter(grepl('U', tkn)) %>%
  pull(tkn) %>%
  length()
#> [1] 1
tmp %>%
  select(tkn) %>%
  filter(grepl('J', tkn)) %>%
  pull(tkn) %>%
  length()
#> [1] 0
tmp %>%
  select(tkn) %>%
  filter(grepl('B', tkn)) %>%
  pull(tkn) %>%
  length()
#> [1] 8
```

#### Total Nitrogen

``` r
tmp %>%
  select(tn) %>%
  filter (! is.na(tn)) %>%
  pull(tn) %>%
  length()
#> [1] 402
tmp %>%
  select(tn) %>%
  filter(grepl('U', tn)) %>%
  pull(tn) %>%
  length()
#> [1] 0
tmp %>%
  select(tn) %>%
  filter(grepl('J', tn)) %>%
  pull(tn) %>%
  length()
#> [1] 53
tmp %>%
  select(tn) %>%
  filter(grepl('B', tn)) %>%
  pull(tn) %>%
  length()
#> [1] 0
```

# Review Irradiance Data

The data review suggests we should pull a separate data series for the
irradiance data, since it occurs in pairs and triplets.

A few of the data reported with irradiance info (about 54 rows) contain
other data, apparently sonde observations. It makes sense that some of
the time sonde data and irradiance data would be acquired
simultaneously.

``` r
irr_data <- dep_data %>%
  filter(! (is.na(irr_air) & 
              is.na(irr_water) & 
              is.na(irr_pct)))
irr_data %>%
  summarize(across(everything(), ~ sum(! is.na(.x))))
#> # A tibble: 1 x 39
#>   site_name  site  Date  Time    dt month  year  time  hour `Sample Type`
#>       <int> <int> <int> <int> <int> <int> <int> <int> <int>         <int>
#> 1      8396  8396  8396  8396  8396  8396  8396  8396  8396          8396
#> # ... with 29 more variables: QC Type <int>, Sampled By <int>, depth <int>,
#> #   Depth Unit <int>, temp <int>, salinity <int>, ph <int>, pctsat <int>,
#> #   do <int>, turbidity <int>, chl_a_sonde <int>, chl_a <int>,
#> #   chl_less_phaeo <int>, phaeo <int>, nox_n <int>, nh4_n <int>, tkn <int>,
#> #   tn <int>, op_p <int>, tp <int>, tss <int>, secchi <int>, irr_air <int>,
#> #   irr_water <int>, irr_pct <int>, Sample Comments <int>,
#> #   Validation Comments <int>, Latitude <int>, Longitude <int>
```

``` r
irr_data <- irr_data %>%
  select(c(site_name:hour), `QC Type`, depth, 
         c(temp:chl_a),
         c(irr_air:`Sample Comments`)
         ) %>%
  arrange(`site_name`, Date, Time, depth)
irr_data
#> # A tibble: 8,396 x 23
#>    site_name      site  Date                Time                dt         month
#>    <chr>          <chr> <dttm>              <dttm>              <date>     <fct>
#>  1 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:22:00 2017-06-02 Jun  
#>  2 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:22:00 2017-06-02 Jun  
#>  3 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:23:00 2017-06-02 Jun  
#>  4 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:23:00 2017-06-02 Jun  
#>  5 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:24:00 2017-06-02 Jun  
#>  6 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:24:00 2017-06-02 Jun  
#>  7 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:25:00 2017-06-02 Jun  
#>  8 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:25:00 2017-06-02 Jun  
#>  9 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:26:00 2017-06-02 Jun  
#> 10 BANDM RAILROA~ BMR02 2017-06-02 00:00:00 1899-12-31 09:26:00 2017-06-02 Jun  
#> # ... with 8,386 more rows, and 17 more variables: year <dbl>, time <chr>,
#> #   hour <dbl>, QC Type <chr>, depth <dbl>, temp <dbl>, salinity <chr>,
#> #   ph <dbl>, pctsat <chr>, do <chr>, turbidity <chr>, chl_a_sonde <chr>,
#> #   chl_a <chr>, irr_air <dbl>, irr_water <dbl>, irr_pct <dbl>,
#> #   Sample Comments <chr>
```

Looking at those data, the data is sometimes incomplete, leaving out the
simultaneous observation of surface irradiance, but in general, each
observation should have three values: surface air, surface water, and
percent of air. Currently, they are on three separate rows.

The supplementary data from the sondes is available only in association
with the rows with surface water data, as would be expected (code not
shown).

We would like to collapse the data to a single row, if possible. We
check to see if we have any places where we have more rows than the
three rows we expect.

``` r
irr_data %>%
  group_by(site_name, Date, Time, depth) %>%
  summarize(n = n(), .groups = 'drop') %>%
  summarize(any(n) > 3)
#> # A tibble: 1 x 1
#>   `any(n) > 3`
#>   <lgl>       
#> 1 FALSE
```

So we can assemble a tibble based on the assumption that we never have
more than the expected three rows of irradiance data. We also checked if
we have any duplicate data, which we don’t. We can use `group_by()`
semantics with `summarize()` to collapse three related rows to a single
row.

We focus here on the irradiance numbers only, as many of the other rows
of data are still in character form.

``` r
test <- irr_data %>%
  group_by(site_name, site, Date, Time, depth) %>%
  summarize(across(irr_air:irr_pct, c(irr = ~mean(.x, na.rm = TRUE), 
                                   n = ~ sum(! is.na(.x)))),
            .groups = 'drop')
test %>%
  summarize(across(contains('_n'), ~any(.x) > 1))
#> Warning in any(site_name): coercing argument of type 'character' to logical
#> # A tibble: 1 x 4
#>   site_name irr_air_n irr_water_n irr_pct_n
#>   <lgl>     <lgl>     <lgl>       <lgl>    
#> 1 NA        FALSE     FALSE       FALSE

test <- test %>%
  select (-contains('_n'), site_name) %>%
  relocate(site_name)
```

# Remove Sonde Data?

We have only a handful of observations of temperature, salinity, etc.
that are not obviously part of the sonde data. We need to decide whether
we have enough non-sonde data to keep the temperature, salinity, etc.
data that is not fully part of the sonde data to analyze it as part of
the core nutrient data.

Here are the rows of the data with at least on type of data assocaited
with sondes, but no more than three of them.

``` r
dep_data %>%
  # select rows with any data in the sonde related categories
  # Ther3 is one flag column here that we don't want to count, so that's why > 1 
  # (not 0).
  filter(rowSums(across(
             .cols = temp:chl_a_sonde,
             .fns = ~ ! is.na(.x)))>1) %>%
  # filter down to those where we only have a few of those metrics
  filter(rowSums(across(
             .cols = temp:chl_a_sonde,
             .fns = ~ ! is.na(.x))) < 5) %>%
  select(temp:chl_a_sonde)
#> # A tibble: 3 x 7
#>     temp salinity    ph pctsat do                 turbidity chl_a_sonde
#>    <dbl> <chr>    <dbl> <chr>  <chr>              <chr>     <chr>      
#> 1 16.8   29.66    NA    105    8.65               <NA>      <NA>       
#> 2 NA     <NA>      7.87 <NA>   <NA>               1.9       J 3.4      
#> 3  0.524 7.37     NA    98     9.7200000000000006 <NA>      <NA>
```

There are only two of them. Essentially all data in these data columns
are associated with complete sonde records.

We do not need to retain these variables in the nutrient data subset.
