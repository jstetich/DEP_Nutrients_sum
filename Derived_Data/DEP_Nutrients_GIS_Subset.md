Preparation of Data for DEP Nutrient Data for GIS Mapping
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
06/15/2021

-   [Introduction](#introduction)
-   [Extract Geographic Locations](#extract-geographic-locations)
-   [Folder References](#folder-references)
-   [Load\_data](#load_data)
    -   [Load Data](#load-data)
-   [Simplify Names](#simplify-names)
    -   [Split Site Code and Depth
        Code](#split-site-code-and-depth-code)
    -   [A Simple Function for Capitalizing
        Strings](#a-simple-function-for-capitalizing-strings)
-   [Folder References](#folder-references-1)
-   [Load WQ Data](#load-wq-data)
-   [Surface Data Only](#surface-data-only)
    -   [Correct Misleading NH4 Values](#correct-misleading-nh4-values)
    -   [Add Shorter Site Names](#add-shorter-site-names)
    -   [Add DIN and Organic N](#add-din-and-organic-n)
-   [Data Prevalence](#data-prevalence)
-   [Data Subsets](#data-subsets)
    -   [Core Sites Data](#core-sites-data)
    -   [Fore River Subset from 2016](#fore-river-subset-from-2016)
    -   [Harraseeket Subset from 2016](#harraseeket-subset-from-2016)
    -   [Royal and Cousins Data Subset from
        2017](#royal-and-cousins-data-subset-from-2017)
    -   [Antoine Creek Data Subset from
        2015](#antoine-creek-data-subset-from-2015)
-   [Cleanup](#cleanup)
-   [Descriptive Statistics](#descriptive-statistics)
    -   [Output Decriptive Statistics for
        GIS](#output-decriptive-statistics-for-gis)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This R Notebook develops data subsets from DEP’s recent nutrient
monitoring in Casco Bay to show in GIS displays. The central challenge
is that the unequal sampling effort from year to year makes certain
comparisons suspect, and we need to signal that to viewers by showing
consistent symbolism for data subsets.

In particular, there are some sites that were sampled in only one year,
others that were sampled over several years. We need to clarify those
groups.

\#Load Libraries

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
library(viridis)  # Normally not called directly, but we need it for the ternary
#> Warning: package 'viridis' was built under R version 4.0.5
#> Loading required package: viridisLite
#> Warning: package 'viridisLite' was built under R version 4.0.5
                  # plot color scale.

library(GGally)
#> Warning: package 'GGally' was built under R version 4.0.5
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
library(mgcv)
#> Warning: package 'mgcv' was built under R version 4.0.5
#> Loading required package: nlme
#> 
#> Attaching package: 'nlme'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse
#> This is mgcv 1.8-38. For overview type 'help("mgcv-package")'.
library(emmeans)
#> Warning: package 'emmeans' was built under R version 4.0.5
#> 
#> Attaching package: 'emmeans'
#> The following object is masked from 'package:GGally':
#> 
#>     pigs

library(Ternary) # Base graphics ternary plots
#> Warning: package 'Ternary' was built under R version 4.0.5

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Extract Geographic Locations

# Folder References

``` r
sibfldnm <- 'Original_Data'
parent <- dirname(getwd())
sibling <- paste(parent,sibfldnm, sep = '/')

#dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

# Load\_data

## Load Data

``` r
dep_loc_data <- read_excel(file.path(sibling, "Curtis Bohlen 051021.xlsx"),
                       col_types = c("text", "text", rep('skip', 30), 
                                     "numeric", "numeric")) 
```

# Simplify Names

``` r
dep_loc_data <- dep_loc_data %>% 
  rename(site_name = `Site ID`,
         site = `Sample Point ID`)
```

## Split Site Code and Depth Code

A number of entries under `site` are composites, with a site code
followed by " - SUR“,” - BOT“, or” - MAX“. We split those off here. We
chose to retain”RR00\_A" as a separate site, because we don’t understand
why DEP gave it a separate designation, despite the same nominal
latitude and longitude as “RR00”.

``` r
dep_loc_data <- dep_loc_data %>%
    mutate(site = if_else(grepl(' - ', site),
                          substr(site, 1, nchar(site) - 5),
                          site),
           site = str_trim(site))
```

## A Simple Function for Capitalizing Strings

``` r
simple_title <- function(x) {
  s <- strsplit(x, " ")[[1]]
  return(paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" "))
}

to_title <- function(.x) {
  v <- sapply(.x, simple_title, USE.NAMES = FALSE)
  return(v)
}
```

``` r
a <- to_title(c('Testing a NUMBER of IdEas here.', 'And again!'))
a
#> [1] "Testing A Number Of Ideas Here." "And Again!"
```

``` r
geographic_data <- dep_loc_data %>%
  unique() %>%
  mutate(short_name = sub( '- CR-', '', site_name)) %>%
  mutate(short_name = sub( '- CR', '', short_name)) %>%
  mutate(short_name = sub( '- PR-', '', short_name)) %>%
  mutate(short_name = sub( '- PR', '', short_name)) %>%
  mutate(short_name = sub( '- RR-', '', short_name)) %>%
  mutate(short_name = sub( '- RR', '', short_name)) %>%
  mutate(short_name = sub( '- AC0', '', short_name)) %>%
  mutate(short_name = sub( '- CR', '', short_name)) %>%
  mutate(short_name = sub( '- FR0', '', short_name)) %>%
  mutate(short_name = sub( '- HR0', '', short_name)) %>%
  mutate(short_name = sub( '- LC0', '', short_name)) %>%
  mutate(short_name = sub(' -.*$', '', short_name)) %>%
  mutate(short_name = sub(' V70', '', short_name)) %>%
  
  mutate(short_name = sub(' RIVER', '', short_name)) %>%

  mutate(short_name = sub('FORE', 'FORE RIVER', short_name)) %>%
  mutate(short_name = sub('TRIB0', 'TRIB', short_name)) %>%
  
  mutate(short_name = to_title(short_name)) %>%
  

  mutate(short_name = if_else(site == 'BMR02',
                              'B&M Railroad',
                              short_name)) %>%
  mutate(short_name = if_else(site == 'PRV70',
                              'Walton Park',
                              short_name)) %>%
  mutate(short_name = if_else(site == 'CBPR',
                              'Presumpscot Mouth',
                              short_name)) %>%
  mutate(short_name = if_else(site == 'RR00_A',
                              'Royal A',
                              short_name)) %>%
  
  relocate(short_name, .after = site_name)
```

``` r
write_csv(geographic_data, 'gis/dep_locations.csv')
```

# Folder References

``` r
sibfldnm <- 'Derived_Data'
parent <- dirname(getwd())
sibling <- paste(parent,sibfldnm, sep = '/')

#dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

# Load WQ Data

``` r
dep_data <- read_csv(file.path(sibling, 'dep_nutrient_data.csv'),
                     col_types = cols(
                       .default = col_logical(),
                       site_name = col_character(),
                       site = col_character(),
                       depth_designation = col_character(),
                       dt = col_date(format = ""),
                       month = col_character(),
                       year = col_double(),
                       time = col_time(format = ""),
                       hour = col_double(),
                       depth = col_double(),
                       chl = col_double(),
                       phaeo = col_double(),
                       nox_n = col_double(),
                       nh4_n = col_double(),
                       tn = col_double(),
                       op_p = col_double(),
                       tp = col_double(),
                       tss = col_double(),
                       `Sample Comments` = col_character(),
                       `Validation Comments` = col_character(),
                       Latitude = col_skip(),
                       Longitude = col_skip())) %>%
  rename(sample_date = dt)
```

# Surface Data Only

``` r
surface_data <- dep_data %>%
  filter(depth <= 1) %>%
  mutate(month = factor(month, levels = month.abb))%>%
  mutate(yearf = factor(year)) %>%
  mutate(doy = as.numeric(format(sample_date, format = '%j'))) %>%
  relocate(yearf, doy, .after = year)
```

There are a few sky-high NH4 values, at nominal concentrations of 0.4 or
4.0 mg.l NH4-N. All are flagged as “censored” (despite high values) and
carry annotations of excessive hold times. We delete them.

### Correct Misleading NH4 Values

``` r
surface_data <- surface_data %>%
  mutate(nh4_n = if_else(nh4_n > 0.3, NA_real_, nh4_n))
```

### Add Shorter Site Names

``` r
site_names <- read_csv(file.path(sibling, 'GIS', 'dep_locations.csv')) %>%
  select(site, short_name)
#> Rows: 44 Columns: 5
#> -- Column specification --------------------------------------------------------
#> Delimiter: ","
#> chr (3): site_name, short_name, site
#> dbl (2): Latitude, Longitude
#> 
#> i Use `spec()` to retrieve the full column specification for this data.
#> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
surface_data <- surface_data %>%
  left_join(site_names, by = 'site') %>%
  relocate(short_name, .after = site)
```

### Add DIN and Organic N

We calculate DIN as the sum of ammonium and nitrate, and organic N as
the difference between DIN and TN.

``` r
surface_data <- surface_data %>%
  mutate(din = nh4_n + nox_n,
         din_cens = nh4_n_cens | nox_n_cens,
         din_flag = nh4_n_flag | nox_n_flag,
         on       = tn - din,
         on_flag  = din_flag | tn_flag,
         on_cens_r  = din_flag) %>%   #since this is calculated by difference
  relocate(din:on_cens_r, .after = tn_flag)
```

# Data Prevalence

``` r
xtabs(~ site + year, data = surface_data)
#>          year
#> site      2015 2016 2017 2018 2019 2020
#>   AC01       4    0    0    0    0    0
#>   AC02       4    0    0    0    0    0
#>   AC04       4    0    0    0    0    0
#>   AC05       4    2    0    0    0    0
#>   BMR02      0    0    0    8    9   14
#>   CBPR       0    0    7    8    8   18
#>   CR-31      0    1    7    0    0    0
#>   CR-44      0    0    3    0    0    0
#>   CR00       0    0    8    0    0    0
#>   CRTRIB0    0    0    7    0    0    0
#>   EEB18      0    0    0    8    8   14
#>   FR01       0    4    0    0    0    0
#>   FR02       0    4    0    0    0    0
#>   FR03       0    4    0    0    0    0
#>   FR04       0    4    0    0    0    0
#>   FR05A      0    2    0    0    0    0
#>   FR05B      0    2    0    0    0    0
#>   FR06       0    4    0    0    0    0
#>   FR07       0    4    0    0    0    0
#>   FR08       0    4    0    0    0    0
#>   FR09       0    4    6    7    8   14
#>   HR01       0    7    0    0    0    0
#>   HR02       0    8    0    0    0    0
#>   HR03       0    7    0    0    0    0
#>   HR04       0    8    0    0    0    0
#>   HR05       0    8    0    0    0    0
#>   LC01       0    2    0    0    0    0
#>   LC02       0    4    0    0    0    0
#>   P6FGG      0    0    0    2    8    6
#>   P7CBI      0    0    0    2    8    6
#>   PR-13      0    0    0    0    0    2
#>   PR-17      0    0    0    8    8   18
#>   PR-19      0    0    0    0    0    2
#>   PR-28      0    0    7    8    8   18
#>   PR01       0    0    0    0    0    2
#>   PRV70      0    0    7    8    8   16
#>   RR-01      0    0    7    0    0    0
#>   RR-06      1    2    7    0    0    0
#>   RR-13      0    2    7    0    0    0
#>   RR-19      1    2    3    0    0    0
#>   RR-20      0    0    3    0    0    0
#>   RR00       1    0    0    0    0    0
#>   RR00_A     0    0    8    0    0    0
#>   TB01       0    2    0    0    0    0
```

# Data Subsets

We do not order sites and site names by location or TN values here, as
the purpose is to assemble data for GIS analysis.

## Core Sites Data

``` r
core_sites <- surface_data %>%
  select(site, year, din) %>%
  filter(year == 2018) %>%
  filter(! is.na(din)) %>%
  select(-year, -din) %>%
  unique %>%
  arrange(site) %>%
  pull
core_sites
#> [1] "BMR02" "CBPR"  "EEB18" "FR09"  "P6FGG" "P7CBI" "PR-17" "PR-28" "PRV70"
```

``` r
core_data <- surface_data %>%
  filter(year > 2017) %>%
  filter(site %in% core_sites) %>%
  # mutate(site = fct_reorder(factor(site), tn, na.rm = TRUE),
  #        short_name = fct_reorder(factor(short_name), tn, na.rm = TRUE)) %>%
  mutate(years = '2018 - 2020')
```

## Fore River Subset from 2016

``` r
fore_sites <- levels(factor(surface_data$site))
fore_sites <- fore_sites[grepl('FR', fore_sites) | grepl('LC', fore_sites)]
fore_sites <- fore_sites[c(1,2,11,12,3:10)] # upstream-downstream order
fore_sites <- fore_sites[fore_sites != 'FR09']   # remove "core" site which
                                                 # will be displayed based 
                                                 # on data from later years

fore_names <- site_names$short_name[match(fore_sites,site_names$site)]
cbind(fore_sites, fore_names)
#>       fore_sites fore_names     
#>  [1,] "FR01"     "Fore River 1" 
#>  [2,] "FR02"     "Fore River 2" 
#>  [3,] "LC01"     "Long Creek 1" 
#>  [4,] "LC02"     "Long Creek 2" 
#>  [5,] "FR03"     "Fore River 3" 
#>  [6,] "FR04"     "Fore River 4" 
#>  [7,] "FR05A"    "Fore River 5a"
#>  [8,] "FR05B"    "Fore River 5b"
#>  [9,] "FR06"     "Fore River 6" 
#> [10,] "FR07"     "Fore River 7" 
#> [11,] "FR08"     "Fore River 8"
```

``` r
fore_data <- surface_data %>%
  filter(year == 2016) %>%
  filter(site %in% fore_sites) %>%
  # mutate(site = factor(site, levels = fore_sites),
  #        short_name = factor(short_name, levels = fore_names)) %>%
  mutate(years = '2016')
```

## Harraseeket Subset from 2016

``` r
harraseeket_sites <- levels(factor(surface_data$site))
harraseeket_sites <- harraseeket_sites[grepl('HR', harraseeket_sites)]

harraseeket_names <- site_names$short_name[match(harraseeket_sites,site_names$site)]
cbind(harraseeket_sites, harraseeket_names)
#>      harraseeket_sites harraseeket_names
#> [1,] "HR01"            "Harraseeket 1"  
#> [2,] "HR02"            "Harraseeket 2"  
#> [3,] "HR03"            "Harraseeket 3"  
#> [4,] "HR04"            "Harraseeket 4"  
#> [5,] "HR05"            "Harraseeket 5"
```

``` r
harraseeket_data <- surface_data %>%
  filter(year == 2016) %>%
  filter(site %in% harraseeket_sites) %>%
  # mutate(site = factor(site, levels = harraseeket_sites),
  #        short_name = factor(short_name, levels = harraseeket_names)) %>%
  mutate(years = '2016')
```

## Royal and Cousins Data Subset from 2017

``` r
royal_sites <- c('CRTRIB0','CR00', 'CR-31', 'CR-44',
                 #'RR00',                            # This site not present in 2017
                 'RR00_A', 'RR-01', 'RR-06',
                 'RR-13', 'RR-19', 'RR-20')

royal_names <- site_names$short_name[match(royal_sites,site_names$site)]
cbind(royal_sites, royal_names)
#>       royal_sites royal_names   
#>  [1,] "CRTRIB0"   "Cousins Trib"
#>  [2,] "CR00"      "Cousins 00"  
#>  [3,] "CR-31"     "Cousins 31"  
#>  [4,] "CR-44"     "Cousins 44"  
#>  [5,] "RR00_A"    "Royal A"     
#>  [6,] "RR-01"     "Royal 01"    
#>  [7,] "RR-06"     "Royal 06"    
#>  [8,] "RR-13"     "Royal 13"    
#>  [9,] "RR-19"     "Royal 19"    
#> [10,] "RR-20"     "Royal 20"
```

``` r
royal_data <- surface_data %>%
  filter(year == 2017) %>%
  filter(site %in% royal_sites) %>%
  # mutate(site = factor(site, levels = royal_sites),
  #        short_name = factor(short_name, levels = royal_names)) %>%
  mutate(years = '2017')
```

## Antoine Creek Data Subset from 2015

Antoine Creek only has NOx data, but we retain it anyway.

``` r
antoine_sites <- levels(factor(surface_data$site))
antoine_sites <- antoine_sites[grepl('AC', antoine_sites)]

antoine_names <- site_names$short_name[match(antoine_sites,site_names$site)]
cbind(antoine_sites, antoine_names)
#>      antoine_sites antoine_names     
#> [1,] "AC01"        "Anthoine Creek 1"
#> [2,] "AC02"        "Anthoine Creek 2"
#> [3,] "AC04"        "Anthoine Creek 4"
#> [4,] "AC05"        "Anthoine Creek 5"
```

``` r
antoine_data <- surface_data %>%
  filter(year == 2015) %>%
  filter(site %in% antoine_sites) %>%
  # mutate(site = factor(site, levels = antoine_sites),
  #        short_name = factor(short_name, levels = antoine_names)) %>%
  mutate(years = '2015')
```

``` r
gis_data <- core_data %>%
  bind_rows(fore_data) %>%
    bind_rows(royal_data) %>%
    bind_rows(harraseeket_data) %>%
    bind_rows(antoine_data)
```

# Cleanup

``` r
rm(surface_data, antoine_data, core_data, fore_data, 
   harraseeket_data, royal_data)
rm(antoine_names, core_names, fore_names, 
   harraseeket_names, royal_names)
#> Warning in rm(antoine_names, core_names, fore_names, harraseeket_names, : object
#> 'core_names' not found
rm(antoine_sites, core_sites, fore_sites, 
   harraseeket_sites, royal_sites)
```

# Descriptive Statistics

``` r
gis_results <- gis_data %>%
  group_by(site, short_name) %>%
  summarize(years = first(years),
            across(c(nox_n, nh4_n, din, on, tn, chl, tss),
                   .fns = c(mn = ~ mean(.x, na.rm = TRUE),
                            sd = ~ sd(.x, na.rm = TRUE), 
                            n = ~sum(! is.na(.x)),
                            md = ~ median(.x, na.rm = TRUE),
                            iqr = ~ IQR(.x, na.rm = TRUE),
                            p90 = ~ quantile(.x, .9, na.rm = TRUE),
                            gm = ~ exp(mean(log(.x), na.rm = TRUE)))),
            .groups = 'drop') %>%
  mutate(site = fct_reorder(factor(site), tn_md),
         short_name = fct_reorder(factor(short_name), tn_md))
```

## Output Decriptive Statistics for GIS

``` r
write_csv(gis_results, file.path('GIS', 'gis_results.csv'))
```
