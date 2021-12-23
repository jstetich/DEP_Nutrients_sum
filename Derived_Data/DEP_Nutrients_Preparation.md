Preparation of Maine DEP Nutrients Data for Analysis
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
04/26/2021

-   [Load libraries](#load-libraries)
-   [Introduction](#introduction)
-   [DIN Data](#din-data)
    -   [Folder References](#folder-references)
    -   [Load Data](#load-data)
-   [Simplify Names](#simplify-names)
    -   [Split Site Code and Depth
        Code](#split-site-code-and-depth-code)
-   [Delete Data QC Samples](#delete-data-qc-samples)
-   [Generate Irradiance Data](#generate-irradiance-data)
    -   [Remove Irradiance Data](#remove-irradiance-data)
    -   [Remove Rows with No Data](#remove-rows-with-no-data)
-   [Clean Up Quantitative Data](#clean-up-quantitative-data)
    -   [Address Censoring and Data Quality
        Flags](#address-censoring-and-data-quality-flags)
        -   [Salinity and Oxygen](#salinity-and-oxygen)
        -   [Turbidity](#turbidity)
        -   [Chlorophyll Data From
            Sondes](#chlorophyll-data-from-sondes)
        -   [Analytic Chlorophyll Data](#analytic-chlorophyll-data)
        -   [Phaeophyton](#phaeophyton)
        -   [Nitrate](#nitrate)
        -   [Ammonium](#ammonium)
        -   [Total Kjeldahl Nitrogen](#total-kjeldahl-nitrogen)
        -   [Total Nitrogen](#total-nitrogen)
        -   [Orthophosphate](#orthophosphate)
        -   [Total Phosphorus](#total-phosphorus)
        -   [TSS](#tss)
-   [Extract Sonde Data](#extract-sonde-data)
    -   [Delete Bad Temperature Data](#delete-bad-temperature-data)
    -   [Remove Sonde Data from Core
        Data](#remove-sonde-data-from-core-data)
-   [Extract Secchi Data](#extract-secchi-data)
    -   [Remove Rows Without Data From Core
        Data](#remove-rows-without-data-from-core-data)
-   [Output Revised Data](#output-revised-data)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Load libraries

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

# Introduction

This notebook documents steps taken to prepare DEP nutrient-related data
for analysis. In particular, this R notebook takes the original data and
transforms it, step by step, into derived data files used for further
analysis and development of graphics.

The original data included different types of data, collected on
fundamentally different spatial and temporal supports. As a result, this
notebook splits the data into five more limited, but internally more
consistent data files. These include:

1.  “dep\_secchi\_data.csv”: Data on secchi depths (collected at point
    locations; only meaningful for a whole water column observations)

2.  “dep\_sonde\_data.csv”: Vertical profile data derived from downcasts
    of water quality sondes. This data is collected at regular or
    (usually) irregular depths at more or less one time, data that is
    anchored to specific location, date, and depth. Location, depth and
    time of sample collection matter.

3.  “dep\_irradiance\_data.csv”: Also data collected on a vertical
    profile, but usually only up to a depth of a few meters. Data is
    (usually) coupled to collection of matching data on surface
    irradiance, to allow estimation of light extinction coefficients.
    The primary purpose is to document light extinction coefficients
    (and light levels in shallow waters where eelgrass may persist).

4.  “dep\_nutrient\_data.csv”: Data on nutrients, chlorophyll and
    suspended solids.  
    This represents data derived principally from discrete physical
    samples. Most data represents results of laboratory analyses of
    water samples.

5.  “dep\_locations.csv”: Geographic data , aligning site codes and site
    names with latitudes and longitudes. This data set was used to
    generate a GIS data layer, subsequently used to map selected of
    results of analyses.

This notebook provides complete documentation regarding what data was
included our derived data products. It also provides step by step
documentation of how data was processed to generate those working data
sets. In particular, the code provides a record of how we chose to deal
with inconsistent data coding, small sample sizes for certain data
types, treatment of data bearing data quality flags, and censored data.

In some cases, especially where analytic choices about including on
excluding data may have significant impact on results, or where in our
judgment the choices to include of exclude data is more subjective,
final exclusions occur in data analysis notebook to make them more
visible to anyone reviewing these analyses.

# DIN Data

## Folder References

``` r
sibfldnm <- 'Original_Data'
parent <- dirname(getwd())
sibling <- file.path(parent,sibfldnm)

#dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

## Load Data

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

## Split Site Code and Depth Code

A number of entries under `site` are composites, with a site code
followed by " - SUR“,” - BOT“, or” - MAX“. We split those off here. We
chose to retain”RR00\_A" as a separate site, because we don’t understand
why DEP gave it a separate designation, despite the same nominal
latitude and longitude as “RR00”.

``` r
dep_data <- dep_data %>%
    mutate(depth_designation = if_else(grepl(' - ', site),
                                     substr(site, nchar(site) - 3, nchar(site)),
                                     NA_character_),
         site = if_else(grepl(' - ', site),
                          substr(site, 1, nchar(site) - 5),
                          site),
         site = str_trim(site)) %>%
  relocate(depth_designation, .after = site)
```

``` r
dep_data %>%
  filter(! is.na(depth_designation))
#> # A tibble: 277 x 40
#>    site_name      site  depth_designati~ Date                Time               
#>    <chr>          <chr> <chr>            <dttm>              <dttm>             
#>  1 BANDM RAILROA~ BMR02 " SUR"           2018-05-10 00:00:00 1899-12-31 12:42:00
#>  2 BANDM RAILROA~ BMR02 " SUR"           2018-05-31 00:00:00 1899-12-31 12:00:00
#>  3 BANDM RAILROA~ BMR02 " SUR"           2018-05-31 00:00:00 1899-12-31 12:05:00
#>  4 BANDM RAILROA~ BMR02 " SUR"           2018-06-25 00:00:00 1899-12-31 14:00:00
#>  5 BANDM RAILROA~ BMR02 " SUR"           2018-07-18 00:00:00 1899-12-31 14:18:00
#>  6 BANDM RAILROA~ BMR02 " SUR"           2018-07-18 00:00:00 1899-12-31 14:25:00
#>  7 BANDM RAILROA~ BMR02 " SUR"           2018-08-08 00:00:00 1899-12-31 13:20:00
#>  8 BANDM RAILROA~ BMR02 " SUR"           2018-08-30 00:00:00 1899-12-31 12:18:00
#>  9 BANDM RAILROA~ BMR02 " SUR"           2018-09-20 00:00:00 1899-12-31 12:45:00
#> 10 BANDM RAILROA~ BMR02 " SUR"           2018-09-20 00:00:00 1899-12-31 12:45:00
#> # ... with 267 more rows, and 35 more variables: dt <date>, month <fct>,
#> #   year <dbl>, time <chr>, hour <dbl>, Sample Type <chr>, QC Type <chr>,
#> #   Sampled By <chr>, depth <dbl>, Depth Unit <chr>, temp <dbl>,
#> #   salinity <chr>, ph <dbl>, pctsat <chr>, do <chr>, turbidity <chr>,
#> #   chl_a_sonde <chr>, chl_a <chr>, chl_less_phaeo <chr>, phaeo <chr>,
#> #   nox_n <chr>, nh4_n <chr>, tkn <chr>, tn <chr>, op_p <chr>, tp <chr>,
#> #   tss <chr>, secchi <chr>, irr_air <dbl>, irr_water <dbl>, irr_pct <dbl>, ...
```

# Delete Data QC Samples

``` r
dep_data_2 <- dep_data %>%
  select(-`Sampled By`, -`Depth Unit`) %>%
  # delete the QC samples
  filter(`QC Type` == 'NA') %>%
  select(-`Sample Type`, -`QC Type`)
```

# Generate Irradiance Data

We pull out a separate data tibble for the irradiance data, principally
so we can combine data rows.

A few of these data (about 54) rows contain other data, apparently sonde
observations.

``` r
irr_data <- dep_data_2 %>%
  filter(! (is.na(irr_air) & 
              is.na(irr_water) & 
              is.na(irr_pct))) %>%
  select(c(site_name:hour), depth, 
         c(temp:chl_a_sonde),
         c(irr_air:`Sample Comments`)
         ) %>%
  arrange(`site_name`, Date, Time, depth) %>%
  group_by(site_name, site, dt, month, year, time, hour, depth) %>%
  
  # This is the key step to collapse multiple rows.
  # Since we have no field duplicate data, we could
  # use any of several summary functions here.
  summarize(across(irr_air:irr_pct,  ~mean(.x, na.rm = TRUE)),
            .groups = 'drop') %>%
  relocate(site_name)
```

## Remove Irradiance Data

``` r
dep_data_3 <- dep_data_2 %>%
  select(-contains('irr_')) %>%
  select(-Date, -Time)
```

## Remove Rows with No Data

Many data rows only contained data for irradiance. We drop them now. The
method here

``` r
dep_data_3 <- dep_data_3 %>%
  filter(if_any(temp:secchi, ~ ! is.na(.x)))
```

# Clean Up Quantitative Data

We have a significant problem with some entries in the Excel Table
including data flags in the same column as the data, so we need to read
many columns in as text and clean them up after the fact. We may be able
to simplify this a bit once we understand the data structure and quality
codes.

## Address Censoring and Data Quality Flags

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

Sometimes they occur in combinations.

For details see the R Notebook “DEP\_Nutrients\_Preliminary.Rmd”.

In general, we will create a logical flag to indicate EITHER censoring
or presence of a data quality flag. We will not retain information on
the exact data quality flag, in part because the meaning of the flags
may vary based on the original data source. Without full metadata, we
can only note that someone upstream of us flagged the data.

In most cases, censoring is uncommon, so it requires no additional
handling, but that will be addressed during later analysis.

### Salinity and Oxygen

`salinity`, `pctsat` and `do` all contain no text flags (after the QA/QC
data were removed) and can be converted directly to numeric values.

``` r
dep_data_4 <- dep_data_3 %>%
  mutate(across(c(salinity, pctsat, do), as.numeric))
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

### Turbidity

Turbidity includes both “E” codes and “U” codes. The “E” codes are for
powers of ten in scientific notation, and are correctly interpreted by
`as.numeric()`.

``` r
dep_data_4 <- dep_data_4 %>%
  mutate(turbidity_cens = grepl('U', turbidity),
         turbidity = if_else(turbidity_cens, 
                             as.numeric(substr(turbidity, 3, nchar(turbidity))),
                             as.numeric(turbidity))) %>%
  relocate(turbidity_cens, .after = turbidity)
#> Warning in if_else(turbidity_cens, as.numeric(substr(turbidity, 3,
#> nchar(turbidity))), : NAs introduced by coercion
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### Chlorophyll Data From Sondes

As we explored these data, we learned that all of the data are flagged.
As a result, we simply extract numeric values, skipping the text values.

``` r
dep_data_4 <- dep_data_4 %>%
  mutate(chl_a_sonde = as.numeric(substr(chl_a_sonde, 
                                           3, 
                                           nchar(chl_a_sonde))))
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
```

### Analytic Chlorophyll Data

We never have both `chl_a` and `chl_less_phaeo` data at the same time.
Either one can and does occur in a pair with `phaeo` data (code not
shown). It is not clear whether these two columns represent different
quantities, or only alternate labeling in the EGAD data source.

EPA’s Method 445.0 separates “corrected” and “uncorrected” chlorophyll.
The relations between the two are not linear, as the correction occurs
in the realm of florescence, not concentration.

We have much more data in `chl_less_phaeo`. Since there is no easy
interrelationship between the two chlorophyll measures, we drop the \~
17 samples where simple `chla` was reported.

``` r
dep_data_4 <- dep_data_4 %>%
  select(-chl_a) %>%
  
  mutate(chl_flag = grepl('J', chl_less_phaeo),
         chl_cens = grepl('U', chl_less_phaeo),
         chl      = if_else(chl_flag | chl_cens,
                                    as.numeric(substr(chl_less_phaeo, 3, nchar(chl_less_phaeo))), 
                                    as.numeric(chl_less_phaeo)))  %>%
  
  relocate(chl, chl_cens, chl_flag, .after = chl_a_sonde) %>%
  select(-chl_less_phaeo)
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### Phaeophyton

We also calculate the alternative interpretation of the

``` r
dep_data_4 <- dep_data_4 %>%
  mutate(phaeo_flag = grepl('J', phaeo),
         phaeo_cens = grepl('U', phaeo),
         phaeo = if_else(phaeo_flag | phaeo_cens,
                           as.numeric(substr(phaeo, 3, nchar(phaeo))), 
                           as.numeric(phaeo)))  %>%
  relocate(phaeo_flag, phaeo_cens, .after = phaeo)
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### Nitrate

``` r
dep_data_4 <- dep_data_4 %>%
  mutate(nox_n_j_flag = grepl('J', nox_n) & (! grepl('B', nox_n)),
         nox_n_jb_flag = grepl('JB', nox_n),
         nox_n_cens = grepl('U', nox_n),
         nox_n = if_else(nox_n_j_flag | nox_n_cens,
                           as.numeric(substr(nox_n, 3, nchar(nox_n))),
                           if_else(nox_n_jb_flag,
                                   as.numeric(substr(nox_n, 4, nchar(nox_n))),
                                   as.numeric(nox_n))),
         nox_n_flag = nox_n_j_flag | nox_n_jb_flag)  %>%
  select(-nox_n_jb_flag, -nox_n_j_flag) %>%
 relocate(nox_n_cens, nox_n_flag, .after = nox_n)
#> Warning in if_else(nox_n_jb_flag, as.numeric(substr(nox_n, 4, nchar(nox_n))), :
#> NAs introduced by coercion
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### Ammonium

``` r
dep_data_4 <- dep_data_4 %>%
  mutate(nh4_n_flag = grepl('J', nh4_n) | grepl('B', nh4_n),
         nh4_n_cens = grepl('U', nh4_n),
         nh4_n = if_else(nh4_n_flag | nh4_n_cens,
                           as.numeric(substr(nh4_n, 3, nchar(nh4_n))),
                           as.numeric(nh4_n)))  %>%
 relocate(nh4_n_cens, nh4_n_flag, .after = nh4_n)
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### Total Kjeldahl Nitrogen

Data is too sparse to be useful, so we delete it.

``` r
dep_data_4 <- dep_data_4 %>%
  select(-tkn)
```

### Total Nitrogen

No TN values are non-detects, but about one in eight are flagged.

``` r
dep_data_4 <- dep_data_4 %>%
  mutate(tn_flag = grepl('J', tn),
         tn = if_else(tn_flag,
                           as.numeric(substr(tn, 3, nchar(tn))),
                           as.numeric(tn)))  %>%
 relocate(tn_flag, .after = tn)
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### Orthophosphate

``` r
dep_data_4 <- dep_data_4 %>%
  
  mutate(op_p_jb_flag = grepl('JB', op_p),
         op_p_star_flag = grepl('\\*', op_p),
         op_p_j_flag = grepl('J', op_p) & 
                          (! op_p_jb_flag) & 
                             (! op_p_star_flag),
         op_p = if_else(op_p_j_flag,
                           as.numeric(substr(op_p, 3, nchar(op_p))),
                           if_else(op_p_jb_flag | op_p_star_flag,
                                   as.numeric(substr(op_p, 4, nchar(op_p))),
                                   as.numeric(op_p))),
         op_p_flag = op_p_j_flag | op_p_jb_flag | op_p_star_flag)  %>%
 select(-op_p_jb_flag, -op_p_j_flag, -op_p_star_flag) %>%
 relocate(op_p_flag, .after = op_p)
#> Warning in if_else(op_p_jb_flag | op_p_star_flag, as.numeric(substr(op_p, : NAs
#> introduced by coercion
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### Total Phosphorus

``` r
dep_data_4 <- dep_data_4 %>%
  mutate(tp_b_flag = grepl('B', tp),
         tp_cens = grepl('U', tp),
         tp_j_flag = grepl('J', tp),
         tp = if_else(tp_j_flag | tp_cens | tp_b_flag,
                           as.numeric(substr(tp, 3, nchar(tp))),
                           as.numeric(tp)),
         tp_flag = tp_j_flag | tp_b_flag )  %>%
 select(-tp_j_flag, -tp_b_flag) %>%
relocate(tp_cens,  tp_flag, .after = tp )
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

### TSS

``` r
dep_data_4 <- dep_data_4 %>%
    mutate(tss_cens = grepl('U', tss),
         tss_flag = grepl('J', tss),
         tss = if_else(tss_flag | tss_cens,
                           as.numeric(substr(tss, 3, nchar(tss))),
                           as.numeric(tss))) %>%
  relocate(tss_cens, tss_flag, .after = tss)
#> Warning in if_else(tss_flag | tss_cens, as.numeric(substr(tss, 3,
#> nchar(tss))), : NAs introduced by coercion
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

\#\#\#Secchi Depths

``` r
dep_data_4 <- dep_data_4 %>%
  
  mutate(secchi_on_bottom = grepl('>', secchi),
         secchi = if_else(secchi_on_bottom,
                           as.numeric(substr(secchi, 2, nchar(secchi))),
                           as.numeric(secchi))) %>%
  relocate(secchi_on_bottom, .after = secchi)
#> Warning in replace_with(out, !condition, false, fmt_args(~false), glue("length
#> of {fmt_args(~condition)}")): NAs introduced by coercion
```

# Extract Sonde Data

We also extract a subset of the data derived from sondes for independent
review, but as we dis not delete those data from the full data set, the
two data sets are now not independent of each other

``` r
sonde_data <- dep_data_4 %>%
  select(site_name:chl_a_sonde) %>%
  relocate(turbidity, turbidity_cens, .after = chl_a_sonde) %>%
  filter(if_any(temp:turbidity, ~ ! is.na(.x)))
```

## Delete Bad Temperature Data

We note a series of low temperature data. These appear to be
problematic. There is a collection of temperature values below 1 C in
spring and summer months, which is unlikely. We delete those
questionable temperature values.

``` r
sonde_data <- sonde_data %>%
  mutate(temp = if_else(temp < 5,
                 NA_real_, temp)) %>%
  select(-depth_designation)
```

## Remove Sonde Data from Core Data

We have only a handful of observations of temperature, salinity, etc.
that are not part of a complete sonde data record. So we will analyze
these data nearly completely when we evaluate the sonde data.

``` r
dep_data_5 <- dep_data_4 %>%
  select(-c(temp:chl_a_sonde))
```

# Extract Secchi Data

``` r
secchi_data <- dep_data_5 %>%
filter(! is.na(secchi)) %>%
  select(site_name:hour, secchi:`Validation Comments`) %>%
  select(-depth_designation)
```

## Remove Rows Without Data From Core Data

Since we deleted most of the data collected at depth (Secchi,
irradiance, sonde) we now have many empty rows. We need to remove them
from the final data.

First, we create a flag to identify rows that contain any numerical
data. Then we use that to extract data that contains real data for any
of our core parameters of interest.

(We split the two steps to avoid retaining spurious “data” where we have
data quality of censoring flags, but no data other than that.)

``` r
grab_these <- dep_data_5 %>%
  select(site, dt, where(is.numeric)) %>%
  mutate(grab = if_any(chl:tss, ~ ! is.na(.x))) %>%
  pull(grab)

dep_data_6 <- dep_data_5 %>%
  select(-secchi, -secchi_on_bottom) %>%
  filter(grab_these)
```

# Output Revised Data

``` r
write_csv(dep_data_6, 'dep_nutrient_data.csv', na = '')
write_csv(irr_data, 'dep_irradiance_data.csv')
write_csv(sonde_data, 'dep_sonde_data.csv')
write_csv(secchi_data, 'dep_secchi_data.csv')
```
