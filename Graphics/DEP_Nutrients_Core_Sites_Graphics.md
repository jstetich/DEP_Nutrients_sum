Graphics of Maine DEP Nutrient Data from Casco Bay, Maine
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
12/27/2021

-   [Introduction](#introduction)
-   [Folder References](#folder-references)
-   [Load Data](#load-data)
-   [Surface Data Only](#surface-data-only)
    -   [Correct Misleading NH4 Values](#correct-misleading-nh4-values)
    -   [Add Shorter Site Names](#add-shorter-site-names)
    -   [Add DIN and Organic N](#add-din-and-organic-n)
    -   [Add N to P Ratio](#add-n-to-p-ratio)
-   [Core Sites Data](#core-sites-data)
    -   [Descriptive Statistics](#descriptive-statistics)
    -   [TN Graphic](#tn-graphic)
    -   [DIN Graphic](#din-graphic)
-   [N to P Ratios](#n-to-p-ratios)
    -   [Descriptive Statistics](#descriptive-statistics-1)
    -   [Graphic](#graphic)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This R Notebook focuses on graphics showing nutrients concentrations,
especially total nitrogen, from a small number of “core” locations
sampled by Maine DEP regularly in 2018, 2019, and 2020. These sites are
all found close to Portland, Maine.

The focus is on depicting differences among sites in simple graphics and
maps.

\#Load Libraries

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.1     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(viridis)  # Normally not called directly, but we need it for the ternary
#> Loading required package: viridisLite
                  # plot color scale.

library(Ternary) # Base graphics ternary plots

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Folder References

``` r
sibfldnm <- 'Data'
parent <- dirname(getwd())
sibling <- paste(parent,sibfldnm, sep = '/')

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

# Load Data

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
site_names <- read_csv(file.path(sibling, "GIS", 'dep_locations.csv')) %>%
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

We can calculate DIN as the sum of ammonium and nitrate, and organic N
as the difference between DIN and TN

``` r
surface_data <- surface_data %>%
  mutate(din = nh4_n + nox_n,
         din_cens = nh4_n_cens | nox_n_cens,
         din_flag = nh4_n_flag | nox_n_flag,
         on       = tn - din,
         on_flag  = din_flag | tn_flag,
         on_cens_r  = din_flag)    #since this is calculated by difference
```

### Add N to P Ratio

``` r
surface_data <- surface_data %>%
  mutate(n_to_p = tn/tp)
```

# Core Sites Data

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
  mutate(site = fct_reorder(factor(site), tn, na.rm = TRUE),
         short_name = fct_reorder(factor(short_name), tn, na.rm = TRUE))
```

``` r
rm(dep_data, surface_data)
```

## Descriptive Statistics

``` r
core_results <- core_data %>%
  group_by(site, short_name) %>%
  summarize(across(c(nox_n, nh4_n, din, on, tn, chl, tss),
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

## TN Graphic

``` r
plt <- ggplot(core_data, aes(tn, short_name)) +

  geom_point(alpha = 1, color = cbep_colors()[4]) +
  
  ylab('') +
  xlab('Total Nitrogen (mg/l)') +
  
  theme_cbep(base_size = 12) +
  theme(axis.title.x = element_text(size = 10),
        #axis.text.x= element_text(angle = 45, hjust = 1, size = 8),
        legend.position = 'None',
        #panel.grid.major.x = element_line(color = 'gray85'),
        panel.spacing.x = unit(1, 'lines')) +
  
  geom_vline(xintercept = 0.32, color = cbep_colors()[3], lty = 3) +
  geom_text(aes(x = 0.35, y = 3, label = '0.32'), 
            angle = 90, hjust = 0, size = 3,
                color = cbep_colors()[3]) +
  geom_vline(xintercept = 0.45, color = cbep_colors()[3], lty = 3) +
  geom_text(aes(x = 0.49, y = 3, label = '0.45'), 
            angle = 90, hjust = 0, size = 3,
                color = cbep_colors()[3]) +
  labs(subtitle = 'DEP Data, 2017-2019')

plt
#> Warning: Removed 57 rows containing missing values (geom_point).
```

<img src="DEP_Nutrients_Core_Sites_Graphics_files/figure-gfm/tn_graphic-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/tn_by_site.pdf', device = cairo_pdf, width = 3.5, height = 4)
#> Warning: Removed 57 rows containing missing values (geom_point).
```

## DIN Graphic

``` r
plt <- ggplot(core_data, aes(din, short_name)) +

  geom_point(alpha = 1, color = cbep_colors()[6]) +
  
  ylab('') +
  xlab('Dissolved Inorganic Nitrogen\n(mg/l)') +
  
  theme_cbep(base_size = 12) +
  theme(axis.title.x = element_text(size = 9),
        legend.position = 'None',
        #panel.grid.major.x = element_line(color = 'gray85'),
        panel.spacing.x = unit(1, 'lines'))+
  labs(subtitle = 'DEP Data, 2017-2019')
plt
#> Warning: Removed 93 rows containing missing values (geom_point).
```

<img src="DEP_Nutrients_Core_Sites_Graphics_files/figure-gfm/din_points_only-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/din_by_site.pdf', device = cairo_pdf, width = 3.5, height = 4)
#> Warning: Removed 93 rows containing missing values (geom_point).
```

# N to P Ratios

Generally, our N:P ratios are well below 15, suggesting continued N
limitation. Is that worth reporting on? There may be N:P ratio variation
between sites.

### Descriptive Statistics

``` r
core_n2p_results <- core_data %>%
  group_by(site, short_name) %>%
  summarize(across(c(tn, tp, n_to_p),
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

### Graphic

``` r
ggplot(core_data, aes(n_to_p, short_name)) +
 geom_point(alpha = 1, color = cbep_colors()[2]) +
  
  geom_point(data = core_n2p_results, mapping = aes(x = n_to_p_md, y = short_name),
             shape = 3, size = 2,
             color = cbep_colors()[3]) +
  
  geom_vline(xintercept = 15, color = 'gray50', lty = 3) +
   geom_text(aes(x = 16, y = 0.5, label = 'N:P = 15'), 
            angle = 90, hjust = 0, size = 3,
                color = cbep_colors()[3]) +
  ylab('') +
  xlab('N to P Ratio') +
  
  theme_cbep(base_size = 12)
#> Warning: Removed 127 rows containing missing values (geom_point).
```

<img src="DEP_Nutrients_Core_Sites_Graphics_files/figure-gfm/n2p_graphic-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/n_to_p_by_site.pdf', device = cairo_pdf, width = 3.5, height = 4)
#> Warning: Removed 127 rows containing missing values (geom_point).
```

It’s clear there is a seasonal pattern in N to P ratios.
