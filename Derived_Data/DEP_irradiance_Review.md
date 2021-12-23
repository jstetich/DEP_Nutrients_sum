Initial Review of DEP Irradiance Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
04/26/2021

-   [Introduction](#introduction)
-   [Review of theory](#review-of-theory)
-   [Load Data](#load-data)
-   [Summary of Metadata](#summary-of-metadata)
    -   [QA/QC Samples](#qaqc-samples)
    -   [Censoring Flags](#censoring-flags)
    -   [Units](#units)
-   [Review of Irradiance Data](#review-of-irradiance-data)
    -   [Scatterplot Matrix (Pairs
        Plot)](#scatterplot-matrix-pairs-plot)
    -   [Sites by Depths](#sites-by-depths)
    -   [How often was each site
        sampled?](#how-often-was-each-site-sampled)
-   [Calculation of Light Extinction
    Coefficients](#calculation-of-light-extinction-coefficients)
-   [Output Light Extinction Coefficient
    Data](#output-light-extinction-coefficient-data)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

In this notebook, we review Maine DEP irradiance data and calculate
light attenuation coefficients (k values.) We export a data set
consisting of K estimates, sample sizes, and estimated standard errors
of those estimates.

# Review of theory

Light attenuation is often measured as
*I*<sub>*d*</sub> = *I*<sub>0</sub>*e*<sup> − *k**z*</sup>
Where *z* is depth.

$$
\\frac{I\_d}{I\_0} = e^{-kz}
$$

$$
log(\\frac{I\_d}{I\_0}) = -kz
$$

Note that this produces a value of k at each depth. If we assume this
theory is accurate and light attenuation is vertically uniform, we can
average across depths to improve accuracy.

$$ k = \\frac{1}{-z} \\times log(\\frac{I\_d}{I\_0}) $$

Where we recast values as percentages of surface light, we can estimate
k\~d as

$$ k \\approx - \\frac{log(I\_d)}{z}$$

We want to recast that as a linear regression problem.

Y = mx + b

log(Id) = -kz

So, K can be estimated as the negative of linear coefficient of depth in
a linear model.

\#Load libraries

``` r
#library(readxl)
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

library(GGally)
#> Warning: package 'GGally' was built under R version 4.0.5
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
#library(emmeans)
#library(mgcv)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Load Data

``` r
irr_data <- read_csv(file.path('dep_irradiance_data.csv')) %>%
              rename(sample_date = dt)
#> Rows: 2851 Columns: 11
#> -- Column specification --------------------------------------------------------
#> Delimiter: ","
#> chr  (3): site_name, site, month
#> dbl  (6): year, hour, depth, irr_air, irr_water, irr_pct
#> date (1): dt
#> time (1): time
#> 
#> i Use `spec()` to retrieve the full column specification for this data.
#> i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# Summary of Metadata

## QA/QC Samples

We conducted no analysis of QA/QC samples, and simply deleted them from
the data to avoid confusion.

## Censoring Flags

While preparing our working data, we separated raw observations from
text annotations, including data quality flags. In the sonde-related
data, we only had to contend with (1) left censoring of turbidity data,
and (2) data quality flags on all chlorophyll data.

Since all sonde-related chlorophyll data was flagged as of questionable
accuracy (with “J” flags), it does us no good to track that information
during further analysis. We retain all data, but recognize that it’s
accuracy is suspect, especially in comparison to laboratory results.

We also had a few “U&lt;” flags in the Turbidity data. We separated out
a `TRUE` / `FALSE` flag to indicated censored values, with the name
’turbidity\_cens\`.

## Units

Our derived data files lack any indication of units. Units were
documented in the source Excel files. We summarize relevant information
here.

| Variable Name | Meaning                                         | Units                       |
|---------------|-------------------------------------------------|-----------------------------|
| site\_name    | DEP “Site ID”                                   |                             |
| site          | DEP “Sample Point ID” without depth designation |                             |
| sample\_date  | Date of sample collection                       | yyyy-mm-dd format           |
| month         | Month, derived from date                        | Three letter codes          |
| year          | Year, derived from date                         |                             |
| time          | time of sample                                  | 24 hour clock, hh:mm format |
| hour          | hour, derived from time                         |                             |
| depth         | Sample Depth                                    | Meters                      |
| irr\_air      | Irradiance (air)                                | µmol/m2/s                   |
| irr\_water    | Irradiance (surface water)                      | µmol/m2/s                   |
| irr\_pct\_    | Irradiance (% of air in surface water)          | %                           |

# Review of Irradiance Data

## Scatterplot Matrix (Pairs Plot)

``` r
tmp <- irr_data %>%
  select(irr_air:irr_pct)
ggpairs(log(tmp), progress = FALSE)
#> Warning: Removed 157 rows containing non-finite values (stat_density).
#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removed 157 rows containing missing values

#> Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
#> Removed 157 rows containing missing values
#> Warning: Removed 157 rows containing missing values (geom_point).

#> Warning: Removed 157 rows containing missing values (geom_point).
```

<img src="DEP_Irradiance_Review_files/figure-gfm/scatterplot_matrix-1.png" style="display: block; margin: auto;" />
Note skewed data distributions, even for the percentage values.

## Sites by Depths

``` r
tmp <- irr_data %>%
  mutate(dpth_clss = if_else(depth < 2, round(depth, 1), round(depth,0)))
xtabs(~ dpth_clss + site, data = tmp)
#>          site
#> dpth_clss BMR02 CBPR CR-31 CR-44 EEB18 FR03 FR04 FR05A FR05B FR07 FR09 HR02
#>       0       0    0     0     0     0    0    0     0     0    0    0    0
#>       0.1    20   25     5     3    23    4    4     1     2    3   25    3
#>       0.2     8   14     3     3    16    0    0     0     1    0    4    2
#>       0.3     1    2     0     0     2    0    0     0     0    0    1    0
#>       0.4     0    2     0     0     0    0    0     0     0    0    1    0
#>       0.5    27   28     5     3    27    4    4     1     2    3   28    3
#>       0.6     1    1     0     0     1    0    0     0     0    0    1    0
#>       0.7     0    0     0     0     1    0    0     0     0    0    0    0
#>       0.8     4   12     3     3    15    0    0     0     1    0    1    2
#>       0.9     0    0     0     0     1    0    0     0     0    0    0    0
#>       1      28   28     5     3    27    4    4     1     2    3   29    3
#>       1.1     0    0     0     0     0    0    0     0     0    0    1    0
#>       1.2     4    7     2     3    14    0    0     0     2    0    0    2
#>       1.3     0    1     0     0     2    0    0     0     0    0    0    0
#>       1.4     0    0     0     0     1    0    0     0     0    0    0    0
#>       1.5    27   22     4     3    26    4    4     1     1    3   24    3
#>       1.6     0    1     0     0     0    0    0     0     0    0    0    0
#>       1.7     0    1     0     0     1    0    0     0     0    0    0    0
#>       1.8     4    2     1     2    12    0    0     0     1    0    0    2
#>       2      47   41     7     1    36    8    8     2     3    6   54    7
#>       3      29   20     3     0     7    4    4     1     0    3   30    0
#>       4      34    5     5     0     3   11   11     3     0    8   78    0
#>       5       3    0     0     0     0    1    4     1     0    3   30    0
#>       6       0    0     0     0     0    0    4     1     0    3   28    0
#>       7       0    0     0     0     0    0    4     1     0    3   25    0
#>       8       0    0     0     0     0    0    3     1     0    2   24    0
#>       9       0    0     0     0     0    0    1     0     0    1   17    0
#>       10      0    0     0     0     0    0    0     0     0    1   11    0
#>       11      0    0     0     0     0    0    0     0     0    1    3    0
#>       12      0    0     0     0     0    0    0     0     0    0    3    0
#>       13      0    0     0     0     0    0    0     0     0    0    0    0
#>          site
#> dpth_clss HR03 HR04 HR05 LC02 P6FGG P7CBI PR-17 PR-28 PRV70 RR-01 RR-06 RR-13
#>       0      0    0    0    0     0     0     1     0     1     0     0     0
#>       0.1    3    3    3    4    17    18    19    23    20     5     7     7
#>       0.2    0    0    0    0     1     2    10     4     5     3     2     2
#>       0.3    0    0    0    0     0     0     3     2     3     0     0     0
#>       0.4    0    0    0    0     0     0     0     0     0     0     0     0
#>       0.5    3    3    3    4    18    18    24    30    26     5     7     7
#>       0.6    0    0    0    0     0     0     0     0     0     0     0     0
#>       0.7    0    0    0    0     0     0     0     0     0     0     0     0
#>       0.8    0    0    0    0     0     1    10     1     2     3     1     2
#>       0.9    0    0    0    0     0     0     0     1     0     0     0     0
#>       1      3    3    3    4    18    18    24    29    25     5     7     7
#>       1.1    0    0    0    0     0     0     0     0     0     0     0     0
#>       1.2    0    0    0    0     0     1    10     0     2     2     1     1
#>       1.3    0    0    0    0     0     0     0     0     0     0     0     0
#>       1.4    0    0    0    0     0     0     0     0     0     0     0     0
#>       1.5    3    3    3    4     9     9    24    24    22     5     7     6
#>       1.6    0    0    0    0     0     0     0     0     1     0     0     0
#>       1.7    0    0    0    0     0     0     2     0     0     0     0     0
#>       1.8    0    0    0    0     0     1     8     0     1     0     1     1
#>       2      6    6    6    8    27    27    35    53    43     6    14    13
#>       3      3    3    3    3    18    18    14    30    27     2     6     6
#>       4      8    9    9    2    29    29    24    65    27     6     8     5
#>       5      2    3    3    0    15    15     5    25    12     2     1     1
#>       6      0    5    3    0    11    13     0    13     3     0     0     0
#>       7      0    1    3    0     8    10     0     6     0     0     0     0
#>       8      0    0    2    0     7     9     0     3     0     0     0     0
#>       9      0    0    1    0     7     7     0     0     0     0     0     0
#>       10     0    0    0    0     4     5     0     0     0     0     0     0
#>       11     0    0    0    0     2     3     0     0     0     0     0     0
#>       12     0    0    0    0     2     2     0     0     0     0     0     0
#>       13     0    0    0    0     0     1     0     0     0     0     0     0
#>          site
#> dpth_clss RR-19 RR-20
#>       0       0     0
#>       0.1     4     3
#>       0.2     0     3
#>       0.3     0     0
#>       0.4     0     0
#>       0.5     4     3
#>       0.6     0     0
#>       0.7     0     0
#>       0.8     0     3
#>       0.9     0     0
#>       1       4     3
#>       1.1     0     0
#>       1.2     0     3
#>       1.3     0     0
#>       1.4     0     0
#>       1.5     4     3
#>       1.6     0     0
#>       1.7     0     0
#>       1.8     0     3
#>       2       8     5
#>       3       4     0
#>       4       4     0
#>       5       0     0
#>       6       0     0
#>       7       0     0
#>       8       0     0
#>       9       0     0
#>       10      0     0
#>       11      0     0
#>       12      0     0
#>       13      0     0
rm(tmp)
```

It appears there are several different sampling conventions combined
here: \* Sampling with downcast at uneven shallow depths \* Sampling at
shallow half meter intervals to two meters and at one meter intervals
beyond that point.

## How often was each site sampled?

We make an assumption here that sampling on one day is all related.

``` r
tmp <- irr_data %>%
  group_by(site, sample_date) %>%
  summarize(was_sampled = sum(! is.na(depth)) > 1,
            .groups = 'drop')
xt <- xtabs(~ sample_date + site, data = tmp)
tot <- colSums(xt)
tot
#> BMR02  CBPR CR-31 CR-44 EEB18  FR03  FR04 FR05A FR05B  FR07  FR09  HR02  HR03 
#>    28    30     5     3    28     4     4     1     2     3    30     3     3 
#>  HR04  HR05  LC02 P6FGG P7CBI PR-17 PR-28 PRV70 RR-01 RR-06 RR-13 RR-19 RR-20 
#>     3     3     4    18    18    24    30    26     5     7     7     4     3
```

We identify the sites with the richest data history, and focus on them.

``` r
(preferred_sites <- names(tot[tot > 20]))
#> [1] "BMR02" "CBPR"  "EEB18" "FR09"  "PR-17" "PR-28" "PRV70"
rm(tmp, xt)
```

We see clearly that certain sites had data collected much more
frequently. These are the same sites for which we have more abundant
sonde data. we will need to correlate these data in some intelligent
way.

``` r
irr_data %>%
  filter(year == 2018) %>%
  filter(site %in% preferred_sites) %>%
ggplot(aes(sample_date, depth, color = irr_water)) +
  geom_jitter(width = 3) +
   scale_colour_gradient(name = "Light (µmol/m2/s)",
                         low = scales::muted("lightblue", l = 10),
                         high = scales::muted("lightblue", l = 80)) +
   scale_y_reverse() +
  theme_cbep(base_size = 12) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 12),
        legend.text =  element_text(size = 10),
        axis.ticks.length.x = unit(0, 'in')) +
  guides(color = guide_colourbar(title.position="top", barheight = .5))
```

<img src="DEP_Irradiance_Review_files/figure-gfm/plot_date_by_depth_2018-1.png" style="display: block; margin: auto;" />

``` r
irr_data %>%
  filter(year == 2018) %>%
  filter(site %in% preferred_sites) %>%
ggplot(aes(depth, irr_pct, color = factor(sample_date))) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'lm', formula = y ~ x) +
  scale_color_discrete(name = 'Date') +
  xlab('Irradiance') +
  ylab('Depth') +
  scale_y_reverse() +
  scale_x_log10() +
  theme_cbep(base_size = 12) +
  theme(#legend.position = 'bottom',
        legend.title = element_text(size = 12),
        legend.text =  element_text(size = 10),
        axis.ticks.length.x = unit(0, 'in')) +
  #guides(color = guide_colorbar(title.position="top", barheight = .5)) +
  facet_wrap("site")
```

<img src="DEP_Irradiance_Review_files/figure-gfm/plot_light_by_depth-1.png" style="display: block; margin: auto;" />

# Calculation of Light Extinction Coefficients

We develop an estimates of K for each unique depth profile, using linear
regression. This is an ideal context for use of nested tibbles, as we
eventually can drop the raw data and focus only on the derived
quantities.

``` r
k_data <- irr_data %>%
  group_by(site, sample_date) %>%
  nest() %>%
  mutate(the_lm = map(data, 
                      function(dat) lm(log(irr_pct) ~ depth, data = dat))) %>%
  mutate(k_est = map(the_lm, 
                     function(mod) -summary(mod)$coef[2, 1]), # extracts slope
         k_se = map(the_lm, 
                     function(mod) summary(mod)$coef[2, 2]), # extracts SE
         k_n =  map(data, 
                     function(dat) sum(! is.na(dat$irr_pct)))
         ) %>%
  mutate(site_name = map(data, function(dat) first(dat$site_name)[[1]]),
         month = map(data, function(dat) first(dat$month)[[1]]),
         year = map(data, function(dat) first(dat$year)[[1]]),
         start_hour = map(data, function(dat) min(dat$hour)[[1]]),
         doy = map(data, 
                   function(dat) as.numeric(format(min(sample_date), 
                                                   format = '%j')))) %>%

  select (-data, -the_lm) %>%
  unnest(everything()) %>%
  relocate(site_name, site, sample_date,  
           year, month, doy, start_hour) %>%
  filter(k_n >4) %>%         # Removes sample with only two light values 
  arrange(site, sample_date)
```

PRV70 on 2020-09-24 had only two samples, making the estimates of k
unstable and physically impossible. As a result, that site strongly
influences models looking at k by location and date. We chose to delete
any record (location by date) with fewer than five light values on which
to base an estimate of k.

\#\#Reorder `site` Factor For some reason, we were having trouble
reordering the levels in `site` according to light attenuation estimates
within the usual dplyr workflows.

We resort to handling this manually (although this factor ordering will
be lost when importing from `*.csv` files, it’s convenient to retain the
code here for reference.)

``` r
(my_lvls <- levels(fct_reorder(k_data$site, k_data$k_est, mean,  na.rm = TRUE)))
#>  [1] "P7CBI" "FR09"  "P6FGG" "FR07"  "FR05A" "HR05"  "FR04"  "BMR02" "LC02" 
#> [10] "HR04"  "EEB18" "CBPR"  "FR03"  "FR05B" "PR-28" "HR03"  "RR-19" "PR-17"
#> [19] "HR02"  "PRV70" "RR-01" "RR-13" "RR-06" "RR-20" "CR-31" "CR-44"

k_data <- k_data %>%
  mutate(site = factor(site, levels = my_lvls))
```

# Output Light Extinction Coefficient Data

``` r
write_csv(k_data, 'light_extinction_data.csv')
```
