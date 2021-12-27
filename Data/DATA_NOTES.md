# DEP Data Related to Nutrients in Casco Bay
All data tables were derived from spreadsheet data received directly from Maine
DEP. Most data files represent specific subsets of the source data, with data 
grouped by theme and the dimensionality of the underlying data.  For example,
Secchi depth is collected over an entire depth profile, so values depend only on
location, not depth. Most nutrient data is collected as grab samples, and depth
matters.  Data collected by sondes are collected as vertical profiles, with
groups of observations with the same (similar) date and time. Raw light
intensity data is collected as vertical profiles, but comparisons between
samples at different depth are what matter most.

## Nutrient Data
### `dep_nutrient_data.csv`
Data on nutrients, chlorophyll and suspended solids. This represents data
derived principally from discrete physical samples. Most data represents results
of laboratory analyses of water samples.

For most data types, we created two "flag" values, based on data qualifiers in
the source data.  We split data qualifiers into two categories, one for 
"non-detects", here considered left-censored values, and one for any other data
quality qualifiers found in the source data. We did not retain information on
the exact data quality flag, in part because the meaning of the flags may vary
based on the original data source.  Without full metadata, we can only note that
someone upstream of us flagged the data.  Most flags represent data below
reliable quantitatation limits, although a few indicate sample handling or hold
time issues.  Raw data and our data processing code, along with detailed 
explanations are available at a companion 
[GitHub repository](https://github.com/CBEP-SoCB-Details/DEP_Nutrients.git).

Variable Name |  Meaning                     | Units                |  
--------------|------------------------------|----------------------|  
site_name     | DEP "Site ID"                |                      |  
site          | DEP "Sample Point ID" without depth designation     |   
depth_designation | Depth category from source data | Blank, "SUR", "BOT", "MAX" * |
dt            | Date of data collection      | %m/%d/%Y format      |  
month         | Month, derived from date     | Three letter codes   |  
year          | Year of sample collection    | Four digit integer   |  
time          | Time of sample collection    | %H:%M, 24 hour clock |  
hour          | Hour of data collection      | integer              |  
depth         | Sample Depth	               | Meters               |
chl           | Chlorophyll a concentration  | ug/l        **       |
chl_cens      | Flag for left censored values | TRUE / FALSE        |
chl_flag      | Flag indicating data quality flags in source data   | TRUE / FALSE |
phaeo         | Phaeophytin concentration    |  ug/l                |
phaeo_flag    | Flag indicating data quality flags in source data   | TRUE / FALSE |  
phaeo_cens    | Flag for left censored values | TRUE / FALSE        |
nox_n         | Nitrate plus nitrite concentration |  mg/l as N     |
nox_n_cens    | Flag for left censored values | TRUE / FALSE        |
nox_n_flag    | Flag indicating data quality flags in source data   | TRUE / FALSE |  
nh4_n         | Ammonium concentration        |  mg/l as N          |
nh4_n_cens    | Flag for left censored values | TRUE / FALSE        |
nh4_n_flag    | Flag indicating data quality flags in source data   | TRUE / FALSE |  
tn	           | Total Nitrogen concentration  | mg/L as N           |
tn_flag       | Flag indicating data quality flags in source data   | TRUE / FALSE |  
op_p          | Orthophosphate concentration  | mg/l as P           |
op_p_flag     | Flag indicating data quality flags in source data   | TRUE / FALSE |  
tp	           |  Total phosphorus concentration | mg/l as P         |
tp_cens       | Flag for left censored values | TRUE / FALSE        |
tp_flag       | Flag indicating data quality flags in source data   |  TRUE / FALSE |  
tss           |  Total Suspended solids       | mg/l                |
tss_cens      | Flag for left censored values | TRUE / FALSE        |
tss_flag      | Flag indicating data quality flags in source data   |  TRUE / FALSE |  
Sample Comments  | Comments associated with sample collection       |               |
Validation Comments | Comments associated with data validation
Latitude      | Latitude (WGS 1984)           | Decimal degrees
Longitude     | Longtude (WGS 1984)           | Decimal degrees


\* "Sur" = Surface, "BOT" = Bottom, "MAX" = Chlorophyll Maximum.  Designations
not always provided in source data, so use with caution, or code your own
labels.

\** Chlorophyll data included here does not include data designated in the
source data as having been collected by sonde. Data does appears to derive from
multiple analytic procedures, some with and some without companion phaeophytin
values. Data provided by DEP did not include information on laboratory methods.
Thus it is not clear whether data from different rounds of sampling are fully
compatible.

We found no formal designation of the time coordinate in the DEP metadata that 
was shared with us.  Time coordinates are thought to be local clock time, so 
Eastern Daylight time (UTC - 4) for most observations, since relatively little
data was collected between November and April.

## Locations and GIS Data
### `dep_locations.csv`
Geographic data , aligning site codes and site names with latitudes and
longitudes.  This data set was used to generate a GIS data layer, subsequently
used to map selected of results of analyses.


## Light Attenuation Data
### `dep_irradiance_data.csv`
Data on light intensity collected on a vertical profile, usually only up to a
depth of a few meters.  Data is (usually) coupled to matching data
on surface irradiance, to allow estimation of light extinction coefficients.

This file contains "raw" QA/QC'd irradiance data

Variable Name |  Meaning                     | Units                 |  
--------------|------------------------------|-----------------------|  
site_name     | DEP "Site ID"                |                       |  
site          | DEP "Sample Point ID" without depth designation |    |  
sample_date   | Date of sample collection    | yyyy-mm-dd format     |
month         | Month, derived from date     | Three letter codes    |
year          | Year, derived from date      |                  |    |
time          | time of sample               | 24 hour clock, hh:mm format |
hour          | hour, derived from time      |                       |
depth         | Sample Depth	               | Meters                |
irr_air       | Irradiance (air)             | µmol/m2/s             |
irr_water     | Irradiance (surface water)   | µmol/m2/s             |
irr_pct_      | Irradiance (% of air in surface water) | %           |

irr_pct here is as provided by DEP.

We found no formal designation of the time coordinate in the DEP metadata that 
was shared with us.  Time coordinates are thought to be local clock time, so 
Eastern Daylight time (UTC - 4) for most observations, since relatively little
data was collected between November and April.

### `light_extinction_data.csv`
This file contains light extinction coefficients based on fitting a log-linear
model.

#### Review of theory
Light attenuation is often measured as 
$$
I_d = I_0 e^{-kz}
$$
Where $z$ is depth.

$$
\frac{I_d}{I_0} = e^{-kz}
$$


$$
log(\frac{I_d}{I_0}) = -kz
$$

Note that this produces a value of k at each depth. If we assume this theory is
accurate and light attenuation is vertically uniform, we can average across 
depths to improve accuracy. 

$$ k = \frac{1}{-z} \times log(\frac{I_d}{I_0}) $$

Where we recast values as percentages of surface light, we can estimate k~d as

$$ k \approx - \frac{log(I_d)}{z}$$

We want to recast that as a linear regression problem.

Y = mx + b

log(Id) = -kz

So, K can be estimated as the negative of linear coefficient of depth in a 
linear model regressing irradiance percent by depth.

We conducted that analysis on the raw DEP irradiance data, producing a separate
estimate of k for each unique vertical profile ( by location, date and time).


Variable Name |  Meaning                  | Units                |  
--------------|---------------------------|----------------------|  
site_name     | DEP "Site ID"             | Character string     |
site          | DEP "Sample Point ID"     | Character string     |  
sample_date   | Date of sample collection | yyyy-mm-dd format    |
year          | Year, derived from date   |                      |
month         | Month, derived from date  | Three letter codes   |
doy           | day of year (Julian day)  |                      |
start_hour    | Hour light sampling began |                      |
k_est         | Estimate of light extinction coefficient | 1 / m |
k_se          | Standard error of that estimate, based on regression |  
K_n           | Number of observations used to estimate k. |        |

### `k_summary_by_site.csv`
We have multiple estimates of k for most locations, as we have anywhere from
a few to a couple dozen light profiles from each site.  For GIS presentation

We estimate long-term "typical" light extinction coefficients by site, in two
ways.  First, we calculate simple observed averages and standard errors based
on the observed values of K.  However, the sampling history of different sites 
are very different,and water clarity varies seasonally and year to year. 


Column Name     | Contents              
----------------|------------------------------------------
site_name       | DEP "Site ID" 
site            | DEP "Sample Point ID" without depth designation  
k_mean          | Arithmetic mean of all K estimates for the site (k in 1/m) 
k_gm            | Geometric mean of K estimates for the site
k_se            | Standard error of mean 
k_observs       | Number of K values on which means are based
light_observs   | Total number or light observations on which means are based
em_mn           | Predicted geometric mean based on weighted hierarchical model
em_se           | Standard erro of that estimate.
df              | Error degrees of freedom for the model
lower.CL        | Lower 95% confidence interval of model estimate
upper.CL        | Upper 95% Confidence Interval

### `dep_secchi_data.csv`
Data on secchi depths. Collected at point location. Only meaningful for a whole 
water column observations.
    
Variable Name |  Meaning                  | Units                |  
--------------|---------------------------|----------------------|   
site_name     | DEP "Site ID"             |                      |  
site          | DEP "Sample Point ID" without depth designation  |   
dt            | Date of data collection   | %m/%d/%Y format      |  
month         | Month, derived from date  | Three letter codes   |  
year          | Year of sample collection | Four digit integer   |  
time          | Time of sample collection | %H:%M, 24 hour clock |  
hour          | Hour of data collection   |                      |  
secchi        | Secchi depth or water depth | meters             |  
secchi_on_bottom | Flag indicating if Secchi disk was still visible on bottom | TRUE / FALSE |  
Sample Comments     | Comments            |                      |                                    
Validation Comments |  Comments           |                      |  

We found no formal designation of the time coordinate in the DEP metadata that 
was shared with us.  Time coordinates are thought to be local clock time, so 
Eastern Daylight time (UTC - 4) for most observations, since relatively little
data was collected between November and April.

### `dep_sonde_data.csv`
Vertical profile data derived from downcasts of water quality sondes. This data
is collected at regular or (usually) irregular depths at more or less one time,
data that is anchored to specific location, date, and depth. Location, depth and
time of sample collection matter.
   
Variable Name |  Meaning                  | Units                |  
--------------|---------------------------|----------------------|   
site_name     | DEP "Site ID"             |                      |  
site          | DEP "Sample Point ID" without depth designation  |   
dt            | Date of data collection   | %m/%d/%Y format      |  
month         | Month, derived from date  | Three letter codes   |  
year          | Year of sample collection | Four digit integer   |  
time          | Time of sample collection | %H:%M, 24 hour clock |  
hour          | Hour of data collection   |                      |  
secchi        | Secchi depth or water depth | meters             |  
depth         | Water depth, from pressure sensor on sonde |  Meters |  
temp	        | Temperature               | Degrees Celsius
salinity	     | Salinity from conductivity | PSU  ( ~ PPT)
ph            | pH                        | NBS Scale (ion selective electrode)
pctsat	     | Percent oxygen saturation | Percent
do	           | Dissolved oxygen          | mg/l
chl_a_sonde   | Chlorophyle, by fluorescence, not calibrated to lab measurements | ug/l
turbidity     | Turbidity by sonde        | NTU
turbidity_cens | Flar indicating left censored turbidity measurement  | TRUE / FALSE

We found no formal designation of the time coordinate in the DEP metadata that 
was shared with us.  Time coordinates are thought to be local clock time, so 
Eastern Daylight time (UTC - 4) for most observations, since relatively little
data was collected between November and April.
