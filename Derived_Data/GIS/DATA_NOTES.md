# GIS Data
GIS data was assembled principally based on data in the CSV files,
"dep_locations.csv" and "gis_results.csv".  Those files were generated via the R
Notebook, "DEP_Nutrients_GIS_Subset.Rmd".  See the Notebook for details.

## Location Data
Data from "dep_locations.csv" was loaded into GIS as a table.  A feature layer
was create by displaying the XY data (longitudes and latitudes) from that file,
and exporting as a feature layer.  The data layer was saved in a Personal
Geodatabase because shapefiles generally do not accept NULL values in their
attributes.

## Attribute Data
Specific data on nutrients, chlorophyll and turbidity were loaded from
"gis_results.csv".  This file contains descriptive statistics by site derived
from a specific subset of available DEP data. Analysis shows that  year to year
and seasonal variation is substantial, so we focused on periods of time when DEP
sampling was reasonably consistent across sites.  This omitted data from some
sites entirely, when those sites were seldom sampled or inconsistently sampled.
See the R Notebook "DEP_Nutrients_GIS_Subset.Rmd" for details.

Data on light extinction coefficients was imported from "k_summary_by_site.csv",
which contains summary statistics.  Details on development of the light 
extinction coefficients can be found in the notebook 
"DEP_Irradiance_Analysis.Rmd".

## Combining the Data
Data from the attribute tables was added to the features by importing the 
attribute data as separate table, joining the tables by Site ID, and copying 
data from the joined tables into the feature data, and finally removing the
joins.  Data imported to the feature layer represent median values for the 
specific data subset for GIS mapping.  Data for the light extinction 
coefficients represent 

## Attribute Definitions
Data in the dep_locations feature data have the following attributes:

| Site      |   Definition                                            |
|:----------|:--------------------------------------------------------| 
|site_name  |   Original name of the site from the DEP source data    |
|short_name |   Simplified name for graphics and GIS display          |
|site       |   Site Code from DEP data, with depth designations (if any) removed |
|Latitude   |   Latitude.  Interpreted here as WGS 1984               |
|Longitude  |   Longitude.  Interpreted here as WGS 1984              |
|NH4        |   Ammonium concentration, mg/l, median values           |
|NOX        |   Nitrate + nitrite concentration,  mg/l, median values |
|DIN        |   Dissolved Inorganic Nitrogen (sum of NH4 and NOX values) mg/l, median values |
|Org_N      |   Organic nitrogen. (Difference between TN and DIN)  mg/l, median values |
|TN         |   Total nitrogen,  mg/l, median values                  |
|K          |   Light extinction coefficients, 1/m, mean values       |
|Years      |   Years in which nutrient (NOT K)  data were collected. |



