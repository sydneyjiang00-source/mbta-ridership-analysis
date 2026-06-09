# MBTA Bus Ridership Analysis (2016–2024)

An exploratory analysis of Massachusetts Bay Transportation Authority (MBTA) bus ridership from 2016 to 2024, focusing on the impact of the COVID-19 pandemic and the differential recovery patterns across routes, day types, and time periods. The project combines time-series trend analysis, route-level variability measures, and a spatial map of the busiest routes.

## Authors

Xin Ni Jiang · Rui Ye · Yujia Li · Yu Lu

## Overview

The analysis is organized into five sections:

1. **Data Cleansing** — type conversion, derived flags (`covid_2020`, `day_group`), and period bucketing (Pre-COVID, COVID-2020, Recovery, Post-Recovery).
2. **Load Analysis** — top-10 passenger loads per year, average load trends with a 2020 marker, and route stability via the coefficient of variation.
3. **Routes Analysis** — most frequent routes in the top 10 by period and day group, plus a spatial map of route rankings over time.
4. **Ridership Analysis** — annual boarding trends, route recovery percentages relative to a 2016–2019 baseline, and shifts in each route's share of total system ridership.
5. **Recovery Rate Analysis** — year-over-year growth rates, a COVID-drop vs. recovery-rate comparison, and route-level decline/recovery distributions.

## Repository structure

```
.
├── Group_8_code.Rmd      # Full analysis (R Markdown)
├── README.md
└── data/
    ├── MBTA_Bus_Ridership_2016-2024.csv   # ridership data (see Data section)
    └── mbtabus/                            # MBTA bus route shapefile
```

> The code reads from a `data/` folder using relative paths. Create a `data/` folder in the repository root and place the CSV and the `mbtabus` shapefile directory inside it before knitting.

## Data

The ridership data comes from the **MBTA / MassDOT Open Data Portal**, dataset *"MBTA Bus Ridership by Time Period, Season, Route/Line, and Stop"*: https://mbta-massdot.opendata.arcgis.com

The route geometries (`mbtabus`) are an MBTA bus route shapefile from the same portal. The data files are not committed to this repository; download them from the portal and place them under `data/` as shown above.

## Requirements

R (4.x) with the following packages:

```r
install.packages(c("arrow", "tidyverse", "stringr", "dplyr",
                   "forcats", "sf", "tmap"))
```

`sf` and `tmap` rely on system geospatial libraries (GDAL, GEOS, PROJ). On macOS these install automatically with the packages; on Linux you may need to install them separately.

## How to run

Open `Group_8_code.Rmd` in RStudio and click **Knit**, or from the R console:

```r
rmarkdown::render("Group_8_code.Rmd")
```

The output is set to `github_document`, so knitting produces a `.md` file (and a folder of figures) that GitHub renders directly in the browser.

## Key findings

- A sharp ridership drop in 2020 across all routes, followed by an uneven recovery.
- Weekday and weekend loads recovered at different rates, with weekend patterns shifting relative to the pre-COVID baseline.
- Some routes gained share of total system ridership post-2020 while others lost it, indicating changing travel patterns rather than a uniform decline.
