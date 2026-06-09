MA \[46\]15 Group 8
================
**XIN NI JIANG** **Rui Ye** **Yujia Li** **Yu Lu**

## Data Cleansing

``` r
library(arrow)
```

    ## 
    ## Attaching package: 'arrow'

    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::duration() masks arrow::duration()
    ## ✖ dplyr::filter()       masks stats::filter()
    ## ✖ dplyr::lag()          masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(stringr)
library(dplyr)
library(forcats)
library(sf)
```

    ## Linking to GEOS 3.13.0, GDAL 3.8.5, PROJ 9.5.1; sf_use_s2() is TRUE

``` r
library(tmap)

csv_mbta = read_csv("~/Desktop/MBTA_Bus_Ridership_2016-2024.csv")
```

    ## Rows: 7879638 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): day_type_id, day_type_name, route_id, route_variant, season, stop_...
    ## dbl  (8): alightings, boardings, direction_id, load_, sample_size, stop_id, ...
    ## time (1): trip_start_time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
csv_mbta = csv_mbta |> 
  mutate(
    boardings = as.numeric(boardings), 
    alightings = as.numeric(alightings),
    covid_2020 = ifelse(year_from_name == 2020, 1, 0),
    day_group = ifelse(day_type_name == "weekday", "Weekday", "Weekend"),
    period = case_when(
      year_from_name < 2020 ~ "Pre-COVID",
      year_from_name == 2020 ~ "COVID-2020",
      year_from_name %in% 2021:2022 ~ "Recovery",
      year_from_name >= 2023 ~ "Post-Recovery")) |> 
  drop_na(boardings, alightings)
```

## Load Analysis

# Top 10 Loads Each Year

``` r
top10_loads = csv_mbta |> 
  group_by(year_from_name, day_group) |>
  arrange(desc(load_)) |>
  slice_head(n = 10) |>
  ungroup()

ggplot(top10_loads, aes(x = year_from_name, y = load_, color = day_group)) +
  geom_smooth(se = FALSE) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Top 10 Bus Load Trends: Weekday vs Weekend",
    subtitle = "Highest passenger loads per year",
    x = "Year",
    y = "Load (Passengers)",
    color = "Day Type")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](Group-8-code_files/figure-gfm/load-analysis-1.png)<!-- -->

# Average Load Trends

``` r
top10_loads_summary = top10_loads |>
  group_by(year_from_name, period, day_group) |>
  summarise(
    avg_load = mean(load_),
    median_load = median(load_),
    max_load = max(load_),
    min_load = min(load_),
    .groups = "drop")

ggplot(top10_loads_summary, aes(x = year_from_name, y = avg_load, color = day_group)) +
  geom_line(linewidth = 1) + 
  geom_point(size = 3) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Average Load of Top 10 Routes Over Time",
    subtitle = "Clear COVID-19 Impact and Differential Recovery Patterns",
    x = "Year",
    y = "Average Load",
    color = "Day Type")
```

![](Group-8-code_files/figure-gfm/average-load-1.png)<!-- -->

# Load Variability (Stability Analysis)

``` r
variability = top10_loads |>
  group_by(route_id, day_group) |>
  summarise(
    mean_load = mean(load_),
    sd_load = sd(load_),
    cv = sd_load / mean_load,
    .groups = "drop") |>
  arrange(desc(cv)) |>
  slice_max(cv, n = 20)

ggplot(variability, aes(x = reorder(route_id, cv), y = cv, fill = day_group)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Variable Routes (by Coefficient of Variation)",
    x = "Route ID",
    y = "Coefficient of Variation")
```

![](Group-8-code_files/figure-gfm/variability-1.png)<!-- -->

``` r
print(variability)
```

    ## # A tibble: 21 × 5
    ##    route_id day_group mean_load sd_load    cv
    ##    <chr>    <chr>         <dbl>   <dbl> <dbl>
    ##  1 28       Weekend        43.1   19.9  0.462
    ##  2 28       Weekday        50.1   19.4  0.388
    ##  3 111      Weekend        50.5   18.0  0.357
    ##  4 743      Weekday        55.1   15.3  0.278
    ##  5 429      Weekend        69     12.1  0.176
    ##  6 109      Weekend        61.8   10.3  0.167
    ##  7 455      Weekend        50.4    6.76 0.134
    ##  8 39       Weekday        66.8    7.19 0.108
    ##  9 749      Weekday        92      9.71 0.106
    ## 10 39       Weekend        57.5    5.87 0.102
    ## # ℹ 11 more rows

## Routes Analysis

# Most Frequent Routes in Top 10

``` r
route_total = top10_loads |>
  count(route_id) |>
  slice_max(n, n = 10) |> 
  rename(total_appearances = n)

top_routes_detail = top10_loads |>
  filter(route_id %in% route_total$route_id) |>
  count(route_id, day_group, period) |>
  left_join(route_total, by = "route_id")

ggplot(top_routes_detail, aes(x = reorder(route_id, total_appearances), y = n, fill = day_group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ period, nrow = 1) +
  labs(
    title = "Top Routes in Top 10 by Period and Day Group",
    x = "Route ID",
    y = "Number of Top-10 Appearances",
    fill = "Day Group")
```

![](Group-8-code_files/figure-gfm/routes-frequency-1.png)<!-- -->

``` r
top_routes_share = top_routes_detail |>
  group_by(route_id) |>
  mutate(share = n / sum(n)) |>
  ungroup()

ggplot(top_routes_share, aes(x = reorder(route_id, total_appearances), y = share, fill = period)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Composition of Top-10 Appearances by Period",
    x = "Route ID",
    y = "Share of Appearances",
    fill = "Period")
```

![](Group-8-code_files/figure-gfm/routes-frequency-2.png)<!-- -->

# Rankings Over Time on map!

``` r
routes_sf = st_read("~/Desktop/mbtabus", quiet = TRUE)
```

    ## Warning in CPL_read_ogr(dsn, layer, query, as.character(options), quiet, :
    ## automatically selected the first layer in a data source containing more than
    ## one.

``` r
top10_rank = top10_loads |> 
  group_by(year_from_name, day_group) |>
  mutate(rank = dense_rank(desc(load_))) |>
  ungroup()
route_stats = top10_rank |>
  group_by(route_id) |>
  summarise(
    total_appearances = n(), 
    avg_rank = mean(rank, na.rm = TRUE), 
    avg_load = mean(load_, na.rm = TRUE), 
    .groups = "drop")
routes_joined = routes_sf |>
  inner_join(route_stats, by = c("MBTA_ROUTE" = "route_id"))  

routes_joined_xy = routes_joined |>
  st_zm(drop = TRUE, what = "ZM")
routes_dissolved = routes_joined_xy |>
  group_by(MBTA_ROUTE) |>
  summarise(
    total_appearances = first(total_appearances),
    geometry = st_union(geometry),
    .groups = "drop")
routes_label_points = st_centroid(routes_dissolved)
```

    ## Warning: st_centroid assumes attributes are constant over geometries

``` r
tmap_mode("plot")
```

    ## ℹ tmap modes "plot" - "view"
    ## ℹ toggle with `tmap::ttm()`

``` r
top10_map = 
  tm_basemap("OpenStreetMap") + 
  tm_shape(routes_joined_xy) +
    tm_lines(
      col = "total_appearances",
      scale = 2, 
      lwd = 3, 
      palette = "red",
      style = "quantile",
      title.col = "Top 10 routes frequency") +
  tm_shape(routes_label_points) +
    tm_text(
      text = "MBTA_ROUTE",
      size = 1,
      col = "black",
      fontface = "bold",
      bg.color = "white",
      bg.alpha = 0.6,
      legend.show = FALSE) +
  tm_layout(
    main.title = "Route Rankings Over Time",
    main.title.position = "center",
    main.title.fontface = "bold",
    legend.outside = TRUE)
```

    ## 
    ## ── tmap v3 code detected ───────────────────────────────────────────────────────
    ## [v3->v4] `tm_lines()`: instead of `style = "quantile"`, use col.scale =
    ## `tm_scale_intervals()`.
    ## ℹ Migrate the argument(s) 'style', 'palette' (rename to 'values') to
    ##   'tm_scale_intervals(<HERE>)'[v3->v4] `tm_lines()`: migrate the argument(s) related to the legend of the
    ## visual variable `col` namely 'title.col' (rename to 'title') to 'col.legend =
    ## tm_legend(<HERE>)'[v3->v4] `tm_tm_lines()`: migrate the argument(s) related to the scale of the
    ## visual variable `lwd` namely 'scale' (rename to 'values.scale') to lwd.scale =
    ## tm_scale_continuous(<HERE>).
    ## ℹ For small multiples, specify a 'tm_scale_' for each multiple, and put them in
    ##   a list: 'lwd.scale = list(<scale1>, <scale2>, ...)'[tm_text()] Argument `legend.show` unknown.[v3->v4] `tm_layout()`: use `tm_title()` instead of `tm_layout(main.title = )`

``` r
top10_map
```

![](Group-8-code_files/figure-gfm/route-map-1.png)<!-- -->

## Ridership Analysis

# Annual Trend

``` r
yearly_avg = csv_mbta |>
  group_by(year_from_name) |>
  summarise(
    avg_boardings = mean(boardings, na.rm = TRUE))

ggplot(yearly_avg, aes(year_from_name, avg_boardings)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Annual MBTA Bus Ridership (2016–2024)",
    subtitle = "Average boardings per observation across all routes",
    x = "Year",
    y = "Average Boardings")
```

![](Group-8-code_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Route Ridership Profile Analysis

``` r
route_baseline = csv_mbta |>
  filter(year_from_name %in% 2016:2019) |>
  group_by(route_id) |>
  summarise(baseline_avg = mean(boardings, na.rm = TRUE)) |>
  mutate(
    volume_class = cut(
      baseline_avg,
      breaks = quantile(baseline_avg, c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low volume", "Medium volume", "High volume"),
      include.lowest = TRUE))

route_2024 = csv_mbta |>
  filter(year_from_name == 2024) |>
  group_by(route_id) |>
  summarise(
    avg_2024 = mean(boardings, na.rm = TRUE),
    .groups = "drop")

route_volume_recovery <- route_baseline |>
  inner_join(route_2024, by = "route_id") |>
  mutate(
    recovery_ratio = avg_2024 / baseline_avg,
    recovery_pct = (recovery_ratio - 1) * 100)

ggplot(route_volume_recovery, aes(x = volume_class, y = recovery_pct)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Route Recovery Percentage (2024 vs. 2016–2019 Baseline)",
    x = "Route Volume Class (Pre-COVID)",
    y = "Recovery % relative to baseline")
```

![](Group-8-code_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Route Dependency Change

``` r
route_share = csv_mbta |>
  filter(year_from_name %in% c(2019, 2024)) |>
  group_by(year_from_name, route_id) |>
  summarise(
    total_boardings = sum(boardings, na.rm = TRUE),
    .groups = "drop")

system_totals = route_share |>
  group_by(year_from_name) |>
  summarise(
    system_total = sum(total_boardings),
    .groups = "drop")

route_share_pct = route_share |>
  left_join(system_totals, by = "year_from_name") |>
  mutate(
    pct_of_system = total_boardings / system_total)

route_share_change = route_share_pct |>
  select(route_id, year_from_name, pct_of_system) |>
  pivot_wider(
    names_from = year_from_name,
    values_from = pct_of_system,
    names_prefix = "year_") |>
  drop_na(year_2019, year_2024) |>
  mutate(
    pct_point_change = (year_2024 - year_2019) * 100)

top_gain = route_share_change |>
  arrange(desc(pct_point_change)) |>
  slice_head(n = 10)

top_loss = route_share_change |>
  arrange(pct_point_change) |>
  slice_head(n = 10)

ggplot(top_gain, aes(x = reorder(route_id, pct_point_change),
                     y = pct_point_change)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Top 10 Routes with Largest Increase in System Share",
    x = "Route ID",
    y = "Percentage Point Change")
```

![](Group-8-code_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(top_loss, aes(x = reorder(route_id, pct_point_change),
                     y = pct_point_change)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Top 10 Routes with Largest Decrease in System Share",
    x = "Route ID",
    y = "Percentage Point Change")
```

![](Group-8-code_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

## Recovery Rate Analysis

# Year-over-Year Growth Rate

``` r
growth = top10_loads_summary |> 
  group_by(day_group) |>
  arrange(year_from_name) |>
  mutate(yoy_change = (avg_load - lag(avg_load)) / lag(avg_load) * 100) |>
  ungroup()

ggplot(growth |> filter(!is.na(yoy_change)), 
       aes(x = year_from_name, y = yoy_change, color = day_group, group = day_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Year-over-Year Growth Rate of Top 10 Routes",
    x = "Year",
    y = "YoY Change (%)",
    color = "Day Type")
```

![](Group-8-code_files/figure-gfm/yoy-growth-1.png)<!-- -->

# Recovery Rate Comparison

``` r
recovery_analysis = top10_loads_summary |>
  filter(year_from_name %in% c(2019, 2020, 2024)) |>
  select(year_from_name, day_group, avg_load) |>
  pivot_wider(names_from = year_from_name, values_from = avg_load, names_prefix = "year_") |>
  mutate(covid_drop = (year_2020 - year_2019) / year_2019 * 100,
    recovery_rate = (year_2024 - year_2020) / (year_2019 - year_2020) * 100)
recovery_analysis
```

    ## # A tibble: 2 × 6
    ##   day_group year_2019 year_2020 year_2024 covid_drop recovery_rate
    ##   <chr>         <dbl>     <dbl>     <dbl>      <dbl>         <dbl>
    ## 1 Weekday        74.1      33.5      92.0      -54.8         144. 
    ## 2 Weekend        64.7      34.4      61.9      -46.9          90.9

# Route-level decline & recovery

``` r
route_year_summary = csv_mbta |>
  filter(year_from_name %in% c(2019, 2020, 2024)) |>
  group_by(route_id, day_group, year_from_name) |>
  summarise(mean_load = mean(load_, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = year_from_name, values_from = mean_load, names_prefix = "year_") |>
  drop_na(year_2019, year_2020, year_2024) |>
  mutate(
    drop_2019_2020 = (year_2020 - year_2019) / year_2019 * 100,
    rec_2020_2024 = (year_2024 - year_2020) / year_2019 * 100)

ggplot(route_year_summary, aes(x = day_group, y = drop_2019_2020, fill = day_group)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of 2019→2020 Ridership Drop by Route",
    x = "Day Type",
    y = "Percentage Change (%)")
```

![](Group-8-code_files/figure-gfm/route-recovery-1.png)<!-- -->

``` r
ggplot(route_year_summary, aes(x = day_group, y = rec_2020_2024, fill = day_group)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of 2020→2024 Recovery (relative to 2019) by Route",
    x = "Day Type",
    y = "Recovery Relative to 2019 (%)")
```

![](Group-8-code_files/figure-gfm/route-recovery-2.png)<!-- -->
