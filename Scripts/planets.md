---
title: "Mock Data Viz Markdown file"
author: "Prasann Ranade"
date: "04/19/2023"
output:
  html_document:
    keep_md: yes
---



# Test Markdown Document to display visual alongside script 

## Read in data


```r
df <- si_path() %>% 
  return_latest("FY48-49") %>% 
  read_csv()  
```

```
## ℹ Latest file in 'Data' matching 'FY48-49': 'FY48-49_MilkyWay_Cascade_data (1).csv'
## Rows: 96 Columns: 10
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (5): planet, region, indicator, indicator_description, disaggregate
## dbl (5): fiscal_year, qtr1, qtr2, qtr3, qtr4
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
get_metadata()
```

```
## ℹ Latest file in 'Data' matching 'OU_IM_FY21': 'MER_Structured_Datasets_OU_IM_FY21-23_20230210_v1_1.txt'
## ℹ metadata is now stored as a global object and metadata items can be accessed via `metadata$...`
```
## Filter data


```r
df_tx <- df %>%
  filter(planet == "Saturn",
         disaggregate == "Total Numerator") %>% 
  group_by(planet, region, indicator, fiscal_year) %>% 
  summarise(across(c("qtr1","qtr2","qtr3","qtr4"), sum, na.rm = TRUE)) %>%
  ungroup()
```

```
## Warning: There was 1 warning in `summarise()`.
## ℹ In argument: `across(c("qtr1", "qtr2", "qtr3", "qtr4"), sum, na.rm = TRUE)`.
## ℹ In group 1: `planet = "Saturn"`, `region = "Dione"`, `indicator = "HTS_TST"`,
##   `fiscal_year = 2048`.
## Caused by warning:
## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
## Supply arguments directly to `.fns` through an anonymous function instead.
## 
##   # Previously
##   across(a:b, mean, na.rm = TRUE)
## 
##   # Now
##   across(a:b, \(x) mean(x, na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'planet', 'region', 'indicator'. You can
## override using the `.groups` argument.
```

## Visualize data


```r
df_viz <- ggplot(df_tx) +
  geom_col(data = df_tx, aes(x = indicator, y = qtr1, fill = indicator)) +
  geom_col(data = df_tx, aes(x = indicator, y = qtr2, fill = indicator)) +
  si_style_xgrid()

plot(df_viz)
```

![](planets_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#save plot
si_save("Images/md_plot.png")
#si_save("Graphics/md_plot.svg")
```

This is a description of the plot as a sample write-up in markdown.



