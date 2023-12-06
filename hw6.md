HW 6
================
Nhu Nguyen
2023-12-06

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.

``` r
library(dplyr)
library(knitr)

set.seed(1)
```

# Problem 1

# Problem 2

importing data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2022-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## using cached file: /Users/nhunguyen/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-10-04 20:49:04 (8.527)

    ## file min/max dates: 1869-01-01 / 2023-10-31

creating 5000 bootstrap sample

``` r
bootstrap_df = weather_df |> 
  modelr::bootstrap(n = 5000) |> 
  mutate(
    models = map(strap, \(df) lm(tmax ~ tmin + prcp, data = df)),
    results = map(models, broom::tidy),
    rsq = map(models, broom::glance)) |> 
  select(results, .id, rsq) |> 
  unnest(results) |> 
  filter(term %in% c("tmin", "prcp")) |> 
  group_by(.id) |> 
  mutate(beta1xbeta2 = prod(estimate),
         log_beta_product = log(beta1xbeta2)) |> 
  select(log_beta_product, rsq) |> 
  unnest(rsq) |> 
  janitor::clean_names() |> 
  select(log_beta_product, id, r_squared) |> 
  unique()
```

    ## Warning: There were 3361 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `log_beta_product = log(beta1xbeta2)`.
    ## ℹ In group 2: `.id = "0002"`.
    ## Caused by warning in `log()`:
    ## ! NaNs produced
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3360 remaining warnings.

    ## Adding missing grouping variables: `.id`

### plotting distribution of estimates

log of beta product

``` r
bootstrap_df |> 
  ggplot(aes(x = log_beta_product)) + geom_density()
```

    ## Warning: Removed 3361 rows containing non-finite values (`stat_density()`).

![](hw6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> This figure
displays the distribution of the estimates of the log of the product of
betas 1 and 2, which is left skewed and unimodal with a peak at around
-5.75.

r-squared

``` r
bootstrap_df |> 
  ggplot(aes(x = r_squared)) + geom_density()
```

![](hw6_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> This figure
displays the distribution of the estimates of r-squared values, which is
slightly left skewed (although almost normal) and unimodal with a peak
at around 0.916.

95% CI

``` r
bootstrap_conf = bootstrap_df |> 
  unique() |>
  ungroup() |> 
  select(-id) |> 
  summarize(beta_mean = mean(log_beta_product, na.rm = TRUE),
            beta_low = quantile(log_beta_product, 0.025, na.rm = TRUE),
            beta_high = quantile(log_beta_product, 0.975, na.rm = TRUE),
            rs_mean = mean(r_squared),
            rs_low = quantile(r_squared, 0.025),
            rs_high = quantile(r_squared, 0.975))

bootstrap_conf |> knitr::kable()
```

| beta_mean |  beta_low | beta_high |   rs_mean |    rs_low |   rs_high |
|----------:|----------:|----------:|----------:|----------:|----------:|
| -6.089813 | -8.981559 | -4.601673 | 0.9168349 | 0.8885495 | 0.9406812 |

The 95% confidence interval for the log of the beta product is
(-8.9815594, -4.6016727).

The 95% confidence interval for r-squared is (0.8885495, 0.9406812).
