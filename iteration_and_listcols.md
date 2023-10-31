iteration_and_listcols
================
Xiaoyu Wu
2023-10-27

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(1)
```

## List

You can put anything in the list

``` r
vec_numeric = 5:8
vec_char = c("My", "name", "is", "Jeff")
vec_logical = c(TRUE, TRUE, TRUE, FALSE)
mat= matrix(1:8, nrow=2, ncol=4)
# matrix contains number 1 to 8 
summary = summary(rnorm(1000))
```

store everything in the list

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.25322 -0.68967 -0.03448 -0.01626  0.73734  3.63957

``` r
# list can have different vectors with different lengths
```

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
# access individual elements in the list 
l[[1]][1:3]
```

    ## [1] 5 6 7

``` r
# access the 1st and 3rd elements in the first vector of the list 
```

## `for` loops

new list

``` r
list_norms = 
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(20, 0, 5),
    c = rnorm(20, 10, .2),
    d = rnorm(20, -3, 1)
  )
list_norms
```

    ## $a
    ##  [1] 2.1138504 1.0777451 4.6197007 3.5192699 2.9441501 3.6964176 3.0535157
    ##  [8] 1.6897165 0.8769339 2.7919214 2.6872134 1.9417643 3.4172236 2.6845485
    ## [15] 3.8255491 4.2912720 2.3748950 2.1248532 3.1433569 4.4251896
    ## 
    ## $b
    ##  [1] -8.6737471 -4.1870232 -3.5787631  1.8741048 11.9166398 -2.3195648
    ##  [7] -2.1325020 -8.5154761 -5.2074868  3.8436748 -3.7266019 -2.4323575
    ## [13] -1.7651972 -0.3115026  8.0980755  6.2310570 10.0429498 -9.2720976
    ## [19] -2.3135719  5.2155192
    ## 
    ## $c
    ##  [1]  9.939232 10.063616  9.870250  9.626219  9.758601 10.379452  9.947683
    ##  [8] 10.357596 10.173460 10.140516 10.324706  9.681968  9.878002  9.937344
    ## [15]  9.815574  9.948704  9.583429  9.716569 10.122156 10.048304
    ## 
    ## $d
    ##  [1] -1.696035 -3.334529 -3.384540 -3.421168 -3.958355 -2.750884 -4.178879
    ##  [8] -2.504407 -3.177704 -2.225266 -1.752054 -3.416217 -3.429718 -3.046525
    ## [15] -3.658592 -1.397416 -3.063672 -2.131172 -2.621625 -1.584312

``` r
is.list(list_norms)
```

    ## [1] TRUE

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

apply simple mean_and_sd function to list_norm

``` r
mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.86  1.05

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.361  6.15

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.236

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84 0.817

Let’s use a for loop:

``` r
output_mean_and_sd = vector("list", length = 4)
output_median = vector("list", length = 4)
# define two output lists with the same number of entries as my target dataframe
for (i in 1:4) {
  output_mean_and_sd[[i]] = mean_and_sd(list_norms[[i]])
  output_median[[i]] = median(list_norms[[i]])
}
# input is list_norms 
output_mean_and_sd
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.86  1.05
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.361  6.15
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.236
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84 0.817

``` r
output_median
```

    ## [[1]]
    ## [1] 2.868036
    ## 
    ## [[2]]
    ## [1] -2.223037
    ## 
    ## [[3]]
    ## [1] 9.943458
    ## 
    ## [[4]]
    ## [1] -3.055099

## Let’s try map

``` r
output = map(list_norms, mean_and_sd)
# The first argument to map is the list we want to iterate over, and the second argument is the function we want to apply to each element.
output = map(.x = list_norms, ~ mean_and_sd(.x))
# be more specific in giving arguments to map
```

## map variants

``` r
output = map_dbl(list_norms, median, .id = "input")
# Using the .id argument keeps the names of the elements in the input list.
# put the names of each list into a new variable called "input"
```

``` r
output = map_dfr(list_norms, mean_and_sd, .id = "input")
# this will produce a single data frame.
# map_int or map_lgl, integer or a logical.
```

``` r
# output = map2(input_1, input_2, \(x,y) func(arg_1 = x, arg_2 = y))
```

## List columns and operations

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norms
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 2.1138504 1.0777451 4.6197007 3.5192699 2.9441501 3.6964176 3.0535157
    ##  [8] 1.6897165 0.8769339 2.7919214 2.6872134 1.9417643 3.4172236 2.6845485
    ## [15] 3.8255491 4.2912720 2.3748950 2.1248532 3.1433569 4.4251896
    ## 
    ## $b
    ##  [1] -8.6737471 -4.1870232 -3.5787631  1.8741048 11.9166398 -2.3195648
    ##  [7] -2.1325020 -8.5154761 -5.2074868  3.8436748 -3.7266019 -2.4323575
    ## [13] -1.7651972 -0.3115026  8.0980755  6.2310570 10.0429498 -9.2720976
    ## [19] -2.3135719  5.2155192
    ## 
    ## $c
    ##  [1]  9.939232 10.063616  9.870250  9.626219  9.758601 10.379452  9.947683
    ##  [8] 10.357596 10.173460 10.140516 10.324706  9.681968  9.878002  9.937344
    ## [15]  9.815574  9.948704  9.583429  9.716569 10.122156 10.048304
    ## 
    ## $d
    ##  [1] -1.696035 -3.334529 -3.384540 -3.421168 -3.958355 -2.750884 -4.178879
    ##  [8] -2.504407 -3.177704 -2.225266 -1.752054 -3.416217 -3.429718 -3.046525
    ## [15] -3.658592 -1.397416 -3.063672 -2.131172 -2.621625 -1.584312

``` r
listcol_df$samp[[1]]
```

    ##  [1] 2.1138504 1.0777451 4.6197007 3.5192699 2.9441501 3.6964176 3.0535157
    ##  [8] 1.6897165 0.8769339 2.7919214 2.6872134 1.9417643 3.4172236 2.6845485
    ## [15] 3.8255491 4.2912720 2.3748950 2.1248532 3.1433569 4.4251896

``` r
listcol_df |> 
  filter(name=="a")
```

    ## # A tibble: 1 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

apply mean_and_sd to the first element of our list column

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.86  1.05

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.86  1.05
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.361  6.15
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.236
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84 0.817

store the results as a new list column

``` r
listcol_df = 
  listcol_df %>% 
  mutate(summary = map(samp, mean_and_sd))  %>% 
  select(name,summary) %>%
  unnest(summary)
listcol_df
```

    ## # A tibble: 4 × 3
    ##   name    mean    sd
    ##   <chr>  <dbl> <dbl>
    ## 1 a      2.86  1.05 
    ## 2 b     -0.361 6.15 
    ## 3 c      9.97  0.236
    ## 4 d     -2.84  0.817

## Revisiting NSDUH

``` r
nsduh_table <- function(html, table_num) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
}
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

output = vector("list", 3)

for (i in c(1, 4, 5)) {
  output[[i]] = nsduh_table(nsduh_html, i)
}

nsduh_results = bind_rows(output)
```

``` r
nsduh_results = 
  map(c(1, 4, 5), nsduh_table, html = nsduh_html) |> 
# I’m supplying the html argument after the name of the function that I’m iterating over.
  bind_rows()
```

``` r
nsduh_results= 
  tibble(
    name = c("marj", "cocaine", "heroine"),
    number = c(1, 4, 5)) |> 
  mutate(table = map(number, \(num) nsduh_table(html = nsduh_html, num))) |> 
  unnest(cols = "table")
```

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/wuxiaoyu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2023-09-27 13:57:30.794807 (8.524)

    ## file min/max dates: 1869-01-01 / 2023-09-30

    ## using cached file: /Users/wuxiaoyu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2023-09-29 11:56:17.087775 (1.707)

    ## file min/max dates: 1965-01-01 / 2023-09-30

    ## using cached file: /Users/wuxiaoyu/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2023-09-27 13:57:51.612726 (0.994)

    ## file min/max dates: 1999-09-01 / 2023-09-30

Weather data will be separated into three station-specific data frames

``` r
weather_nest = 
  nest(weather_df, data = date:tmin)

weather_nest
```

    ## # A tibble: 3 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]>
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]>
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]>

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # ℹ 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # ℹ 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # ℹ 355 more rows

unnesting

``` r
unnest(weather_nest, cols = data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # ℹ 1,085 more rows

fit the simple linear regression relating tmax to tmin

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}
```

apply our weather_lm function to each data frame using map

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
map(weather_nest$data, ~lm(tmax ~ tmin, data = .x))
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
# avoid the creation of a dedicated function
```

using a for loop:

``` r
output=vector("list",3)

for (i in 1:3) {
  output[[i]]=weather_lm(weather_nest$data[[i]])
}
output
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

mutate to fit this model, and to store the result in the same dataframe
What about a amap in a list column!!!!!??

``` r
weather_nest = 
  weather_nest %>% 
  mutate(models = map(data, weather_lm))

weather_nest
```

    ## # A tibble: 3 × 4
    ##   name           id          data               models
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>
