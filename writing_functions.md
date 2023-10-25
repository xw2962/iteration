writing_functions
================
Xiaoyu Wu
2023-10-25

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

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)
# 30 vectors taking the mean of 5 and sd of 3 
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.767120446  0.109493425 -0.993470503  1.637039863  0.267334741
    ##  [6] -0.977065475  0.438222871  0.709719461  0.533829741 -0.419692475
    ## [11]  1.546684110  0.332624325 -0.761479160 -2.485776741  1.128069748
    ## [16] -0.137851865 -0.106748415  0.932105430  0.799422547  0.553437533
    ## [21]  0.905205442  0.757128408 -0.008541293 -2.241925304  0.581490604
    ## [26] -0.149966223 -0.257816586 -1.680744021 -0.606639531  0.363029790

``` r
# z-scores: subtracted by the mean and divided by sd
```

I wanta function to compute z-scores

``` r
z_scores = function(x) {
# argument is x and inside the brackets 
# input is x everywhere 
  z = (x - mean(x)) / sd(x)
# body and operation 
  return(z)
# return objects   
}

z_scores(x_vec)
```

    ##  [1] -0.767120446  0.109493425 -0.993470503  1.637039863  0.267334741
    ##  [6] -0.977065475  0.438222871  0.709719461  0.533829741 -0.419692475
    ## [11]  1.546684110  0.332624325 -0.761479160 -2.485776741  1.128069748
    ## [16] -0.137851865 -0.106748415  0.932105430  0.799422547  0.553437533
    ## [21]  0.905205442  0.757128408 -0.008541293 -2.241925304  0.581490604
    ## [26] -0.149966223 -0.257816586 -1.680744021 -0.606639531  0.363029790

Try my functions on some other thing

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
# update teh function 
  z = mean(x) / sd(x)
  
  z
}
# add some checks 
```

## Multiple outputs

store each of the values in a named list

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
mean_and_sd(x_vec)
```

    ## $mean
    ## [1] 5.247375
    ## 
    ## $sd
    ## [1] 2.772362

store values in a data frame

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
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.25  2.77

## Multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.40      2.39

I’d like to do this in a function

``` r
sim_mean_sd = function(sample_size, mu, sigma) {
# true population mean mu and true population sd sigma.
# (sample_size, mu=3, sigma=4) default values 
  sim_data = tibble(
    x = rnorm(n=sample_size, mean = mu, sd = sigma),
  )
# inputs  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
# return objects 
}
sim_mean_sd(sample_size=100,mu=6,sigma=3)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   5.83      2.85

## Revisiting past examples

Scraping Amazon

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

html = read_html(url)

review_profile = 
    html %>%
    html_nodes(".a-profile-content") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes(".review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_date = 
    html %>%
    html_nodes(".review-date") %>%
    html_text() 

reviews=
    tibble(
    profile = review_profile,
    stars = review_stars,
    date = review_date
)
reviews
```

    ## # A tibble: 12 × 3
    ##    profile              stars date                                              
    ##    <chr>                <dbl> <chr>                                             
    ##  1 BuyandReviewNoProfit     5 Reviewed in the United States on October 22, 2023 
    ##  2 Bookgirl                 3 Reviewed in the United States on August 17, 2023  
    ##  3 BuyandReviewNoProfit     5 Reviewed in the United States on October 22, 2023 
    ##  4 Adam Rickett             4 Reviewed in the United States on October 17, 2023 
    ##  5 Kevin Smith              5 Reviewed in the United States on October 11, 2023 
    ##  6 Stephanie                5 Reviewed in the United States on October 7, 2023  
    ##  7 Julia Mori               5 Reviewed in the United States on October 5, 2023  
    ##  8 Placeholder              5 Reviewed in the United States on October 3, 2023  
    ##  9 Bowdrie Dockstader       5 Reviewed in the United States on October 1, 2023  
    ## 10 Rafael Cuevas            1 Reviewed in the United States on September 29, 20…
    ## 11 Amazon Customer          5 Reviewed in the United States on September 29, 20…
    ## 12 Bret Caffacus            5 Reviewed in the United States on September 28, 20…

Let’s turn that code into a function

``` r
read_page_reviews = function(url) {
# input is the url 
  html = read_html(url)
  
  review_profile = 
    html %>%
    html_nodes(".a-profile-content") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes(".review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_date = 
    html %>%
    html_nodes(".review-date") %>%
    html_text() 
  
  reviews=
    tibble(
    profile = review_profile,
    stars = review_stars,
    date = review_date
  )
  # body and operations   
  reviews
# return objects 
}

dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
read_page_reviews(dynamite_url)
```

    ## # A tibble: 12 × 3
    ##    profile              stars date                                              
    ##    <chr>                <dbl> <chr>                                             
    ##  1 BuyandReviewNoProfit     5 Reviewed in the United States on October 22, 2023 
    ##  2 Bookgirl                 3 Reviewed in the United States on August 17, 2023  
    ##  3 BuyandReviewNoProfit     5 Reviewed in the United States on October 22, 2023 
    ##  4 Adam Rickett             4 Reviewed in the United States on October 17, 2023 
    ##  5 Kevin Smith              5 Reviewed in the United States on October 11, 2023 
    ##  6 Stephanie                5 Reviewed in the United States on October 7, 2023  
    ##  7 Julia Mori               5 Reviewed in the United States on October 5, 2023  
    ##  8 Placeholder              5 Reviewed in the United States on October 3, 2023  
    ##  9 Bowdrie Dockstader       5 Reviewed in the United States on October 1, 2023  
    ## 10 Rafael Cuevas            1 Reviewed in the United States on September 29, 20…
    ## 11 Amazon Customer          5 Reviewed in the United States on September 29, 20…
    ## 12 Bret Caffacus            5 Reviewed in the United States on September 28, 20…

read in reviews from a few pages and combine the results

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

``` r
# y is the needed value for x 
```

## Functions as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}
mean(x_vec)
```

    ## [1] 0.3460476

``` r
median(x_vec)
```

    ## [1] 0.2199248

``` r
my_summary(x_vec, mean)
```

    ## [1] 0.3460476

``` r
# reorder summ_func according to the mean 
```
