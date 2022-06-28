glmer_fynbos
================
Christopher Shortland
2022-06-28

# Libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

\#importing final dataset

``` r
final <- read_csv('output/final_df/final_df.csv')
```

    ## Rows: 17481700 Columns: 11
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (7): CardNo, Pentad, Spp, Sequence, Common_name, Taxonomic_name, indicator
    ## dbl  (3): Species_Code, Presence, no_pres
    ## date (1): StartDate
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Load fynbos stacked df

``` r
fyn_df <- filter(final, indicator=='fynbos')
```

\#number of indicator species

``` r
fyn_df %>% filter(!Species_Code %in% c('285',"901", "870", "985", "567", "980", "289", "51")) -> fyn_df
```

# add in year variable

``` r
fyn_df$Year <- lubridate::year(fyn_df$StartDate)
str(fyn_df)
```

    ## spec_tbl_df [131,426 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:131426] 181 181 181 181 181 181 181 181 181 181 ...
    ##  $ CardNo        : chr [1:131426] "1800-3300_001449_20150411" "1820_3325_001393_20081019" "1820_3325_002076_20090421" "1845_3415_011138_20090904" ...
    ##  $ StartDate     : Date[1:131426], format: "2015-04-11" "2008-10-19" ...
    ##  $ Pentad        : chr [1:131426] "3300_1800" "3325_1820" "3325_1820" "3415_1845" ...
    ##  $ Spp           : chr [1:131426] "181" "181" "181" "181" ...
    ##  $ Sequence      : chr [1:131426] "5" "12" "42" "10" ...
    ##  $ Common_name   : chr [1:131426] "Spurfowl, Cape" "Spurfowl, Cape" "Spurfowl, Cape" "Spurfowl, Cape" ...
    ##  $ Taxonomic_name: chr [1:131426] "Pternistis capensis" "Pternistis capensis" "Pternistis capensis" "Pternistis capensis" ...
    ##  $ Presence      : num [1:131426] 1 1 1 1 1 0 1 1 1 0 ...
    ##  $ no_pres       : num [1:131426] 116 82 82 137 137 29 282 282 282 14 ...
    ##  $ indicator     : chr [1:131426] "fynbos" "fynbos" "fynbos" "fynbos" ...
    ##  $ Year          : num [1:131426] 2015 2008 2009 2009 2009 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   Species_Code = col_double(),
    ##   ..   CardNo = col_character(),
    ##   ..   StartDate = col_date(format = ""),
    ##   ..   Pentad = col_character(),
    ##   ..   Spp = col_character(),
    ##   ..   Sequence = col_character(),
    ##   ..   Common_name = col_character(),
    ##   ..   Taxonomic_name = col_character(),
    ##   ..   Presence = col_double(),
    ##   ..   no_pres = col_double(),
    ##   ..   indicator = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

# fit random intercepts model

``` r
# fit random intercepts model
start_time <- Sys.time()
glmer_fyn <- glmer(Presence ~ scale(Year) + (1 + scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = fyn_df)
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 1.178539 mins

\#summary

``` r
summary(glmer_fyn)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ scale(Year) + (1 + scale(Year) | Species_Code) + (1 |  
    ##     Pentad)
    ##    Data: fyn_df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 122752.4 122811.1 -61370.2 122740.4   131420 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1649 -0.5310 -0.2776  0.5836 14.8755 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev. Corr 
    ##  Pentad       (Intercept) 1.32128  1.149         
    ##  Species_Code (Intercept) 1.18582  1.089         
    ##               scale(Year) 0.01717  0.131    -0.37
    ## Number of obs: 131426, groups:  Pentad, 470; Species_Code, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.82601    0.38741  -4.713 2.44e-06 ***
    ## scale(Year) -0.01901    0.05093  -0.373    0.709    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scale(Year) -0.332

\##Model coefficients

``` r
model_coefs <- coef(glmer_fyn)$Species_Code %>% 
  rename(Intercept = `(Intercept)`, Slope = `scale(Year)`) %>% 
  rownames_to_column("Species_Code")
```

# see coefficients

``` r
model_coefs
```

    ##   Species_Code  Intercept       Slope
    ## 1          181  0.1963681  0.02501632
    ## 2          540 -2.9901412  0.20280955
    ## 3          612 -1.8959490 -0.15427779
    ## 4          749 -1.0097175 -0.05530743
    ## 5          753 -1.5946559 -0.15796590
    ## 6          855 -2.2024358 -0.10003029
    ## 7          869 -3.2092897  0.10669459

\#joining stuff

``` r
fyn_df$Species_Code <- as.factor(fyn_df$Species_Code)
testjoin <- left_join(fyn_df, model_coefs, by = "Species_Code")
```

\#plotting

``` r
model_coef_plot <- ggplot(data = testjoin, 
                          mapping = aes(x = Year, 
                                        y = Presence, 
                                        colour = Species_Code)) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope,
                  colour = Species_Code),
      size = 1) +
  scale_y_continuous(limits = c(-4, 1)) +
  theme(legend.position = "right")
```

# see the plot

``` r
model_coef_plot
```

![](random-effects-test_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
