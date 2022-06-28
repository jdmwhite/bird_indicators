random effects test 2
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
glmer_fyn <- glmer(Presence ~ scale(Year) + (0 + scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = fyn_df)
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 29.12955 secs

\#summary

``` r
summary(glmer_fyn)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ scale(Year) + (0 + scale(Year) | Species_Code) + (1 |  
    ##     Pentad)
    ##    Data: fyn_df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 137911.0 137950.2 -68951.5 137903.0   131422 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2669 -0.6407 -0.4112  0.8050 11.0559 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev.
    ##  Pentad       (Intercept) 1.39555  1.1813  
    ##  Species_Code scale(Year) 0.01016  0.1008  
    ## Number of obs: 131426, groups:  Pentad, 470; Species_Code, 7
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.65318    0.05612 -11.638   <2e-16 ***
    ## scale(Year) -0.01037    0.03927  -0.264    0.792    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scale(Year) 0.001

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

    ##   Species_Code  Intercept        Slope
    ## 1          181 -0.6531841  0.021870849
    ## 2          540 -0.6531841  0.132049736
    ## 3          612 -0.6531841 -0.071085173
    ## 4          749 -0.6531841 -0.073078037
    ## 5          753 -0.6531841 -0.172493864
    ## 6          855 -0.6531841 -0.007542083
    ## 7          869 -0.6531841  0.097824705

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
  scale_y_continuous(limits = c(-1, 0)) +
  theme(legend.position = "right")
```

# see the plot

``` r
model_coef_plot
```

![](random-effects-test-same-intercept_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
