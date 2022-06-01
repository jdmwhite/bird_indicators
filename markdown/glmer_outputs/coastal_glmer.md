coastal_GLMER
================
Christopher Shortland
2022-05-24

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

``` r
library(car)
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(effects)
```

    ## lattice theme set by effectsTheme()
    ## See ?effectsTheme for details.

``` r
library(ggplot2)
library(beepr)
```

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
coast_df <- filter(final, indicator=='coastal')
```

\#number of indicator species

``` r
length(unique(coast_df$Species_Code))
```

    ## [1] 6

# add in year variable

``` r
coast_df$Year <- lubridate::year(coast_df$StartDate)
str(coast_df)
```

    ## spec_tbl_df [154,614 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:154614] 49 49 49 49 49 49 49 49 49 49 ...
    ##  $ CardNo        : chr [1:154614] "1800-3300_001449_20150411" "1845_3415_011138_20090904" "1845_3415_011138_20090908" "1855_3420_011138_20090831" ...
    ##  $ StartDate     : Date[1:154614], format: "2015-04-11" "2009-09-04" ...
    ##  $ Pentad        : chr [1:154614] "3300_1800" "3415_1845" "3415_1845" "3420_1855" ...
    ##  $ Spp           : chr [1:154614] "-" "-" "-" "-" ...
    ##  $ Sequence      : chr [1:154614] "-" "-" "-" "-" ...
    ##  $ Common_name   : chr [1:154614] "-" "-" "-" "-" ...
    ##  $ Taxonomic_name: chr [1:154614] "-" "-" "-" "-" ...
    ##  $ Presence      : num [1:154614] 0 0 0 0 0 0 0 0 0 1 ...
    ##  $ no_pres       : num [1:154614] 33 7 7 73 73 73 6 2 7 125 ...
    ##  $ indicator     : chr [1:154614] "coastal" "coastal" "coastal" "coastal" ...
    ##  $ Year          : num [1:154614] 2015 2009 2009 2009 2009 ...
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

# Option to run with Indicator Type as a fixed effect

## glmer1 \<- glmer(Presence \~ Year\*Indicator_Type + (1 + Year\|Species_Code), family = ‘binomial’, data = coast_df)

# measure time taken…

``` r
start_time <- Sys.time()
```

# run a mixed effects model; year = fixed; species = random

``` r
coast_glmer <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = coast_df)
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 2.084024 mins

# Summary of model

``` r
summary(coast_glmer)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ scale(Year) + (scale(Year) | Species_Code) + (1 |  
    ##     Pentad)
    ##    Data: coast_df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 128979.7 129039.4 -64483.9 128967.7   154608 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3263 -0.4999 -0.2527 -0.0644 19.8903 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev. Corr
    ##  Pentad       (Intercept) 1.92395  1.3871       
    ##  Species_Code (Intercept) 1.30725  1.1434       
    ##               scale(Year) 0.01852  0.1361   0.35
    ## Number of obs: 154614, groups:  Pentad, 254; Species_Code, 6
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.19974    0.43307  -5.079 3.79e-07 ***
    ## scale(Year)  0.01736    0.05563   0.312    0.755    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scale(Year) 0.306

# Anova

``` r
Anova(coast_glmer)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Presence
    ##              Chisq Df Pr(>Chisq)
    ## scale(Year) 0.0974  1      0.755

# Plot effects

``` r
plot(allEffects(coast_glmer))
```

    ## Warning in Analyze.model(focal.predictors, mod, xlevels, default.levels, : the
    ## predictor scale(Year) is a one-column matrix that was converted to a vector

![](coastal_glmer_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#predict

``` r
coast_df$predict <- predict(coast_glmer, coast_df)

coast_clean <- coast_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))
```

    ## `summarise()` has grouped output by 'Species_Code'. You can override using the
    ## `.groups` argument.

``` r
ggplot(data=coast_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))
```

![](coastal_glmer_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
