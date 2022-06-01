forest_glmer
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
forest_df <- filter(final, indicator=='forest')
```

\#number of indicator species

``` r
length(unique(forest_df$Species_Code))
```

    ## [1] 26

# add in year variable

``` r
forest_df$Year <- lubridate::year(forest_df$StartDate)
str(forest_df)
```

    ## spec_tbl_df [719,695 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:719695] 147 147 147 147 147 147 147 147 147 147 ...
    ##  $ CardNo        : chr [1:719695] "2645_3215_010821_20090503" "2650_3210_000039_20110607" "2650_3210_000055_20080212" "2650_3210_000109_20130816" ...
    ##  $ StartDate     : Date[1:719695], format: "2009-05-03" "2011-06-07" ...
    ##  $ Pentad        : chr [1:719695] "2650_3210" "2650_3210" "2650_3210" "2650_3210" ...
    ##  $ Spp           : chr [1:719695] "-" "-" "-" "-" ...
    ##  $ Sequence      : chr [1:719695] "-" "-" "-" "-" ...
    ##  $ Common_name   : chr [1:719695] "-" "-" "-" "-" ...
    ##  $ Taxonomic_name: chr [1:719695] "-" "-" "-" "-" ...
    ##  $ Presence      : num [1:719695] 0 0 0 0 0 1 0 0 0 1 ...
    ##  $ no_pres       : num [1:719695] 15 15 15 15 15 15 15 15 15 15 ...
    ##  $ indicator     : chr [1:719695] "forest" "forest" "forest" "forest" ...
    ##  $ Year          : num [1:719695] 2009 2011 2008 2013 2014 ...
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

## glmer1 \<- glmer(Presence \~ Year\*Indicator_Type + (1 + Year\|Species_Code), family = ‘binomial’, data = forest_df)

# measure time taken…

``` r
start_time <- Sys.time()
```

# run a mixed effects model; year = fixed; species = random

``` r
forest_glmer <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = forest_df)
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 11.43894 mins

# Summary of model

``` r
summary(forest_glmer)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ scale(Year) + (scale(Year) | Species_Code) + (1 |  
    ##     Pentad)
    ##    Data: forest_df
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ##  694984.9  695053.8 -347486.4  694972.9    719689 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.4663 -0.5803 -0.3276  0.5921 27.9256 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev. Corr
    ##  Pentad       (Intercept) 1.59808  1.2642       
    ##  Species_Code (Intercept) 1.29261  1.1369       
    ##               scale(Year) 0.01291  0.1136   0.02
    ## Number of obs: 719695, groups:  Pentad, 1047; Species_Code, 26
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.340765   0.146685 -15.958   <2e-16 ***
    ## scale(Year) -0.001886   0.024025  -0.078    0.937    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scale(Year) 0.015

# Anova

``` r
Anova(forest_glmer)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Presence
    ##              Chisq Df Pr(>Chisq)
    ## scale(Year) 0.0062  1     0.9374

# Plot effects

``` r
plot(allEffects(forest_glmer))
```

    ## Warning in Analyze.model(focal.predictors, mod, xlevels, default.levels, : the
    ## predictor scale(Year) is a one-column matrix that was converted to a vector

![](forest_glmer_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#predict

``` r
forest_df$predict <- predict(forest_glmer, forest_df)

forest_clean <- forest_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))
```

    ## `summarise()` has grouped output by 'Species_Code'. You can override using the
    ## `.groups` argument.

``` r
ggplot(data=forest_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))
```

![](forest_glmer_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
