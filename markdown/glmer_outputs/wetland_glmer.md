wetland_glmer
================
Christopher Shortland
2022-05-25

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
wet_df <- filter(final, indicator=='wetland')
```

\#number of indicator species

``` r
length(unique(wet_df$Species_Code))
```

    ## [1] 65

# add in year variable

``` r
wet_df$Year <- lubridate::year(wet_df$StartDate)
str(wet_df)
```

    ## spec_tbl_df [5,661,096 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:5661096] 57 57 57 57 57 57 57 57 57 57 ...
    ##  $ CardNo        : chr [1:5661096] "1845_3415_011138_20090904" "1845_3415_011138_20090908" "1855_3355_000866_20110527" "1855_3420_011138_20090831" ...
    ##  $ StartDate     : Date[1:5661096], format: "2009-09-04" "2009-09-08" ...
    ##  $ Pentad        : chr [1:5661096] "3415_1845" "3415_1845" "3355_1855" "3420_1855" ...
    ##  $ Spp           : chr [1:5661096] "-" "-" "-" "-" ...
    ##  $ Sequence      : chr [1:5661096] "-" "-" "-" "-" ...
    ##  $ Common_name   : chr [1:5661096] "-" "-" "-" "-" ...
    ##  $ Taxonomic_name: chr [1:5661096] "-" "-" "-" "-" ...
    ##  $ Presence      : num [1:5661096] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ no_pres       : num [1:5661096] 2 2 6 9 9 9 6 3 3 3 ...
    ##  $ indicator     : chr [1:5661096] "wetland" "wetland" "wetland" "wetland" ...
    ##  $ Year          : num [1:5661096] 2009 2009 2011 2009 2009 ...
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

## glmer1 \<- glmer(Presence \~ Year\*Indicator_Type + (1 + Year\|Species_Code), family = ‘binomial’, data = wet_df)

# measure time taken…

``` r
start_time <- Sys.time()
```

# run a mixed effects model; year = fixed; species = random

``` r
wet_glmer <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = wet_df)
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 1.253522 hours

# Summary of model

``` r
summary(wet_glmer)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ scale(Year) + (scale(Year) | Species_Code) + (1 |  
    ##     Pentad)
    ##    Data: wet_df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4803094  4803175 -2401541  4803082  5661090 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.9053 -0.5020 -0.3274 -0.1581 30.7942 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev. Corr 
    ##  Pentad       (Intercept) 0.61157  0.7820        
    ##  Species_Code (Intercept) 1.20735  1.0988        
    ##               scale(Year) 0.02632  0.1622   -0.40
    ## Number of obs: 5661096, groups:  Pentad, 2133; Species_Code, 65
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.35614    0.04305 -54.725   <2e-16 ***
    ## scale(Year)  0.03538    0.01844   1.919    0.055 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scale(Year) -0.139

# Anova

``` r
Anova(wet_glmer)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Presence
    ##              Chisq Df Pr(>Chisq)  
    ## scale(Year) 3.6833  1    0.05496 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Plot effects

``` r
plot(allEffects(wet_glmer))
```

    ## Warning in Analyze.model(focal.predictors, mod, xlevels, default.levels, : the
    ## predictor scale(Year) is a one-column matrix that was converted to a vector

![](wetland_glmer_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#predict

``` r
wet_df$predict <- predict(wet_glmer, wet_df)

wetland_clean <- wet_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))
```

    ## `summarise()` has grouped output by 'Species_Code'. You can override using the
    ## `.groups` argument.

``` r
ggplot(data=wetland_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))
```

![](wetland_glmer_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
