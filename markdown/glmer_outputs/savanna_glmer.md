savanna_glmer
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
sav_df <- filter(final, indicator=='savanna')
```

\#number of indicator species

``` r
length(unique(sav_df$Species_Code))
```

    ## [1] 188

# add in year variable

``` r
sav_df$Year <- lubridate::year(sav_df$StartDate)
str(sav_df)
```

    ## spec_tbl_df [9,206,175 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:9206175] 63 63 63 63 63 63 63 63 63 63 ...
    ##  $ CardNo        : chr [1:9206175] "2025_2850_001868_20101204" "2025_3110_011088_20160827" "2105_3150_010470_20111105" "2120_3110_010330_20130419" ...
    ##  $ StartDate     : Date[1:9206175], format: "2010-12-04" "2016-08-27" ...
    ##  $ Pentad        : chr [1:9206175] "2505_2850" "2925_3110" "2505_3150" "2220_3110" ...
    ##  $ Spp           : chr [1:9206175] "-" "-" "-" "63" ...
    ##  $ Sequence      : chr [1:9206175] "-" "-" "-" "65" ...
    ##  $ Common_name   : chr [1:9206175] "-" "-" "-" "Heron, Striated" ...
    ##  $ Taxonomic_name: chr [1:9206175] "-" "-" "-" "Butorides striata" ...
    ##  $ Presence      : num [1:9206175] 0 0 0 1 0 1 0 0 0 0 ...
    ##  $ no_pres       : num [1:9206175] 9 4 125 79 97 25 57 4 4 4 ...
    ##  $ indicator     : chr [1:9206175] "savanna" "savanna" "savanna" "savanna" ...
    ##  $ Year          : num [1:9206175] 2010 2016 2011 2013 2013 ...
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

## glmer1 \<- glmer(Presence \~ Year\*Indicator_Type + (1 + Year\|Species_Code), family = ‘binomial’, data = sav_df)

# measure time taken…

``` r
start_time <- Sys.time()
```

# run a mixed effects model; year = fixed; species = random

``` r
sav_glmer <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = sav_df)
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 2.22661 hours

# Summary of model

``` r
summary(sav_glmer)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ scale(Year) + (scale(Year) | Species_Code) + (1 |  
    ##     Pentad)
    ##    Data: sav_df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  7451626  7451711 -3725807  7451614  9206169 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5334 -0.4870 -0.3046 -0.1367 29.2949 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev. Corr 
    ##  Pentad       (Intercept) 0.83338  0.9129        
    ##  Species_Code (Intercept) 1.43379  1.1974        
    ##               scale(Year) 0.01773  0.1332   -0.06
    ## Number of obs: 9206175, groups:  Pentad, 1820; Species_Code, 188
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.495921   0.034639 -72.055   <2e-16 ***
    ## scale(Year)  0.018823   0.009927   1.896   0.0579 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scale(Year) -0.078

# Anova

``` r
Anova(sav_glmer)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Presence
    ##              Chisq Df Pr(>Chisq)  
    ## scale(Year) 3.5955  1    0.05793 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Plot effects

``` r
plot(allEffects(sav_glmer))
```

    ## Warning in Analyze.model(focal.predictors, mod, xlevels, default.levels, : the
    ## predictor scale(Year) is a one-column matrix that was converted to a vector

![](savanna_glmer_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#predict

``` r
sav_df$predict <- predict(sav_glmer, sav_df)

savanna_clean <- sav_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))
```

    ## `summarise()` has grouped output by 'Species_Code'. You can override using the
    ## `.groups` argument.

``` r
ggplot(data=savanna_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))
```

![](savanna_glmer_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
