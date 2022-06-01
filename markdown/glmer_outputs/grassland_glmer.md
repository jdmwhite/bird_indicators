grassland_glmer
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
grass_df <- filter(final, indicator=='grassland')
```

\#number of indicator species

``` r
length(unique(grass_df$Species_Code))
```

    ## [1] 39

# add in year variable

``` r
grass_df$Year <- lubridate::year(grass_df$StartDate)
str(grass_df)
```

    ## spec_tbl_df [1,545,618 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:1545618] 82 82 82 82 82 82 82 82 82 82 ...
    ##  $ CardNo        : chr [1:1545618] "2025_3110_011088_20160827" "2040_2930_001588_20100318" "2140_3100_002148_20091026" "2255_2755_010610_20090125" ...
    ##  $ StartDate     : Date[1:1545618], format: "2016-08-27" "2010-03-18" ...
    ##  $ Pentad        : chr [1:1545618] "2925_3110" "2940_2930" "2940_3100" "2555_2755" ...
    ##  $ Spp           : chr [1:1545618] "-" "-" "-" "-" ...
    ##  $ Sequence      : chr [1:1545618] "-" "-" "-" "-" ...
    ##  $ Common_name   : chr [1:1545618] "-" "-" "-" "-" ...
    ##  $ Taxonomic_name: chr [1:1545618] "-" "-" "-" "-" ...
    ##  $ Presence      : num [1:1545618] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ no_pres       : num [1:1545618] 5 87 15 2 2 2 2 2 2 2 ...
    ##  $ indicator     : chr [1:1545618] "grassland" "grassland" "grassland" "grassland" ...
    ##  $ Year          : num [1:1545618] 2016 2010 2009 2009 2016 ...
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

## glmer1 \<- glmer(Presence \~ Year\*Indicator_Type + (1 + Year\|Species_Code), family = ‘binomial’, data = grass_df)

# measure time taken…

``` r
start_time <- Sys.time()
```

# run a mixed effects model; year = fixed; species = random

``` r
grass_glmer <- glmer(Presence ~ scale(Year)  + (scale(Year)|Species_Code) + (1|Pentad), family = 'binomial', data = grass_df)
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 20.295 mins

# Summary of model

``` r
summary(grass_glmer)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ scale(Year) + (scale(Year) | Species_Code) + (1 |  
    ##     Pentad)
    ##    Data: grass_df
    ## 
    ##       AIC       BIC    logLik  deviance  df.resid 
    ## 1098966.9 1099040.4 -549477.5 1098954.9   1545612 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5790 -0.4382 -0.2713 -0.1306 23.4591 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev. Corr
    ##  Pentad       (Intercept) 1.1079   1.0526       
    ##  Species_Code (Intercept) 0.8819   0.9391       
    ##               scale(Year) 0.0144   0.1200   0.13
    ## Number of obs: 1545618, groups:  Pentad, 1862; Species_Code, 39
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.58556    0.10268 -25.181   <2e-16 ***
    ## scale(Year) -0.02543    0.02105  -1.208    0.227    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## scale(Year) 0.105

# Anova

``` r
Anova(grass_glmer)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Presence
    ##              Chisq Df Pr(>Chisq)
    ## scale(Year) 1.4589  1     0.2271

# Plot effects

``` r
plot(allEffects(grass_glmer))
```

    ## Warning in Analyze.model(focal.predictors, mod, xlevels, default.levels, : the
    ## predictor scale(Year) is a one-column matrix that was converted to a vector

![](grassland_glmer_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#predict

``` r
grass_df$predict <- predict(grass_glmer, grass_df)

grassland_clean <- grass_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))
```

    ## `summarise()` has grouped output by 'Species_Code'. You can override using the
    ## `.groups` argument.

``` r
ggplot(data=grassland_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))
```

![](grassland_glmer_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
