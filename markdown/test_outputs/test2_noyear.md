test2_noyear
================
Christopher Shortland
2022-06-01

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
fyn_df <- filter(final, indicator=='fynbos')
```

\#number of indicator species

``` r
length(unique(fyn_df$Species_Code))
```

    ## [1] 15

# add in year variable

``` r
fyn_df$Year <- lubridate::year(fyn_df$StartDate)
str(fyn_df)
```

    ## spec_tbl_df [184,736 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:184736] 51 51 51 51 51 51 51 51 51 51 ...
    ##  $ CardNo        : chr [1:184736] "1800-3300_001449_20150411" "1845_3415_011138_20090904" "1845_3415_011138_20090908" "1855_3420_011138_20090831" ...
    ##  $ StartDate     : Date[1:184736], format: "2015-04-11" "2009-09-04" ...
    ##  $ Pentad        : chr [1:184736] "3300_1800" "3415_1845" "3415_1845" "3420_1855" ...
    ##  $ Spp           : chr [1:184736] "-" "-" "-" "-" ...
    ##  $ Sequence      : chr [1:184736] "-" "-" "-" "-" ...
    ##  $ Common_name   : chr [1:184736] "-" "-" "-" "-" ...
    ##  $ Taxonomic_name: chr [1:184736] "-" "-" "-" "-" ...
    ##  $ Presence      : num [1:184736] 0 0 0 0 0 0 0 1 0 0 ...
    ##  $ no_pres       : num [1:184736] 14 36 36 98 98 98 66 4 5 74 ...
    ##  $ indicator     : chr [1:184736] "fynbos" "fynbos" "fynbos" "fynbos" ...
    ##  $ Year          : num [1:184736] 2015 2009 2009 2009 2009 ...
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

## glmer1 \<- glmer(Presence \~ Year\*Indicator_Type + (1 + Year\|Species_Code), family = ???binomial???, data = fyn_df)

# measure time taken???

``` r
start_time <- Sys.time()
```

# run a mixed effects model; year = fixed; species = random

``` r
glmer1 <- glmer(Presence ~ Year  + (1|Species_Code) + (1|Pentad), family = 'binomial', data = fyn_df)
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

``` r
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 1.162412 mins

# Summary of model

``` r
summary(glmer1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ Year + (1 | Species_Code) + (1 | Pentad)
    ##    Data: fyn_df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 170952.8 170993.3 -85472.4 170944.8   184732 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -10.3549  -0.5268  -0.2790   0.5730  19.1643 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance Std.Dev.
    ##  Pentad       (Intercept) 1.582    1.258   
    ##  Species_Code (Intercept) 2.969    1.723   
    ## Number of obs: 184736, groups:  Pentad, 486; Species_Code, 15
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) 17.2396927  0.7971660   21.63   <2e-16 ***
    ## Year        -0.0098065  0.0004472  -21.93   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Year -0.869
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?
    ## Model is nearly unidentifiable: large eigenvalue ratio
    ##  - Rescale variables?

# Anova

``` r
Anova(glmer1)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Presence
    ##       Chisq Df Pr(>Chisq)    
    ## Year 480.79  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Plot effects

``` r
plot(allEffects(glmer1))
```

![](test2_noyear_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
\#predict

``` r
fyn_df$predict <- predict(glmer1, fyn_df)

fyn_clean <- fyn_df %>% group_by(Species_Code, Year) %>% summarise(predict = mean(predict))
```

    ## `summarise()` has grouped output by 'Species_Code'. You can override using the
    ## `.groups` argument.

``` r
ggplot(data=fyn_clean, aes(Year, predict)) + geom_line(aes(color = as.factor(Species_Code)))
```

![](test2_noyear_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
