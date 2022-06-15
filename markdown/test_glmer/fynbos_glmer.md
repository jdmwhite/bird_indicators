fynbos_glmer
================
Christopher Shortland
2022-06-11

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
library(sjPlot)
```

    ## Registered S3 method overwritten by 'parameters':
    ##   method                         from      
    ##   format.parameters_distribution datawizard

``` r
library(glmmTMB)
```

    ## Warning in checkMatrixPackageVersion(): Package version inconsistency detected.
    ## TMB was built with Matrix version 1.4.1
    ## Current Matrix version is 1.4.0
    ## Please re-install 'TMB' from source using install.packages('TMB', type = 'source') or ask CRAN for a binary version of 'TMB' matching CRAN's 'Matrix' package

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
fyn_df %>% filter(!Common_name %in% c('Jaeger, Pomarine',"Gull, Sabine's", "Chaffinch, Common", "Stint, Temminck's", "Scrub Robin, Rufous-tailed", "Partridge, Chukar", "Gull, Hartlaub's", "Cormorant, Crowned")) -> fyn_df

length(unique(fyn_df$Common_name))
```

    ## [1] 8

``` r
unique(fyn_df$Common_name)
```

    ## [1] "-"                        "Spurfowl, Cape"          
    ## [3] "Rockjumper, Cape"         "Warbler, Victorin's"     
    ## [5] "Sugarbird, Cape"          "Sunbird, Orange-breasted"
    ## [7] "Siskin, Cape"             "Canary, Protea"

# add in year variable

``` r
fyn_df$Year <- lubridate::year(fyn_df$StartDate)
str(fyn_df)
```

    ## spec_tbl_df [166,170 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Species_Code  : num [1:166170] 51 51 51 51 51 51 51 51 51 51 ...
    ##  $ CardNo        : chr [1:166170] "1800-3300_001449_20150411" "1845_3415_011138_20090904" "1845_3415_011138_20090908" "1855_3420_011138_20090831" ...
    ##  $ StartDate     : Date[1:166170], format: "2015-04-11" "2009-09-04" ...
    ##  $ Pentad        : chr [1:166170] "3300_1800" "3415_1845" "3415_1845" "3420_1855" ...
    ##  $ Spp           : chr [1:166170] "-" "-" "-" "-" ...
    ##  $ Sequence      : chr [1:166170] "-" "-" "-" "-" ...
    ##  $ Common_name   : chr [1:166170] "-" "-" "-" "-" ...
    ##  $ Taxonomic_name: chr [1:166170] "-" "-" "-" "-" ...
    ##  $ Presence      : num [1:166170] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ no_pres       : num [1:166170] 14 36 36 98 98 98 66 5 74 6 ...
    ##  $ indicator     : chr [1:166170] "fynbos" "fynbos" "fynbos" "fynbos" ...
    ##  $ Year          : num [1:166170] 2015 2009 2009 2009 2009 ...
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

# glmer1 \<- glmer(Presence \~ Year\*Indicator_Type + (1 + Year\|Species_Code), family = ‘binomial’, data = fyn_df)

# measure time taken…

``` r
start_time <- Sys.time()
```

# run a mixed effects model; year = fixed; species = random

``` r
glmer1 <- glmer(Presence ~ Year + (Year|Species_Code) + (1|Pentad), family = 'binomial', data = fyn_df)
```

    ## Warning in optwrap(optimizer, devfun, start, rho$lower, control = control, :
    ## convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations
    ## exceeded

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 2.81023 (tol = 0.002, component 1)

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, : Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

``` r
beepr::beep()
end_time <- Sys.time()
```

# How long did this take to run?

``` r
end_time - start_time
```

    ## Time difference of 2.610332 hours

# Summary of model

``` r
summary(glmer1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: Presence ~ Year + (Year | Species_Code) + (1 | Pentad)
    ##    Data: fyn_df
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 122830.6 122890.7 -61409.3 122818.6   166164 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1548 -0.4734 -0.1510  0.0000 14.8724 
    ## 
    ## Random effects:
    ##  Groups       Name        Variance  Std.Dev. Corr 
    ##  Pentad       (Intercept) 1.318e+00  1.148        
    ##  Species_Code (Intercept) 2.385e+03 48.833        
    ##               Year        3.028e-02  0.174   -0.73
    ## Number of obs: 166170, groups:  Pentad, 486; Species_Code, 15
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  2.540e+01  2.518e-05 1008636   <2e-16 ***
    ## Year        -3.286e-02  2.517e-05   -1305   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Year 0.000 
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 2.81023 (tol = 0.002, component 1)
    ## Model is nearly unidentifiable: very large eigenvalue
    ##  - Rescale variables?

# Anova

``` r
Anova(glmer1)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: Presence
    ##        Chisq Df Pr(>Chisq)    
    ## Year 1703740  1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Plot effects

``` r
plot(allEffects(glmer1))
```

![](fynbos_glmer_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
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

![](fynbos_glmer_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot_model(glmer1, type = "re")
```

    ## [[1]]

![](fynbos_glmer_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    ## 
    ## [[2]]

![](fynbos_glmer_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->
