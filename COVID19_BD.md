COVID-19 Bangladesh EDA and Daily Cases Prediction
================

``` r
library(htmltab)
library(tidyverse)
```

    ## -- Attaching packages ------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.0
    ## v tidyr   1.1.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(prophet)
```

    ## Loading required package: Rcpp

    ## Loading required package: rlang

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
    ##     flatten_lgl, flatten_raw, invoke, list_along, modify, prepend,
    ##     splice

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(XML)
library(httr)
```

    ## 
    ## Attaching package: 'httr'

    ## The following object is masked from 'package:plotly':
    ## 
    ##     config

``` r
library(tidyquant)
```

    ## Loading required package: PerformanceAnalytics

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

    ## Loading required package: quantmod

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Version 0.4-0 included new data defaults. See ?getSymbols.

    ## == Need to Learn tidyquant? ===================
    ## Business Science offers a 1-hour course - Learning Lab #9: Performance Analysis & Portfolio Optimization with tidyquant!
    ## </> Learn more at: https://university.business-science.io/p/learning-labs-pro </>

``` r
url<- 'https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Bangladesh'
r<- GET(url)
doc<- readHTMLTable(doc = content(r, "text"))
#names(doc) [for fininding data table]

df<- htmltab("https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Bangladesh",10)
```

    ## Warning: Columns [Notes] seem to have no data and are removed. Use
    ## rm_nodata_cols = F to suppress this behavior

``` r
dim(df)
```

    ## [1] 340  10

``` r
df<- df %>% 
        janitor::clean_names()
names(df)
```

    ##  [1] "date"                                
    ##  [2] "total_tested"                        
    ##  [3] "total_cases"                         
    ##  [4] "total_deaths"                        
    ##  [5] "total_recovered"                     
    ##  [6] "in_the_last_24_hours_newly_tested"   
    ##  [7] "in_the_last_24_hours_new_cases"      
    ##  [8] "in_the_last_24_hours_new_deaths"     
    ##  [9] "in_the_last_24_hours_newly_recovered"
    ## [10] "day_of_pandemic_outbreak"

``` r
df$date<- ymd(df$date)
df[12,2]<- 433
df[12,6]<- 36

df<- df %>% 
  mutate(in_the_last_24_hours_newly_tested= if_else(is.na(in_the_last_24_hours_newly_tested),"0", in_the_last_24_hours_newly_tested),
         in_the_last_24_hours_new_cases= if_else(is.na(in_the_last_24_hours_new_cases),"0",in_the_last_24_hours_new_cases),
         in_the_last_24_hours_new_deaths=if_else(is.na(in_the_last_24_hours_new_deaths),"0",in_the_last_24_hours_new_deaths),
         in_the_last_24_hours_newly_recovered=if_else(is.na(in_the_last_24_hours_newly_recovered),"0",in_the_last_24_hours_newly_recovered))%>%
  separate(day_of_pandemic_outbreak, into= c("day_of_pandemic","day_of_outbreak"), sep = "/")
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 3 rows [1, 2, 3].

``` r
 df[1,10]<- 0
 df[1:3,11]<- 0 
```

``` r
df[,-c(1)]<- sapply(df[,-c(1)], function(x) as.numeric(gsub(",","",x))) 
df<- na.omit(df)
dim(df)
```

    ## [1] 340  11

``` r
df<- df %>% 
  mutate( in_the_last_24_hours_newly_tested = total_tested - lag(total_tested),
          in_the_last_24_hours_new_cases = total_cases - lag(total_cases),
          in_the_last_24_hours_new_deaths = total_deaths - lag(total_deaths),
          in_the_last_24_hours_newly_recovered = total_recovered - lag(total_recovered))


df[1,6:9] <- df[1,2:5]
str(df)
```

    ## 'data.frame':    340 obs. of  11 variables:
    ##  $ date                                : Date, format: "2020-03-08" "2020-03-11" ...
    ##  $ total_tested                        : num  111 142 163 187 211 231 241 277 351 397 ...
    ##  $ total_cases                         : num  3 3 3 3 5 5 8 10 14 17 ...
    ##  $ total_deaths                        : num  0 0 0 0 0 0 0 0 1 1 ...
    ##  $ total_recovered                     : num  0 2 2 2 3 3 3 3 3 3 ...
    ##  $ in_the_last_24_hours_newly_tested   : num  111 31 21 24 24 20 10 36 74 46 ...
    ##  $ in_the_last_24_hours_new_cases      : num  3 0 0 0 2 0 3 2 4 3 ...
    ##  $ in_the_last_24_hours_new_deaths     : num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ in_the_last_24_hours_newly_recovered: num  0 2 0 0 1 0 0 0 0 0 ...
    ##  $ day_of_pandemic                     : num  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ day_of_outbreak                     : num  0 0 0 1 2 3 4 5 6 7 ...

``` r
  df %>%
  ggplot(aes(date, in_the_last_24_hours_new_cases)) +
    # Train Region
    annotate("text", x = ymd("2020-03-20"), y = 3500,
             color = palette_light()[[1]], label = "Train Region") +
    # Validation Region
    geom_rect(xmin = as.numeric(ymd("2020-11-01")), 
              xmax = as.numeric(ymd("2020-12-31")),
              ymin = 0, ymax = Inf, alpha = 0.02,
              fill = palette_light()[[3]]) +
    annotate("text", x = ymd("2020-11-29"), y = 3500,
             color = palette_light()[[1]], label = "Validation\nRegion") +
    # Test Region
    geom_rect(xmin = as.numeric(ymd("2021-01-01")), 
              xmax = as.numeric(ymd("2021-02-12")),
              ymin = 0, ymax = Inf, alpha = 0.02,
              fill = palette_light()[[4]]) +
    annotate("text", x = ymd("2021-01-19"), y = 3500,
             color = palette_light()[[1]], label = "Test\nRegion") +
    # Data
    geom_line(col = palette_light()[1]) +
    geom_point(col = palette_light()[1]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    # Aesthetics
    theme_tq() +
    scale_x_date(date_breaks = waiver(), date_labels = waiver()) +
    labs(title = "COVID-19: March 2020 To February 2021",
         subtitle = "Train, Validation, and Test Sets Shown",
         x = "",
         y = "Daily new cases")
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(df)+
  geom_line(aes(date,in_the_last_24_hours_new_cases))+
  scale_x_date(breaks =  waiver(), labels = waiver())+
  labs(title =  "Daily cases of COVID-19 Patient", subtitle = "From March to middle of February", y = "daily cases")
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(df)+
  geom_line(aes(day_of_pandemic,in_the_last_24_hours_new_deaths))+
  labs(title =  "Daily death cases of COVID-19 Patient", subtitle = "From March to middle of February", y = "daily  death cases")
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ds<- df$date
y<- df$total_cases
dts<- data.frame(ds,y)
mts<- prophet(dts)
```

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future_ts<- make_future_dataframe(mts, periods = 30)
forcast_ts<- predict(mts, future_ts)
dyplot.prophet(mts,forcast_ts)
```

    ## Warning: `select_()` is deprecated as of dplyr 0.7.0.
    ## Please use `select()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

![](COVID19_BD_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
prophet_plot_components(mts,forcast_ts)
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
pre_ts<- forcast_ts$yhat[1:340]
actual_ts<- mts$history$y
plot(pre_ts, actual_ts)
abline(lm(pre_ts~actual_ts), col="blue")
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ds<- df$date
y<- df$total_deaths
dtd<- data.frame(ds,y)
mtd<- prophet(dtd)
```

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future_td<- make_future_dataframe(mtd, periods = 30)
forcast_td<- predict(mtd, future_td)
dyplot.prophet(mtd,forcast_td)
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
prophet_plot_components(mtd,forcast_td)
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
pre_td<- forcast_td$yhat[1:340]
actual_td<- mtd$history$y
plot(pre_td, actual_td)
abline(lm(pre_td~actual_td), col="blue")
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
ds<- df$date
y<- df$in_the_last_24_hours_new_cases
dtns<- data.frame(ds,y)
mtns<- prophet(dtns)
```

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future_tns<- make_future_dataframe(mtns, periods = 180)
forcast_tns<- predict(mtns, future_tns)
dyplot.prophet(mtns,forcast_tns)
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
prophet_plot_components(mtns,forcast_tns)
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
pre_tns<- forcast_tns$yhat[1:340]
actual_tns<- mtns$history$y
plot(pre_tns, actual_tns)
abline(lm(pre_tns~actual_tns), col="blue")
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
ds<- df$date
y<- df$in_the_last_24_hours_new_deaths
dtnd<- data.frame(ds,y)
mtnd<- prophet(dtnd)
```

    ## Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.

    ## Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

``` r
future_tnd<- make_future_dataframe(mtnd, periods = 30)
forcast_tnd<- predict(mtnd, future_tnd)
dyplot.prophet(mtnd,forcast_tnd)
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
prophet_plot_components(mtnd,forcast_tnd)
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
pre_tnd<- forcast_tnd$yhat[1:340]
actual_tnd<- mtnd$history$y
plot(pre_tnd, actual_tnd)
abline(lm(pre_tnd~actual_tnd), col="blue")
```

![](COVID19_BD_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
library(h2o)        # Awesome ML Library
```

    ## 
    ## ----------------------------------------------------------------------
    ## 
    ## Your next step is to start H2O:
    ##     > h2o.init()
    ## 
    ## For H2O package documentation, ask for help:
    ##     > ??h2o
    ## 
    ## After starting H2O, you can use the Web UI at http://localhost:54321
    ## For more information visit http://docs.h2o.ai
    ## 
    ## ----------------------------------------------------------------------

    ## 
    ## Attaching package: 'h2o'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     day, hour, month, week, year

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cor, sd, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     %*%, %in%, &&, ||, apply, as.factor, as.numeric, colnames,
    ##     colnames<-, ifelse, is.character, is.factor, is.numeric, log,
    ##     log10, log1p, log2, round, signif, trunc

``` r
library(timetk)     # Toolkit for working with time series in R
```

    ## Warning: package 'timetk' was built under R version 4.0.3

``` r
library(tidyquant) 


cdf<- df %>% 
  select(date, in_the_last_24_hours_new_cases) 


ndf<- df %>% 
  select(date, in_the_last_24_hours_new_cases) %>% 
  tk_augment_timeseries_signature()
```

    ## tk_augment_timeseries_signature(): Using the following .date_var variable: date

``` r
ndf %>% 
  glimpse()
```

    ## Rows: 340
    ## Columns: 30
    ## $ date                           <date> 2020-03-08, 2020-03-11, 2020-03-12,...
    ## $ in_the_last_24_hours_new_cases <dbl> 3, 0, 0, 0, 2, 0, 3, 2, 4, 3, 3, 4, ...
    ## $ index.num                      <dbl> 1583625600, 1583884800, 1583971200, ...
    ## $ diff                           <dbl> NA, 259200, 86400, 86400, 86400, 864...
    ## $ year                           <int> 2020, 2020, 2020, 2020, 2020, 2020, ...
    ## $ year.iso                       <int> 2020, 2020, 2020, 2020, 2020, 2020, ...
    ## $ half                           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ quarter                        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ month                          <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, ...
    ## $ month.xts                      <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, ...
    ## $ month.lbl                      <ord> March, March, March, March, March, M...
    ## $ day                            <int> 8, 11, 12, 13, 14, 15, 16, 17, 18, 1...
    ## $ hour                           <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ minute                         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ second                         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ hour12                         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ am.pm                          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    ## $ wday                           <int> 1, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, ...
    ## $ wday.xts                       <int> 0, 3, 4, 5, 6, 0, 1, 2, 3, 4, 5, 6, ...
    ## $ wday.lbl                       <ord> Sunday, Wednesday, Thursday, Friday,...
    ## $ mday                           <int> 8, 11, 12, 13, 14, 15, 16, 17, 18, 1...
    ## $ qday                           <int> 68, 71, 72, 73, 74, 75, 76, 77, 78, ...
    ## $ yday                           <int> 68, 71, 72, 73, 74, 75, 76, 77, 78, ...
    ## $ mweek                          <int> 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, ...
    ## $ week                           <int> 10, 11, 11, 11, 11, 11, 11, 11, 12, ...
    ## $ week.iso                       <int> 10, 11, 11, 11, 11, 11, 12, 12, 12, ...
    ## $ week2                          <int> 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, ...
    ## $ week3                          <int> 1, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, ...
    ## $ week4                          <int> 2, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, ...
    ## $ mday7                          <int> 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, ...

``` r
ndf_clean<- ndf %>% 
  select_if(~ !is.Date(.)) %>% 
  select_if(~ !any(is.na(.))) %>% 
  mutate_if(is.ordered, ~as.character(.) %>% as.factor)

train_tbl<- ndf_clean %>% filter( month <7)
valid_tbl<- ndf_clean %>% filter( month == 12 | month == 11)
test_tbl<- ndf_clean %>% filter(month == 01 | month == 02)

h2o.init()
```

    ##  Connection successful!
    ## 
    ## R is connected to the H2O cluster: 
    ##     H2O cluster uptime:         45 minutes 6 seconds 
    ##     H2O cluster timezone:       Asia/Dhaka 
    ##     H2O data parsing timezone:  UTC 
    ##     H2O cluster version:        3.30.0.7 
    ##     H2O cluster version age:    6 months and 26 days !!! 
    ##     H2O cluster name:           H2O_started_from_R_Sahadat___pzv484 
    ##     H2O cluster total nodes:    1 
    ##     H2O cluster total memory:   2.79 GB 
    ##     H2O cluster total cores:    4 
    ##     H2O cluster allowed cores:  4 
    ##     H2O cluster healthy:        TRUE 
    ##     H2O Connection ip:          localhost 
    ##     H2O Connection port:        54321 
    ##     H2O Connection proxy:       NA 
    ##     H2O Internal Security:      FALSE 
    ##     H2O API Extensions:         Amazon S3, Algos, AutoML, Core V3, TargetEncoder, Core V4 
    ##     R Version:                  R version 4.0.2 (2020-06-22)

    ## Warning in h2o.clusterInfo(): 
    ## Your H2O cluster version is too old (6 months and 26 days)!
    ## Please download and install the latest version from http://h2o.ai/download/

``` r
h2o.no_progress()

#Before running below code, please run library(bit64)
train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

y <- "in_the_last_24_hours_new_cases"
x <- names(train_h2o) %>% setdiff(y)

automl_models_h2o <- h2o.automl(
    x = x, 
    y = y, 
    training_frame = train_h2o, 
    validation_frame = valid_h2o, 
    leaderboard_frame = test_h2o, 
    max_runtime_secs = 60, 
    stopping_metric = "deviance")
```

    ## 
    ## 23:33:08.140: User specified a validation frame with cross-validation still enabled. Please note that the models will still be validated using cross-validation only, the validation frame will be used to provide purely informative validation metrics on the trained models.
    ## 23:33:08.141: AutoML: XGBoost is not available; skipping it.
    ## 23:33:14.189: Skipping training of model GBM_5_AutoML_20210216_233308 due to exception: water.exceptions.H2OModelBuilderIllegalArgumentException: Illegal argument(s) for GBM model: GBM_5_AutoML_20210216_233308.  Details: ERRR on field: _min_rows: The dataset size is too small to split for min_rows=100.0: must have at least 200.0 (weighted) rows, but have only 156.0.

``` r
automl_leader <- automl_models_h2o@leader

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

h2o.performance(automl_leader, newdata = test_h2o)
```

    ## H2ORegressionMetrics: gbm
    ## 
    ## MSE:  2.906568
    ## RMSE:  1.704866
    ## MAE:  1.531658
    ## RMSLE:  0.004015597
    ## Mean Residual Deviance :  2.906568

``` r
cdf %>% 
    openair:: selectByDate( start = "2021-01-01", end = "2021-02-12") %>%
    add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
    rename(actual = in_the_last_24_hours_new_cases) %>%
    mutate(
        error     = actual - pred,
        error_pct = error / actual
        ) %>%
    summarise(
        ME   = mean(error),
        RMSE = mean(error^2)^0.5,
        MAE  = mean(abs(error)),
        MAPE = mean(abs(error_pct)),
        MPE  = mean(error_pct)
    ) %>%
    glimpse()
```

    ## Rows: 1
    ## Columns: 5
    ## $ ME   <dbl> -1.485168
    ## $ RMSE <dbl> 1.704866
    ## $ MAE  <dbl> 1.531658
    ## $ MAPE <dbl> 0.003236253
    ## $ MPE  <dbl> -0.003190973

``` r
new_d<- data.frame( date = as.Date("2021-02-13") + sort(sample(1:20, 20)))

new_h20<- new_d %>% 
  tk_augment_timeseries_signature() %>% 
  select_if(~ !is.Date(.)) %>% 
  select_if(~ !any(is.na(.))) %>% 
  mutate_if(is.ordered, ~as.character(.) %>% as.factor) %>% 
  as.h2o()
```

    ## tk_augment_timeseries_signature(): Using the following .date_var variable: date

``` r
new_d %>% 
  add_column(Predicted_cases = h2o.predict(automl_leader, newdata = new_h20) %>%   as_tibble() %>% 
  pull(predict))
```

    ##          date Predicted_cases
    ## 1  2021-02-14        368.1123
    ## 2  2021-02-15        384.0719
    ## 3  2021-02-16        480.9544
    ## 4  2021-02-17        472.0065
    ## 5  2021-02-18        487.3558
    ## 6  2021-02-19        488.7832
    ## 7  2021-02-20        404.4042
    ## 8  2021-02-21        430.2300
    ## 9  2021-02-22        460.9229
    ## 10 2021-02-23        472.4997
    ## 11 2021-02-24        444.5073
    ## 12 2021-02-25        463.6892
    ## 13 2021-02-26        357.5753
    ## 14 2021-02-27        278.6499
    ## 15 2021-02-28        363.5338
    ## 16 2021-03-01        294.1852
    ## 17 2021-03-02        328.9740
    ## 18 2021-03-03        318.6245
    ## 19 2021-03-04        327.3121
    ## 20 2021-03-05        307.7691
