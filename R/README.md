Exploring COVID Data
================
Sara Colom
12/8/2020

# Goals:

Prepare a report on current COVID cases and make cool maps showing COVID
cases.

1.  Load libraries
2.  Read in data from the covidtracking API and from project directory.

# Explore data

``` r
head(data)
```

    ##       date state positive probableCases negative pending totalTestResultsSource
    ## 1 20201207    AK    36196            NA  1045944      NA        totalTestsViral
    ## 2 20201207    AL   272229         46440  1425784      NA  totalTestsPeopleViral
    ## 3 20201207    AR   172042         22867  1623898      NA        totalTestsViral
    ## 4 20201207    AS        0            NA     2140      NA        totalTestsViral
    ## 5 20201207    AZ   365843         12708  2035704      NA  totalTestsPeopleViral
    ## 6 20201207    CA  1366435            NA 24126916      NA        totalTestsViral
    ##   totalTestResults hospitalizedCurrently hospitalizedCumulative inIcuCurrently
    ## 1          1082140                   166                    805             NA
    ## 2          1651573                  2079                  27044             NA
    ## 3          1773073                  1053                   9445            382
    ## 4             2140                    NA                     NA             NA
    ## 5          2388839                  3059                  28273            736
    ## 6         25493351                 10998                     NA           2470
    ##   inIcuCumulative onVentilatorCurrently onVentilatorCumulative recovered
    ## 1              NA                    24                     NA      7165
    ## 2            2290                    NA                   1317    168387
    ## 3              NA                   182                   1044    151248
    ## 4              NA                    NA                     NA        NA
    ## 5              NA                   459                     NA     57040
    ## 6              NA                    NA                     NA        NA
    ##   dataQualityGrade    lastUpdateEt         dateModified checkTimeEt death
    ## 1                A 12/7/2020 03:59 2020-12-07T03:59:00Z 12/06 22:59   146
    ## 2                A 12/7/2020 11:00 2020-12-07T11:00:00Z 12/07 06:00  3892
    ## 3               A+ 12/7/2020 00:00 2020-12-07T00:00:00Z 12/06 19:00  2713
    ## 4                D 12/1/2020 00:00 2020-12-01T00:00:00Z 11/30 19:00     0
    ## 5               A+ 12/7/2020 00:00 2020-12-07T00:00:00Z 12/06 19:00  6950
    ## 6                B 12/7/2020 02:59 2020-12-07T02:59:00Z 12/06 21:59 19935
    ##   hospitalized          dateChecked totalTestsViral positiveTestsViral
    ## 1          805 2020-12-07T03:59:00Z         1082140              43903
    ## 2        27044 2020-12-07T11:00:00Z              NA                 NA
    ## 3         9445 2020-12-07T00:00:00Z         1773073                 NA
    ## 4           NA 2020-12-01T00:00:00Z            2140                 NA
    ## 5        28273 2020-12-07T00:00:00Z              NA                 NA
    ## 6           NA 2020-12-07T02:59:00Z        25493351                 NA
    ##   negativeTestsViral positiveCasesViral deathConfirmed deathProbable
    ## 1            1037022                 NA            146            NA
    ## 2                 NA             225789           3465           427
    ## 3            1623898             149175           2485           228
    ## 4                 NA                  0             NA            NA
    ## 5                 NA             353135           6431           519
    ## 6                 NA            1366435             NA            NA
    ##   totalTestEncountersViral totalTestsPeopleViral totalTestsAntibody
    ## 1                       NA                    NA                 NA
    ## 2                       NA               1651573                 NA
    ## 3                       NA                    NA                 NA
    ## 4                       NA                    NA                 NA
    ## 5                       NA               2388839             371261
    ## 6                       NA                    NA                 NA
    ##   positiveTestsAntibody negativeTestsAntibody totalTestsPeopleAntibody
    ## 1                    NA                    NA                       NA
    ## 2                    NA                    NA                    74956
    ## 3                    NA                    NA                       NA
    ## 4                    NA                    NA                       NA
    ## 5                    NA                    NA                       NA
    ## 6                    NA                    NA                       NA
    ##   positiveTestsPeopleAntibody negativeTestsPeopleAntibody
    ## 1                          NA                          NA
    ## 2                          NA                          NA
    ## 3                          NA                          NA
    ## 4                          NA                          NA
    ## 5                          NA                          NA
    ## 6                          NA                          NA
    ##   totalTestsPeopleAntigen positiveTestsPeopleAntigen totalTestsAntigen
    ## 1                      NA                         NA                NA
    ## 2                      NA                         NA                NA
    ## 3                  156801                      27881             21856
    ## 4                      NA                         NA                NA
    ## 5                      NA                         NA                NA
    ## 6                      NA                         NA                NA
    ##   positiveTestsAntigen fips positiveIncrease negativeIncrease    total
    ## 1                   NA    2              476             3888  1082140
    ## 2                   NA    1             2352             4658  1698013
    ## 3                 3300    5             1118             8919  1795940
    ## 4                   NA   60                0                0     2140
    ## 5                   NA    4             1567            16891  2401547
    ## 6                   NA    6            24735           273570 25493351
    ##   totalTestResultsIncrease   posNeg deathIncrease hospitalizedIncrease
    ## 1                     4364  1082140             3                    6
    ## 2                     6532  1698013             3                  713
    ## 3                     9923  1795940            53                   44
    ## 4                        0     2140             0                    0
    ## 5                    18340  2401547             0                   25
    ## 6                   298305 25493351            59                    0
    ##                                       hash commercialScore negativeRegularScore
    ## 1 76febf2618a346518b43da2fa5bbd7591a608060               0                    0
    ## 2 df860fde17a5e26e2d44bcdef12ed7928128398e               0                    0
    ## 3 003efdc66daa06f5067d9e0f1c19f48a57a42be0               0                    0
    ## 4 184abb6c50736a266d8165ce2eee0300b37622b5               0                    0
    ## 5 3d6b48f3f0135bca4182ebe8df867dae4ceccb16               0                    0
    ## 6 552f9cf03157e965371e7c64aa6dc117dd7be8d5               0                    0
    ##   negativeScore positiveScore score grade
    ## 1             0             0     0    NA
    ## 2             0             0     0    NA
    ## 3             0             0     0    NA
    ## 4             0             0     0    NA
    ## 5             0             0     0    NA
    ## 6             0             0     0    NA

# Compare positive cases and hospitalized currently.

``` r
pos_hos <- data %>% 
  select(positive, hospitalizedCurrently) %>% 
  gather() %>% 
  mutate(state = rep(data %>% pull(state), times = 2))

pos_hos <- pos_hos %>% 
  mutate(key = recode(key,
                           hospitalizedCurrently = "Currently Hospitalized",
                            positive = "Positive"))
```

``` r
ggplot(pos_hos %>% 
         filter(state != "AS"), aes(state, log(value), fill = key)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_fill_manual("Case status", values = c("blue", "gold")) +
  xlab("States and Territories") +
  ylab("Case Numbers (Log-Fold )") +
  ggtitle("COVID-19 Cases in the United States") +
  labs(caption = "Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases between states and U.S. territories.\nAmerican Samoa was excluded due to no data.\nNote: Y-axis case count is log-transformed.\nData Accessed & Plot created: 12/8/20") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
         plot.caption = element_text(hjust = 0, size = 8)) +
  theme(legend.position = "top")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

COVID Deaths total between states and territories.

``` r
ggplot(data %>% 
         filter(!is.na(state)) %>% 
         filter(deathConfirmed != 0), aes(state, deathConfirmed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9, fill = "gold", alpha = 0.8) +
  xlab("States and Territories") +
  ylab("Number of Deaths") +
  ggtitle("Deaths due to COVID-19 in the United States") +
  labs(caption = "Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases between states and U.S. territories.\nMissing states/territories imply no data.\nData Accessed & Plot created: 12/8/20") +
  theme_classic() +
  geom_text(aes(label = deathConfirmed), vjust = -1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
         plot.caption = element_text(hjust = 0, size = 8)) +
  theme(legend.position = "top")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Scatter plot of daily positive case increase versus daily death
increase.

``` r
ggplot(data %>% 
         filter(state != "AS"), aes(positiveIncrease, deathIncrease)) +
  geom_point(size = 5, color = "gold", alpha = 0.6) +
  xlab("Daily Increase in Positive Cases") +
  ylab("Daily Increase in Death Increase") +
  ggtitle("COVID-19 Cases in the United States") +
  labs(caption = "Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases between states and U.S. territories.\nAmerican Samoa was excluded due to no data.\nData Accessed & Plot created: 12/8/20") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.25, size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 20),
         plot.caption = element_text(hjust = 0, size = 8)) +
  theme(legend.position = "top")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Scatterplot above w/o outlier (positive increase \> 24,000).

``` r
ggplot(data %>% 
         filter(state != "AS" & positiveIncrease < 24735), aes(positiveIncrease, deathIncrease)) +
  geom_point(size = 5, color = "gold", alpha = 0.6) +
  xlab("Daily increase in Positive Cases") +
  ylab("Daily Increase in Death") +
  ggtitle("COVID-19 Cases in the United States") +
  labs(caption = "Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases across states and U.S. territories.\nAmerican Samoa was excluded due to no data.\nData Accessed & Plot created: 12/8/20") +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.25, size = 8, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 20),
         plot.caption = element_text(hjust = 0, size = 8)) +
  theme(legend.position = "top")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Mapping counts

1.  Combine the main data set with the info data set.

<!-- end list -->

``` r
data <- data %>% 
  rename(abbreviation = state)


data <- data %>% 
  left_join(info)
```
