Exploring COVID Data
================
Sara Colom
12/8/2020

# Goals:

Prepare a report on current COVID cases and make cool maps showing COVID
cases.

1.  Load libraries
2.  Read in data from the COVID-tracking API and from project directory.

# Explore data

``` r
head(data)
```

    ##       date state positive probableCases negative pending totalTestResultsSource
    ## 1 20210117    AK    50296            NA  1354460      NA        totalTestsViral
    ## 2 20210117    AL   422598         86545  1693435      NA  totalTestsPeopleViral
    ## 3 20210117    AR   271154         52186  2053730      NA        totalTestsViral
    ## 4 20210117    AS        0            NA     2140      NA        totalTestsViral
    ## 5 20210117    AZ   673882         37418  2574357      NA        totalTestsViral
    ## 6 20210117    CA  2942475            NA 35286140      NA        totalTestsViral
    ##   totalTestResults hospitalizedCurrently hospitalizedCumulative inIcuCurrently
    ## 1          1404756                    70                   1153             NA
    ## 2          2029488                  2716                  38763             NA
    ## 3          2272698                  1271                  12701            423
    ## 4             2140                    NA                     NA             NA
    ## 5          6029520                  4773                  47081           1118
    ## 6         38228615                 21143                     NA           4820
    ##   inIcuCumulative onVentilatorCurrently onVentilatorCumulative recovered
    ## 1              NA                     8                     NA      7165
    ## 2            2484                    NA                   1420    221961
    ## 3              NA                   221                   1348    241926
    ## 4              NA                    NA                     NA        NA
    ## 5              NA                   800                     NA     91604
    ## 6              NA                    NA                     NA        NA
    ##   dataQualityGrade    lastUpdateEt         dateModified checkTimeEt death
    ## 1               A+ 1/17/2021 03:59 2021-01-17T03:59:00Z 01/16 22:59   229
    ## 2                A 1/17/2021 11:00 2021-01-17T11:00:00Z 01/17 06:00  6120
    ## 3               A+ 1/17/2021 00:00 2021-01-17T00:00:00Z 01/16 19:00  4311
    ## 4              N/A 12/1/2020 00:00 2020-12-01T00:00:00Z 11/30 19:00     0
    ## 5               A+ 1/17/2021 00:00 2021-01-17T00:00:00Z 01/16 19:00 11266
    ## 6                B 1/17/2021 02:59 2021-01-17T02:59:00Z 01/16 21:59 33392
    ##   hospitalized          dateChecked totalTestsViral positiveTestsViral
    ## 1         1153 2021-01-17T03:59:00Z         1404756              60195
    ## 2        38763 2021-01-17T11:00:00Z              NA                 NA
    ## 3        12701 2021-01-17T00:00:00Z         2272698                 NA
    ## 4           NA 2020-12-01T00:00:00Z            2140                 NA
    ## 5        47081 2021-01-17T00:00:00Z         6029520                 NA
    ## 6           NA 2021-01-17T02:59:00Z        38228615                 NA
    ##   negativeTestsViral positiveCasesViral deathConfirmed deathProbable
    ## 1            1343030                 NA             NA            NA
    ## 2                 NA             336053           5098          1022
    ## 3            2053730             218968           3562           749
    ## 4                 NA                  0             NA            NA
    ## 5                 NA             636464          10124          1142
    ## 6                 NA            2942475             NA            NA
    ##   totalTestEncountersViral totalTestsPeopleViral totalTestsAntibody
    ## 1                       NA                    NA                 NA
    ## 2                       NA               2029488                 NA
    ## 3                       NA                    NA                 NA
    ## 4                       NA                    NA                 NA
    ## 5                       NA               3210821             404864
    ## 6                       NA                    NA                 NA
    ##   positiveTestsAntibody negativeTestsAntibody totalTestsPeopleAntibody
    ## 1                    NA                    NA                       NA
    ## 2                    NA                    NA                    97007
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
    ## 3                  312662                      61516                NA
    ## 4                      NA                         NA                NA
    ## 5                      NA                         NA                NA
    ## 6                      NA                         NA                NA
    ##   positiveTestsAntigen fips positiveIncrease negativeIncrease    total
    ## 1                   NA    2              279             5828  1404756
    ## 2                   NA    1             1917             6755  2116033
    ## 3                   NA    5              976            10573  2324884
    ## 4                   NA   60                0                0     2140
    ## 5                   NA    4             6981            11175  3248239
    ## 6                   NA    6            42229           384131 38228615
    ##   totalTestResultsIncrease   posNeg deathIncrease hospitalizedIncrease
    ## 1                     6107  1404756             0                    1
    ## 2                     8365  2116033             1                    0
    ## 3                    11493  2324884            18                   15
    ## 4                        0     2140             0                    0
    ## 5                    60494  3248239            18                  433
    ## 6                   426360 38228615           432                    0
    ##                                       hash commercialScore negativeRegularScore
    ## 1 faf064fc716c0c6920e67e6c5542391af0f401a7               0                    0
    ## 2 76a68fe7e35c1d74478bc169f40223f0a8615cce               0                    0
    ## 3 d61c6c9deed202f3661cd8b8a487ec8e92618d14               0                    0
    ## 4 627c446392c701b91de3b2f614c15acbb0c81e52               0                    0
    ## 5 4f2d06c56ea988e53c70dd2b31e31f59d6d6fb5b               0                    0
    ## 6 ddcb6ef73ec2f71509678afb75d4237bbf2db6fa               0                    0
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
  labs(caption = paste("Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases between states and U.S. territories.\nAmerican Samoa was excluded due to no data.\nNote: Y-axis case count is log-transformed.\nData Accessed & Plot created:",current_date)) +
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
  labs(caption = paste("Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases between states and U.S. territories.\nMissing states/territories imply no data.\nData Accessed & Plot created:", current_date)) +
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
  labs(caption = paste("Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases between states and U.S. territories.\nAmerican Samoa was excluded due to no data.\nData Accessed & Plot created:", current_date)) +
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
  labs(caption = paste("Data source: https://covidtracking.com/data/api\nNumber of COVID-19 cases across states and U.S. territories.\nAmerican Samoa was excluded due to no data.\nData Accessed & Plot created:", current_date)) +
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

# State level trends

``` r
# Make vector of states to read in.

states <- c("ca", "mi", "ny", "fl", "ga", "tx", "wi", "wa", "va", "in")

state_data <- states %>% 
  purrr::map_df(~read_state(.))



# make date variables; one for month, year and day

iso <- "([0-9]{4})([0-1][0-9])([0-3][0-9])" # Year-Month-Day to match

dates <- rematch2::re_match(text = state_data %>% pull(date), pattern = iso) %>% 
  rename(Year = 1,
         Month = 2,
         Day = 3) %>% 
  unite("Date", c(Year, Month, Day), sep = "-", remove = FALSE)


# Incorporate data to the main data frame.  

state_data <- state_data %>% 
  left_join(
    dates %>% 
      rename(date = `.match`) %>% 
      mutate(date = as.integer(date)) %>% 
      select(- `.text`)
  )

rm(dates)
```

Observed positive cases

``` r
ggplot(data = state_data) +
  geom_point(aes(x = Date, y = positive, color = state)) +
  ylab("Daily Positive Cases") +
  labs(color = "State") +
  theme_minimal()# +
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
  #com_theme
```

## Visualize

# Global trends

Prepare a scatter plot animation for multiple countries cases vs deaths.

First read in and wrangle data.

``` r
coun_res <- GET("https://corona.lmao.ninja/v2/historical?lastdays=100") # read in data

coun_data = fromJSON(rawToChar(coun_res$content)) # Convert json to tabular data

countrylist <- c("USA", 
                 "South Africa",
                 "Mexico", 
                 "UK", 
                 "Brazil",
                 "Italy",
                 "India",
                 "China",
                 "Australia",
                 "Cuba",
                 "Niger",
                 "Ghana",
                "Spain",
                "Japan",
                "Peru",
                "Iran"
                ) # Countries to analyze

# Index of countries
index <- coun_data %>%
  pull(country) %in% countrylist %>% 
  which()

# Dates to capture (1st and 15th day of each month)

captdate <- c(paste(c(1:12), rep("1/20", times = 12), sep = "/"), # first day of month
           paste(c(1:12), rep("15/20", times = 12), sep = "/")) %>%  # 15th day of month
  mdy()

countries <- coun_data %>% 
  filter(country %in% countrylist) %>%
  pull(country)

province <- coun_data %>% 
  filter(country %in% countrylist) %>%
  pull(province)

cases <- coun_data$timeline$cases %>% 
  slice(index) %>% 
  gather() %>% 
  rename(Date = key,
         Cases = value) %>% 
  mutate(Date = mdy(Date)) %>% 
  filter(Date %in% captdate)

cases <- cases %>% 
  mutate(Country = rep(countries, times = (nrow(cases)/length(index)))) %>% 
  mutate(Province =  rep(province, times = (nrow(cases)/length(index))))
  

deaths <- coun_data$timeline$deaths  %>% 
  slice(index) %>% 
  gather() %>% 
  rename(Date = key,
         Deaths = value) %>% 
  mutate(Date = mdy(Date)) %>% 
  filter(Date %in% captdate)

deaths <- deaths %>% 
  mutate(Country = rep(countries, times = (nrow(deaths)/length(index)))) %>% 
  mutate(Country = rep(countries, times = (nrow(deaths)/length(index)))) %>% 
  mutate(Province =  rep(province, times = (nrow(deaths)/length(index))))
  

recovered <- coun_data$timeline$recovered  %>% 
  slice(index) %>% 
  gather() %>% 
  rename(Date = key,
         Recovered = value) %>% 
  mutate(Date = mdy(Date)) %>% 
  filter(Date %in% captdate)

recovered <- recovered %>% 
  mutate(Country = rep(countries, times = (nrow(recovered)/length(index))))  %>% 
  mutate(Country = rep(countries, times = (nrow(recovered)/length(index)))) %>% 
  mutate(Province =  rep(province, times = (nrow(recovered)/length(index))))
  
total <- cases %>% 
  left_join(deaths) %>% 
  left_join(recovered)


rm(cases, deaths, recovered)


  

# Collapse to obtain the total of Cases, Deaths and Recovered by Country

total <- total %>% 
  group_by(Country, Date) %>% 
  summarise(Cases = sum(Cases), Deaths = sum(Deaths), Recovered = sum(Recovered)) %>% 
  ungroup()
```

Read in world population data and estimate a per capita per selected
country.

``` r
library(remotes)
library(WDI)

WDIsearch('population, total')
```

    ##           indicator                name 
    ##       "SP.POP.TOTL" "Population, total"

``` r
dat = WDI(indicator='SP.POP.TOTL', start = 2019, end=2019) %>% 
  mutate(country = case_when(
    country %in% "United States" ~ "USA",
    country %in% "United Kingdom" ~ "UK",
    country %in% "Iran, Islamic Rep." ~ "Iran",
    TRUE ~ country
  )) %>% 
  janitor::clean_names() %>% 
  filter(country %in% countrylist) %>% 
  rename(Country = country) 
  

# Merge population data

total <- total %>% 
  left_join(dat)

rm(dat)

# Estimate per capita cases, deaths and recovered


capita <- total %>% 
  group_by(Country, Date) %>% 
  summarise(capitaDeaths = Deaths/sp_pop_totl,
            capitaCases = Cases/sp_pop_totl,
            capitaRecovered = Recovered/sp_pop_totl
            )

total <- total %>% 
  left_join(capita)
```

## Data distribution

Summary stats

``` r
sum_total <- total %>% 
                group_by(Country) %>% 
                summarise(across(c(Cases, Deaths, Recovered), mean)) %>% 
                rename_at(vars(-Country),function(x) paste0(x,"_mean")) %>% 
                left_join(
                total %>% 
                  group_by(Country) %>% 
                  summarise(across(c(Cases, Deaths, Recovered), sd)) %>% 
                  rename_at(vars(-Country),function(x) paste0(x,"_sd"))
                ) %>% 
                select("Country", sort(colnames(.)))

sum_total
```

    ## # A tibble: 16 x 7
    ##    Country Cases_mean Cases_sd Deaths_mean Deaths_sd Recovered_mean Recovered_sd
    ##    <chr>        <dbl>    <dbl>       <dbl>     <dbl>          <dbl>        <dbl>
    ##  1 Austra~     27741.     270.        907.      1.64         25436.        246. 
    ##  2 Brazil    5987001   707950.     166990.  11799.         5349394.     643330. 
    ##  3 China       92336.    1453.       4744       7.48         86748.        951. 
    ##  4 Cuba         7711.    1352.        131       5.79          7055.       1141. 
    ##  5 Ghana       50071.    2499.        320.      6.35         48916.       2286. 
    ##  6 India     8775374. 1017361.     129411.  12608.         8127450.    1180059. 
    ##  7 Iran       799964.  249436.      41539.   9428.          594960.     164323. 
    ##  8 Italy     1152189.  617736.      48529   12394.          577031      379684. 
    ##  9 Japan      129537.   38221.       1998     367.          109229.      29439. 
    ## 10 Mexico    1032078.  168454.      99517.  11800.          802449.      88208. 
    ## 11 Niger        1536.     486.         73       5.61          1192.         84.6
    ## 12 Peru       929375.   49986.      35190    1241.          851310.      62544. 
    ## 13 South ~    768402.   68220.      20653.   2076.          694921.      55521. 
    ## 14 Spain     1396860.  344732.      40822.   6259.          150376           0  
    ## 15 UK        1325612.  482750.      53274    8851.            3241         609. 
    ## 16 USA      11794219  3548650.     254360.  33947.         3241888.    1966886.

Per Capita Deaths

``` r
ggplot(total) +
  geom_histogram(aes(capitaDeaths),
                 alpha = 0.8,
                 bins = 50) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Make animation

### Observed cases (not standardized)

``` r
total <- total %>% 
mutate(country = case_when(
                          Country %in% "Australia" ~ "au",
                          Country %in% "Brazil" ~ "br",
                          Country %in% "China" ~ "cn",
                          Country %in% "Cuba" ~ "cu",
                          Country %in% "Ghana" ~ "gh",
                           Country %in% "India" ~ "io",
                           Country %in% "Iran" ~ "ir",
                           Country %in% "Italy" ~ "it",
                           Country %in% "Japan" ~ "jp",
                           Country %in% "Mexico" ~ "mx",
                           Country %in% "Niger" ~ "ne",
                           Country %in% "Peru" ~ "pe",
                           Country %in% "South Africa" ~ "sa",
                           Country %in% "Spain" ~ "es",
                           Country %in% "UK" ~ "gb",
                           Country %in% "USA" ~ "us"))

p <- ggplot() +
  geom_point(data = total, aes(x = Cases, y = Deaths), alpha = 0.7, size = 7) +
  geom_flag(data = total, aes(x = Cases,
                y = Deaths, 
               country = country), size = 3) +
  ggtitle("COVID Cases & Deaths in Last 100 Days")  +
  scale_color_brewer(palette = "Set3") +
  scale_size(range = c(2, 12)) +
  #scale_x_log10() +
  labs(x = "Cases (Daily Observed Number)", y = "Deaths(Daily Observed Number)") +
  geom_text(data = total, aes(label = Country, x = Cases, y = Deaths), color = "black", vjust = -1) +
  theme_classic() +
  theme(axis.text= element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 25),
        axis.title = element_text(size = 22)) +
  theme(legend.position = "top")

animation <- p + transition_time(Date) +
  labs(title = "Date: {frame_time}")

animation
```

![](README_files/figure-gfm/unnamed-chunk-14-1.gif)<!-- -->

``` r
# anim_save(animation = animation, filename = "anim.gif")
```

### Observed cases per Capita (population standardized)

``` r
p2 <- ggplot() +
  geom_flag(data = total,
            aes(x = capitaCases, y = capitaDeaths, country = country),    position = position_jitter(h=0.15,w=0.15), size = 3) +
  ggtitle("COVID Cases & Deaths in Last 100 Days")  +
  scale_color_brewer(palette = "Set3") +
  scale_size(range = c(2, 12)) +
  #scale_x_log10() +
  labs(x = "Cases (Daily Observed Number per Capita)", y = "Deaths(Daily Observed Number per Capita)") +
  geom_text(data = total,
            aes(x = capitaCases, y = capitaDeaths, label = Country),
            vjust = -1) +
  theme_classic() +
  theme(axis.text= element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 25),
        axis.title = element_text(size = 22)) +
  theme(legend.position = "top")

animation2 <- p2 + transition_time(Date) +
  labs(title = "Date: {frame_time}")

animation2 <- animate(animation2, height = 675, width = 1200)

animation2
```

![](README_files/figure-gfm/unnamed-chunk-15-1.gif)<!-- -->

``` r
#anim_save(animation = animation2, filename = "anim2.gif")
```
