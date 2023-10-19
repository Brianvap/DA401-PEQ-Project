analysis_10_19-23
================
Brian Vap
2023-10-19

## Some libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.1.8
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(dplyr)
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(ggplot2)
library(ggmap)
```

    ## ℹ Google's Terms of Service: ]8;;https://mapsplatform.google.com<https://mapsplatform.google.com>]8;;
    ## ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.

## Importing and some data filtering

``` r
#Importing data
pace_data <- read_csv("pace_data.csv")
```

    ## Rows: 110 Columns: 28
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (17): Stage, ID, Asset Name, Maturity Date, as-is Property Value, as-co...
    ## dbl   (8): Coupon, PACE Asset Balance, Term (Years), Payments per Year, Curr...
    ## date  (3): Closing Date, Amortization Start Date, Cap-I End Date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
uscities <- read_csv("uscities.csv")
```

    ## Rows: 109000 Columns: 42
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (12): city, city_ascii, city_alt, state_id, state_name, county_fips, cou...
    ## dbl (27): lat, lng, population, population_proper, density, ranking, id, age...
    ## lgl  (3): military, incorporated, cdp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#Removing columns not needed from US cities dataset
uscities <- uscities %>% select(-county_fips_all, -county_name_all, -cdp, -zips)

#Filtering uscities dataset by population - don't want any super duper tiny towns
uscities <- uscities[uscities$population >= 10000, ]
```

## Descriptive Stats on both datasets

``` r
#Descriptive Stats for pace_data
summary(pace_data)
```

    ##     Stage                ID             Asset Name            Coupon    
    ##  Length:110         Length:110         Length:110         Min.   :5.12  
    ##  Class :character   Class :character   Class :character   1st Qu.:5.70  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :6.05  
    ##                                                           Mean   :6.29  
    ##                                                           3rd Qu.:6.78  
    ##                                                           Max.   :8.31  
    ##  PACE Asset Balance  Closing Date        Maturity Date       Term (Years)  
    ##  Min.   :  134758   Min.   :2014-08-28   Length:110         Min.   :10.00  
    ##  1st Qu.: 1371688   1st Qu.:2019-06-13   Class :character   1st Qu.:20.00  
    ##  Median : 2669166   Median :2021-03-19   Mode  :character   Median :25.00  
    ##  Mean   : 4003502   Mean   :2020-07-31                      Mean   :23.68  
    ##  3rd Qu.: 5049716   3rd Qu.:2022-08-17                      3rd Qu.:25.00  
    ##  Max.   :39873850   Max.   :2023-07-31                      Max.   :30.00  
    ##  Amortization Start Date Cap-I End Date       Payments per Year
    ##  Min.   :2001-02-18      Min.   :2014-12-31   Min.   :1.000    
    ##  1st Qu.:2020-05-03      1st Qu.:2020-05-02   1st Qu.:1.000    
    ##  Median :2022-02-09      Median :2022-05-26   Median :2.000    
    ##  Mean   :2021-06-13      Mean   :2021-08-27   Mean   :1.709    
    ##  3rd Qu.:2023-11-15      3rd Qu.:2023-11-14   3rd Qu.:2.000    
    ##  Max.   :2026-01-01      Max.   :2025-12-31   Max.   :2.000    
    ##  Current Total Debt MTG + PACE Mortgage Balance    Total Project Budget
    ##  Min.   :   151127             Min.   :        0   Min.   :   151127   
    ##  1st Qu.:  5671668             1st Qu.:  4147303   1st Qu.:  7529331   
    ##  Median : 13074906             Median : 10327600   Median : 19672952   
    ##  Mean   : 19265730             Mean   : 15262228   Mean   : 25664618   
    ##  3rd Qu.: 25140093             3rd Qu.: 19750000   3rd Qu.: 32011597   
    ##  Max.   :154773850             Max.   :114900000   Max.   :184160475   
    ##  as-is Property Value as-complete Property Value as-stabilized Property value
    ##  Length:110           Length:110                 Length:110                  
    ##  Class :character     Class :character           Class :character            
    ##  Mode  :character     Mode  :character           Mode  :character            
    ##                                                                              
    ##                                                                              
    ##                                                                              
    ##  as-stabilized PACE LTV as-stabilized Mortgage LTV as-stabilized Combined LTV
    ##  Length:110             Length:110                 Length:110                
    ##  Class :character       Class :character           Class :character          
    ##  Mode  :character       Mode  :character           Mode  :character          
    ##                                                                              
    ##                                                                              
    ##                                                                              
    ##  Under-Written DSVR     City              State           Property Type     
    ##  Length:110         Length:110         Length:110         Length:110        
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Project Type       PACE Program        Closing Year  Maturity Year     
    ##  Length:110         Length:110         Min.   :2014   Length:110        
    ##  Class :character   Class :character   1st Qu.:2019   Class :character  
    ##  Mode  :character   Mode  :character   Median :2021   Mode  :character  
    ##                                        Mean   :2020                     
    ##                                        3rd Qu.:2022                     
    ##                                        Max.   :2023

``` r
#Descriptive Stats for uscities
summary(uscities)
```

    ##      city            city_ascii          city_alt           state_id        
    ##  Length:79033       Length:79033       Length:79033       Length:79033      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   state_name        county_fips        county_name             lat       
    ##  Length:79033       Length:79033       Length:79033       Min.   :17.97  
    ##  Class :character   Class :character   Class :character   1st Qu.:34.10  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :39.60  
    ##                                                           Mean   :37.97  
    ##                                                           3rd Qu.:41.61  
    ##                                                           Max.   :64.87  
    ##                                                           NA's   :74240  
    ##       lng            population       population_proper    density       
    ##  Min.   :-159.35   Min.   :   10002   Min.   :   4320   Min.   :    4.6  
    ##  1st Qu.: -97.14   1st Qu.:   14099   1st Qu.:  14065   1st Qu.:  460.8  
    ##  Median : -84.55   Median :   21723   Median :  21589   Median :  807.3  
    ##  Mean   : -90.09   Mean   :   78870   Mean   :  48315   Mean   : 1141.4  
    ##  3rd Qu.: -76.79   3rd Qu.:   42123   3rd Qu.:  39741   3rd Qu.: 1405.3  
    ##  Max.   : -65.66   Max.   :18972871   Max.   :8736047   Max.   :28653.9  
    ##  NA's   :74240     NA's   :74240      NA's   :74240     NA's   :74240    
    ##     source           military       incorporated      timezone        
    ##  Length:79033       Mode :logical   Mode :logical   Length:79033      
    ##  Class :character   FALSE:4781      FALSE:1585      Class :character  
    ##  Mode  :character   TRUE :12        TRUE :3208      Mode  :character  
    ##                     NA's :74240     NA's :74240                       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##     ranking            id              age_median         male      
    ##  Min.   :1.00    Min.   :1.630e+09   Min.   :14.20   Min.   :37.60  
    ##  1st Qu.:3.00    1st Qu.:1.840e+09   1st Qu.:35.30   1st Qu.:47.80  
    ##  Median :3.00    Median :1.840e+09   Median :38.80   Median :49.00  
    ##  Mean   :2.77    Mean   :1.839e+09   Mean   :38.95   Mean   :49.15  
    ##  3rd Qu.:3.00    3rd Qu.:1.840e+09   3rd Qu.:42.30   3rd Qu.:50.30  
    ##  Max.   :3.00    Max.   :1.840e+09   Max.   :74.70   Max.   :76.40  
    ##  NA's   :74240   NA's   :74240       NA's   :74245   NA's   :74276  
    ##      female         married       family_size    income_household_median
    ##  Min.   :23.60   Min.   : 4.2    Min.   :2.03    Min.   : 10939         
    ##  1st Qu.:49.70   1st Qu.:42.2    1st Qu.:3.00    1st Qu.: 54660         
    ##  Median :51.00   Median :49.0    Median :3.16    Median : 74064         
    ##  Mean   :50.85   Mean   :48.7    Mean   :3.21    Mean   : 81119         
    ##  3rd Qu.:52.20   3rd Qu.:55.9    3rd Qu.:3.36    3rd Qu.: 99277         
    ##  Max.   :62.40   Max.   :81.0    Max.   :6.04    Max.   :250001         
    ##  NA's   :74276   NA's   :74276   NA's   :74245   NA's   :74245          
    ##  income_household_six_figure home_ownership    home_value       rent_median   
    ##  Min.   : 1.00               Min.   : 0.0    Min.   :  36012   Min.   : 248   
    ##  1st Qu.:22.10               1st Qu.:55.8    1st Qu.: 166467   1st Qu.: 940   
    ##  Median :34.70               Median :66.5    Median : 256603   Median :1219   
    ##  Mean   :36.73               Mean   :66.1    Mean   : 318539   Mean   :1314   
    ##  3rd Qu.:49.60               3rd Qu.:77.9    3rd Qu.: 389445   3rd Qu.:1589   
    ##  Max.   :89.20               Max.   :98.0    Max.   :2000001   Max.   :3501   
    ##  NA's   :74245               NA's   :74245   NA's   :74266     NA's   :74250  
    ##  education_college_or_above labor_force_participation unemployment_rate
    ##  Min.   : 1.8               Min.   :11.60             Min.   : 0.50    
    ##  1st Qu.:22.5               1st Qu.:60.70             1st Qu.: 3.70    
    ##  Median :33.1               Median :65.30             Median : 4.90    
    ##  Mean   :36.2               Mean   :64.33             Mean   : 5.45    
    ##  3rd Qu.:47.4               3rd Qu.:69.10             3rd Qu.: 6.70    
    ##  Max.   :93.6               Max.   :89.00             Max.   :29.20    
    ##  NA's   :74276              NA's   :74245             NA's   :74245    
    ##    race_white      race_black      race_asian     race_native   
    ##  Min.   : 1.70   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
    ##  1st Qu.:59.50   1st Qu.: 1.80   1st Qu.: 1.30   1st Qu.: 0.10  
    ##  Median :75.80   Median : 4.80   Median : 2.70   Median : 0.20  
    ##  Mean   :70.39   Mean   :11.31   Mean   : 5.74   Mean   : 0.55  
    ##  3rd Qu.:86.20   3rd Qu.:13.30   3rd Qu.: 6.30   3rd Qu.: 0.60  
    ##  Max.   :99.30   Max.   :96.20   Max.   :69.40   Max.   :47.20  
    ##  NA's   :74276   NA's   :74276   NA's   :74276   NA's   :74276  
    ##   race_pacific     race_other    race_multiple  
    ##  Min.   : 0.00   Min.   : 0.0    Min.   : 0.10  
    ##  1st Qu.: 0.00   1st Qu.: 1.0    1st Qu.: 3.80  
    ##  Median : 0.00   Median : 2.4    Median : 5.80  
    ##  Mean   : 0.22   Mean   : 4.9    Mean   : 6.88  
    ##  3rd Qu.: 0.10   3rd Qu.: 5.6    3rd Qu.: 8.80  
    ##  Max.   :56.40   Max.   :75.8    Max.   :38.60  
    ##  NA's   :74276   NA's   :74276   NA's   :74276

## Pace_data correlation matrix

``` r
numeric_pace <- pace_data[sapply(pace_data, is.numeric)]
correlation_matrix1 <- cor(numeric_pace)
corrplot(correlation_matrix1)
```

![](analysis_10_19_23_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## uscities correlation matrix

``` r
#uscities correlation matrix
numeric_cities <- uscities[sapply(uscities, is.numeric)]
correlation_matrix <- cor(numeric_cities, use="pairwise.complete.obs")
corrplot(correlation_matrix)
```

![](analysis_10_19_23_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Distribution of Total Amount Financed in pace_data - dep variable

``` r
ggplot(pace_data, aes(x=`PACE Asset Balance`)) + geom_histogram(fill="lightblue", color="black") + ggtitle("Distribution of Total Amount Financed")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](analysis_10_19_23_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Distribution of population in US cities, omitting all citiest below 10000 pop - most are tiny

``` r
ggplot(uscities, aes(x=population)) + geom_histogram(fill="lightblue", color="black") + ggtitle("Distribution of Population")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](analysis_10_19_23_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Visualisation of cities in the uscities dataset

``` r
register_google(key = "AIzaSyA34GsIvCy6603uh_3P4-yFObbiweCegHI")

us_map <- get_map(location = "United States", zoom = 4)
```

    ## ℹ <]8;;https://maps.googleapis.com/maps/api/staticmap?center=United%20States&zoom=4&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx-yFObbiweCegHIhttps://maps.googleapis.com/maps/api/staticmap?center=United%20States&zoom=4&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx-yFObbiweCegHI]8;;>

    ## ℹ <]8;;https://maps.googleapis.com/maps/api/geocode/json?address=United+States&key=xxx-yFObbiweCegHIhttps://maps.googleapis.com/maps/api/geocode/json?address=United+States&key=xxx-yFObbiweCegHI]8;;>

``` r
ggmap(us_map) + 
  geom_point(data = uscities, aes(x = lng, y = lat), color = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Locations on US Map")
```

![](analysis_10_19_23_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
