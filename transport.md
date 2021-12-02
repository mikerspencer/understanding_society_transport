Transport
================
Mike Spencer
25/11/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(haven)
library(knitr)
```

## Intro

``` r
df = read_dta("~/Cloud/personal/gofcoe/understanding_society/6614stata_B17CC6790677EF32F72CE50881AE98E1B9FC1F79133B07B63B353396D3AB917A_V1/UKDA-6614-stata/stata/stata13_se/ukhls_w10/j_indresp.dta")
```

## Transport to work variables

### Main mode of transport to work

``` r
df %>% 
  count(j_wktrvfar)
```

    ## # A tibble: 15 x 2
    ##                                             j_wktrvfar     n
    ##                                              <dbl+lbl> <int>
    ##  1 -8 [inapplicable]                                   31914
    ##  2 -7 [proxy]                                            804
    ##  3 -2 [refusal]                                            7
    ##  4 -1 [don't know]                                        18
    ##  5  1 [Drive myself by car or van]                       434
    ##  6  2 [Get a lift with someone from household]           106
    ##  7  3 [ Get a lift with someone outside the household]    58
    ##  8  4 [Motorcycle/moped/scooter]                          14
    ##  9  5 [Taxi/minicab]                                      18
    ## 10  6 [Bus/coach]                                        205
    ## 11  7 [Train]                                            411
    ## 12  8 [Underground/Metro/Tram/Light railway]             173
    ## 13  9 [Cycle]                                             58
    ## 14 10 [Walk]                                              90
    ## 15 97 [Other]                                              8

### Mode(s) of transport for journey to work

Variables:

-   wktrv1 Drive myself by car or van
-   wktrv2 Get a lift with someone from household
-   wktrv3 Get a lift with someone outside the household
-   wktrv4 Motorcycle/moped/scooter
-   wktrv5 Taxi/minicab
-   wktrv6 Bus/coach
-   wktrv7 Train
-   wktrv8 Underground/Metro/Tram/Light railway
-   wktrv9 Cycle
-   wktrv10 Walk
-   wktrv97 Other

Response options:

-   Value = 0.0 Label = Not mentioned
-   Value = 1.0 Label = Yes mentioned
-   Value = -1.0 Label = don’t know
-   Value = -9.0 Label = missing
-   Value = -8.0 Label = inapplicable
-   Value = -7.0 Label = proxy
-   Value = -2.0 Label = refusal

Employed:

``` r
df %>% 
  select(pidp, contains("j_wktrv"), -j_wktrvfar) %>% 
  pivot_longer(cols = !pidp) %>% 
  count(name, value) %>% 
  pivot_wider(names_from = name, values_from = n)
```

    ## # A tibble: 6 x 12
    ##            value j_wktrv1 j_wktrv10 j_wktrv2 j_wktrv3 j_wktrv4 j_wktrv5 j_wktrv6
    ##        <dbl+lbl>    <int>     <int>    <int>    <int>    <int>    <int>    <int>
    ## 1 -8 [inapplica…    17995     17995    17995    17995    17995    17995    17995
    ## 2 -7 [proxy]          804       804      804      804      804      804      804
    ## 3 -2 [refusal]          5         5        5        5        5        5        5
    ## 4 -1 [don't kno…       10        10       10       10       10       10       10
    ## 5  0 [Not menti…     5389     13147    14674    15097    15389    15384    14130
    ## 6  1 [Yes menti…    10115      2357      830      407      115      120     1374
    ## # … with 4 more variables: j_wktrv7 <int>, j_wktrv8 <int>, j_wktrv9 <int>,
    ## #   j_wktrv97 <int>

Self employed:

``` r
df %>% 
  select(pidp, contains("j_jswktrv"), -j_jswktrvfar) %>% 
  pivot_longer(cols = !pidp) %>% 
  count(name, value) %>% 
  pivot_wider(names_from = name, values_from = n)
```

    ## # A tibble: 6 x 12
    ##         value j_jswktrv1 j_jswktrv10 j_jswktrv2 j_jswktrv3 j_jswktrv4 j_jswktrv5
    ##     <dbl+lbl>      <int>       <int>      <int>      <int>      <int>      <int>
    ## 1 -8 [inappl…      31852       31852      31852      31852      31852      31852
    ## 2 -7 [proxy]         804         804        804        804        804        804
    ## 3 -2 [refusa…          3           3          3          3          3          3
    ## 4 -1 [don't …          4           4          4          4          4          4
    ## 5  0 [Not me…        352        1499       1622       1627       1647       1637
    ## 6  1 [Yes me…       1303         156         33         28          8         18
    ## # … with 5 more variables: j_jswktrv6 <int>, j_jswktrv7 <int>,
    ## #   j_jswktrv8 <int>, j_jswktrv9 <int>, j_jswktrv97 <int>

Larissa Pople states that I can add together travel from employed and
self employed categories.

### How many people report multiple modes?

``` r
df %>% 
  select(pidp, contains("wktrv"), -j_wktrvfar, -j_jswktrvfar) %>% 
  pivot_longer(cols = !pidp) %>% 
  mutate(name = str_replace(name, "j_js", "j_")) %>% 
  filter(value == 1) %>% 
  count(pidp, name = "modes") %>% 
  count(modes, name = "respondents")
```

    ## # A tibble: 7 x 2
    ##   modes respondents
    ##   <int>       <int>
    ## 1     1       15444
    ## 2     2        1159
    ## 3     3         440
    ## 4     4          96
    ## 5     5          17
    ## 6     6           2
    ## 7     7           1

## Income data

### Hours worked

``` r
df %>% 
  count(j_jbhrs)
```

    ## # A tibble: 181 x 2
    ##              j_jbhrs     n
    ##            <dbl+lbl> <int>
    ##  1 -8 [inapplicable] 17724
    ##  2 -2 [refusal]        117
    ##  3 -1 [don't know]     229
    ##  4  0.1                 12
    ##  5  1                   14
    ##  6  1.5                  5
    ##  7  2                   37
    ##  8  2.5                  2
    ##  9  3                   36
    ## 10  3.5                  3
    ## # … with 171 more rows

### Usual pay

``` r
df %>% 
  count(j_payu)
```

    ## # A tibble: 424 x 2
    ##    j_payu     n
    ##     <dbl> <int>
    ##  1     -8 32322
    ##  2     -7   804
    ##  3     -2    28
    ##  4     -1    92
    ##  5      0    16
    ##  6      5     1
    ##  7      6     1
    ##  8      7     2
    ##  9      8     4
    ## 10      9     3
    ## # … with 414 more rows
