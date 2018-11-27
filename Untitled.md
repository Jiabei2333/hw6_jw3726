hw6
================
Jiabei Wang
2018/11/26

problem1

``` r
city_homicides = read_csv(file = "./homicide-data.csv") %>%
  janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
city_homicides = city_homicides %>%
  mutate(city_state = str_c(city, state, sep = "_")) %>%
  filter(!(city_state == "Dallas_TX" | city_state == "Phoenix_AZ" | city_state == "Kansas City_MO" | city_state == "Tulsa_AL")) %>%
  mutate(resolved = as.numeric(disposition == "Closed by arrest"),
         victim_age = as.numeric(victim_age)) %>%
mutate(victim_race = fct_relevel(ifelse(victim_race == "White", "white", "non-white"), "white")) %>%
mutate(victim_race = fct_relevel(victim_race, "White")) 
```

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

    ## Warning: Unknown levels in `f`: White

The dataset describes the data on homicides in 50 cities in US. It contains 14 columns and 48507 rows. The variables in this data set are uid, reported\_date, victim\_last, victim\_first, victim\_race, victim\_age, victim\_sex, city, state, lat, lon, disposition, city\_state, resolved. I used glm function to fit the logistic regression.

``` r
baltimore = city_homicides %>%
  filter(city == "Baltimore")

fit_logistic = baltimore %>%
  glm(resolved ~victim_age + victim_sex + victim_race, data = ., family = binomial()) 

  fit_logistic %>%
broom::tidy() %>%
mutate(OR = exp(estimate), 
  lower_bound = exp(estimate - std.error*1.96),
         upper_bound = exp(estimate + std.error*1.96)) %>%
  select(term, log_OR = estimate, OR, lower_bound, upper_bound, p.value) %>% 
  knitr::kable(digits = 3)
```

| term                    |    log\_OR|        OR|    lower\_bound|    upper\_bound|               p.value|
|:------------------------|----------:|---------:|---------------:|---------------:|---------------------:|
| (Intercept)             |      1.186|     3.274|           2.067|           5.186|                 0.000|
| victim\_age             |     -0.007|     0.993|           0.987|           0.999|                 0.032|
| victim\_sexMale         |     -0.888|     0.412|           0.315|           0.537|                 0.000|
| victim\_racenon-white   |     -0.820|     0.441|           0.313|           0.620|                 0.000|
| The odds ratio for nonw |  hite is 0|  .441, an|  d the 95% conf|  idence interva|  l is (0.313, 0.620).|

``` r
city_data = city_homicides %>%
  group_by(city_state) %>%
  nest() 
city_data = city_data %>%  
mutate(model_result = map(data, ~glm(resolved ~ victim_age + victim_sex + victim_race, data = ., family = binomial()))) %>%
  select(-data) %>%
   mutate (model_result = map(model_result, broom::tidy)) %>%
     unnest(model_result) %>%
filter(term == "victim_racenon-white") %>% 
  mutate(OR = exp(estimate),
         lower_bound = exp(estimate - std.error*1.96),
         upper_bound = exp(estimate + std.error*1.96)) 
```

``` r
city_data%>%
  mutate(city_state = fct_reorder(city_state, OR)) %>%
ggplot(aes(x = city_state, y = OR)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound)) +  
    theme(text = element_text(size = 6), 
          axis.text.x = element_text(angle = 90),
          legend.key.size= unit(0.05,"cm")) +

    labs(title = "Estimate OR and CI Across Cities",
    x = "city_state",
    y = "Estimated OR"
  ) 
```

![](Untitled_files/figure-markdown_github/unnamed-chunk-4-1.png) From the plot, Boston, MA has the lowest OR value among all the cities, while Tempa, Florida has the highest OR. problem 2

``` r
Birthweight_data = read_csv(file = "./birthweight.csv") %>%
mutate(babysex = as.factor(babysex),
frace = as.factor(frace),
malformn = as.factor(malform),
mrace = as.factor(mrace))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   gaweeks = col_double(),
    ##   ppbmi = col_double(),
    ##   smoken = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
##Build Model
mult.fit <- lm(bwt ~ ., data=Birthweight_data)
step(mult.fit, direction='backward')
```

    ## Start:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain + malformn
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken + wtgain
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + pnumsga + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     pnumlbw + ppbmi + ppwt + smoken
    ## 
    ## 
    ## Step:  AIC=48717.83
    ## bwt ~ babysex + bhead + blength + delwt + fincome + frace + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - frace     4    124365 320848704 48712
    ## - malform   1      1419 320725757 48716
    ## - ppbmi     1      6346 320730684 48716
    ## - momage    1     28661 320752999 48716
    ## - mheight   1     66886 320791224 48717
    ## - menarche  1    111679 320836018 48717
    ## - ppwt      1    131132 320855470 48718
    ## <none>                  320724338 48718
    ## - fincome   1    193454 320917792 48718
    ## - parity    1    413584 321137922 48721
    ## - mrace     3    868321 321592659 48724
    ## - babysex   1    853796 321578134 48727
    ## - gaweeks   1   4611823 325336161 48778
    ## - smoken    1   5076393 325800732 48784
    ## - delwt     1   8008891 328733230 48823
    ## - blength   1 102050296 422774634 49915
    ## - bhead     1 106535716 427260054 49961
    ## 
    ## Step:  AIC=48711.51
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     malform + menarche + mheight + momage + mrace + parity + 
    ##     ppbmi + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - malform   1      1447 320850151 48710
    ## - ppbmi     1      6975 320855679 48710
    ## - momage    1     28379 320877083 48710
    ## - mheight   1     69502 320918206 48710
    ## - menarche  1    115708 320964411 48711
    ## - ppwt      1    133961 320982665 48711
    ## <none>                  320848704 48712
    ## - fincome   1    194405 321043108 48712
    ## - parity    1    414687 321263390 48715
    ## - babysex   1    852133 321700837 48721
    ## - gaweeks   1   4625208 325473911 48772
    ## - smoken    1   5036389 325885093 48777
    ## - delwt     1   8013099 328861802 48817
    ## - mrace     3  13540415 334389119 48885
    ## - blength   1 101995688 422844392 49908
    ## - bhead     1 106662962 427511666 49956
    ## 
    ## Step:  AIC=48709.53
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppbmi + ppwt + 
    ##     smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - ppbmi     1      6928 320857079 48708
    ## - momage    1     28660 320878811 48708
    ## - mheight   1     69320 320919470 48708
    ## - menarche  1    116027 320966177 48709
    ## - ppwt      1    133894 320984044 48709
    ## <none>                  320850151 48710
    ## - fincome   1    193784 321043934 48710
    ## - parity    1    414482 321264633 48713
    ## - babysex   1    851279 321701430 48719
    ## - gaweeks   1   4624003 325474154 48770
    ## - smoken    1   5035195 325885346 48775
    ## - delwt     1   8029079 328879230 48815
    ## - mrace     3  13553320 334403471 48883
    ## - blength   1 102009225 422859375 49906
    ## - bhead     1 106675331 427525481 49954
    ## 
    ## Step:  AIC=48707.63
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + momage + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - momage    1     29211 320886290 48706
    ## - menarche  1    117635 320974714 48707
    ## <none>                  320857079 48708
    ## - fincome   1    195199 321052278 48708
    ## - parity    1    412984 321270064 48711
    ## - babysex   1    850020 321707099 48717
    ## - mheight   1   1078673 321935752 48720
    ## - ppwt      1   2934023 323791103 48745
    ## - gaweeks   1   4621504 325478583 48768
    ## - smoken    1   5039368 325896447 48773
    ## - delwt     1   8024939 328882018 48813
    ## - mrace     3  13551444 334408523 48881
    ## - blength   1 102018559 422875638 49904
    ## - bhead     1 106821342 427678421 49953
    ## 
    ## Step:  AIC=48706.02
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     menarche + mheight + mrace + parity + ppwt + smoken
    ## 
    ##            Df Sum of Sq       RSS   AIC
    ## - menarche  1    100121 320986412 48705
    ## <none>                  320886290 48706
    ## - fincome   1    240800 321127090 48707
    ## - parity    1    431433 321317724 48710
    ## - babysex   1    841278 321727568 48715
    ## - mheight   1   1076739 321963029 48719
    ## - ppwt      1   2913653 323799943 48743
    ## - gaweeks   1   4676469 325562760 48767
    ## - smoken    1   5045104 325931394 48772
    ## - delwt     1   8000672 328886962 48811
    ## - mrace     3  14667730 335554021 48894
    ## - blength   1 101990556 422876847 49902
    ## - bhead     1 106864308 427750598 49952
    ## 
    ## Step:  AIC=48705.38
    ## bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + 
    ##     mheight + mrace + parity + ppwt + smoken
    ## 
    ##           Df Sum of Sq       RSS   AIC
    ## <none>                 320986412 48705
    ## - fincome  1    245637 321232048 48707
    ## - parity   1    422770 321409181 48709
    ## - babysex  1    846134 321832545 48715
    ## - mheight  1   1012240 321998651 48717
    ## - ppwt     1   2907049 323893461 48743
    ## - gaweeks  1   4662501 325648912 48766
    ## - smoken   1   5073849 326060260 48771
    ## - delwt    1   8137459 329123871 48812
    ## - mrace    3  14683609 335670021 48894
    ## - blength  1 102191779 423178191 49903
    ## - bhead    1 106779754 427766166 49950

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     gaweeks + mheight + mrace + parity + ppwt + smoken, data = Birthweight_data)
    ## 
    ## Coefficients:
    ## (Intercept)     babysex2        bhead      blength        delwt  
    ##   -6098.822       28.558      130.777       74.947        4.107  
    ##     fincome      gaweeks      mheight       mrace2       mrace3  
    ##       0.318       11.592        6.594     -138.792      -74.887  
    ##      mrace4       parity         ppwt       smoken  
    ##    -100.678       96.305       -2.676       -4.843

he dataset of child birth weight contains 21 columns and 4342 rows.

``` r
build_own_model = lm(bwt ~ babysex + bhead + blength + delwt +  
    gaweeks + mheight + mrace + parity + ppwt + smoken, data = Birthweight_data) 

build_own_model %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + gaweeks + 
    ##     mheight + mrace + parity + ppwt + smoken, data = Birthweight_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1106.16  -183.65    -2.51   174.67  2338.68 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6101.8188   137.5732 -44.353  < 2e-16 ***
    ## babysex2       28.3735     8.4565   3.355 0.000800 ***
    ## bhead         131.0228     3.4448  38.035  < 2e-16 ***
    ## blength        74.7933     2.0178  37.066  < 2e-16 ***
    ## delwt           4.0840     0.3920  10.419  < 2e-16 ***
    ## gaweeks        11.6785     1.4617   7.990 1.72e-15 ***
    ## mheight         6.8569     1.7795   3.853 0.000118 ***
    ## mrace2       -145.3753     9.2256 -15.758  < 2e-16 ***
    ## mrace3        -77.9781    42.2918  -1.844 0.065279 .  
    ## mrace4       -105.9871    19.1083  -5.547 3.09e-08 ***
    ## parity         94.8103    40.3386   2.350 0.018800 *  
    ## ppwt           -2.6507     0.4273  -6.204 6.02e-10 ***
    ## smoken         -4.8738     0.5855  -8.324  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272.4 on 4329 degrees of freedom
    ## Multiple R-squared:  0.7179, Adjusted R-squared:  0.7171 
    ## F-statistic:   918 on 12 and 4329 DF,  p-value: < 2.2e-16

``` r
build_own_model %>%
  broom::tidy()
```

    ## # A tibble: 13 x 5
    ##    term        estimate std.error statistic   p.value
    ##    <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept) -6102.     138.       -44.4  0.       
    ##  2 babysex2       28.4      8.46       3.36 8.00e-  4
    ##  3 bhead         131.       3.44      38.0  2.31e-273
    ##  4 blength        74.8      2.02      37.1  1.91e-261
    ##  5 delwt           4.08     0.392     10.4  4.00e- 25
    ##  6 gaweeks        11.7      1.46       7.99 1.72e- 15
    ##  7 mheight         6.86     1.78       3.85 1.18e-  4
    ##  8 mrace2       -145.       9.23     -15.8  1.93e- 54
    ##  9 mrace3        -78.0     42.3       -1.84 6.53e-  2
    ## 10 mrace4       -106.      19.1       -5.55 3.09e-  8
    ## 11 parity         94.8     40.3        2.35 1.88e-  2
    ## 12 ppwt           -2.65     0.427     -6.20 6.02e- 10
    ## 13 smoken         -4.87     0.585     -8.32 1.13e- 16

``` r
Birthweight_data %>% 
  add_residuals(build_own_model) %>% 
  add_predictions(build_own_model) %>% 
  ggplot(aes(x = pred, y = resid)) + geom_point()
```

![](Untitled_files/figure-markdown_github/unnamed-chunk-6-1.png)

I used the stepwise backward regression selection method to build my model. The variables in my model contribute significantly to the birthweight. The adjusted r square value is 0.7173, which means this model explain about 71 percentage of the data. The plot I made showed that the residuals get smaller when we have larger prediction value, while the residuals were very large when our prediction value is smaller than 1000, which is lack of accuracy

``` r
##two other models
model1 = lm(bwt~blength + gaweeks, data = Birthweight_data)

model1 %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ blength + gaweeks, data = Birthweight_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1709.6  -215.4   -11.4   208.2  4188.8 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -4347.667     97.958  -44.38   <2e-16 ***
    ## blength       128.556      1.990   64.60   <2e-16 ***
    ## gaweeks        27.047      1.718   15.74   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 333.2 on 4339 degrees of freedom
    ## Multiple R-squared:  0.5769, Adjusted R-squared:  0.5767 
    ## F-statistic:  2958 on 2 and 4339 DF,  p-value: < 2.2e-16

``` r
model1 %>%
broom::tidy()
```

    ## # A tibble: 3 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  -4348.      98.0      -44.4 0.      
    ## 2 blength        129.       1.99      64.6 0.      
    ## 3 gaweeks         27.0      1.72      15.7 2.36e-54

``` r
model2 = lm(bwt~bhead + blength + babysex + bhead*babysex*blength, data = Birthweight_data)

model2 %>%
summary()
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ bhead + blength + babysex + bhead * babysex * 
    ##     blength, data = Birthweight_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1132.99  -190.42   -10.33   178.63  2617.96 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            -7176.8170  1264.8397  -5.674 1.49e-08 ***
    ## bhead                    181.7956    38.0542   4.777 1.84e-06 ***
    ## blength                  102.1269    26.2118   3.896 9.92e-05 ***
    ## babysex2                6374.8684  1677.7669   3.800 0.000147 ***
    ## bhead:babysex2          -198.3932    51.0917  -3.883 0.000105 ***
    ## bhead:blength             -0.5536     0.7802  -0.710 0.478012    
    ## blength:babysex2        -123.7729    35.1185  -3.524 0.000429 ***
    ## bhead:blength:babysex2     3.8781     1.0566   3.670 0.000245 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 287.7 on 4334 degrees of freedom
    ## Multiple R-squared:  0.6849, Adjusted R-squared:  0.6844 
    ## F-statistic:  1346 on 7 and 4334 DF,  p-value: < 2.2e-16

``` r
model2 %>%
broom::tidy()
```

    ## # A tibble: 8 x 5
    ##   term                    estimate std.error statistic      p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>        <dbl>
    ## 1 (Intercept)            -7177.     1265.       -5.67  0.0000000149
    ## 2 bhead                    182.       38.1       4.78  0.00000184  
    ## 3 blength                  102.       26.2       3.90  0.0000992   
    ## 4 babysex2                6375.     1678.        3.80  0.000147    
    ## 5 bhead:babysex2          -198.       51.1      -3.88  0.000105    
    ## 6 bhead:blength             -0.554     0.780    -0.710 0.478       
    ## 7 blength:babysex2        -124.       35.1      -3.52  0.000429    
    ## 8 bhead:blength:babysex2     3.88      1.06      3.67  0.000245

The Rsquare of our own designed model is 0.7173, and the Rsquare for the other two is 0.5767 and 0.6844 respectively. Since Rsquare explains the percentage of explained variation, the higher is better. Thus, our fitted model is better than the other two.

``` r
set.seed(1)
cv_df = crossv_mc(Birthweight_data, 100, test =0.2) %>%
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble))
```

``` r
cv_df = 
  cv_df %>% 
  mutate(model1    = map(train, ~lm(bwt ~ babysex + bhead + blength + delwt +  gaweeks + mheight + mrace + parity + ppwt + smoken, data =  Birthweight_data)),
        model2 = map(train, ~lm(bwt~blength + gaweeks, data = Birthweight_data)), 
        model3 = map(train, ~lm(bwt~bhead + blength + babysex + bhead*babysex*blength, data = Birthweight_data))) %>% 
  mutate(rmse_1    = map2_dbl(model1, test, ~rmse(model = .x, data = .y)),
         rmse_2 = map2_dbl(model2, test, ~rmse(model = .x, data = .y)),
         rmse_3 = map2_dbl(model3, test, ~rmse(model = .x, data = .y)))
```

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin() + ggtitle("rmse for for each model ")
```

![](Untitled_files/figure-markdown_github/unnamed-chunk-10-1.png)

The violin model lies at the lowest level has the relatively lowest rmse. Since we want to minimize rmse, the first model, the one we designed, is the best.
