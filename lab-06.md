Lab 06 - Ugly charts and Simpson’s paradox
================
Anaelle Gackiere
02-20-2026

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
```

``` r
data(Whickham)

?Whickham
```

### Exercise 1

This is an observational study, because the data comes from a survey
that asks for their smoking status, age, and then the survival status is
measured 20 years after, but no manipulation of variables is present. As
the data description describes, “Data on age, smoking, and mortality
from a one-in-six survey of the electoral roll in Whickham \[…\] the
survey was conducted in 1972-1974 to study heart disease and thyroid
disease. A follow-up on those in the survey was conducted twenty years
later.”

### Exercise 2

How many observations are in this dataset? What does each observation
represent?

There are 1314 observations on women, each of which represents their
age, smoking status at baseline, and survival status.

### Exercise 3

How many variables are in this dataset? What type of variable is each?
Display each variable using an appropriate visualization.

There are three variables in this data set (age, smoker, outcome). The
outcome variable is a factor with two levels: Alive and Dead. The smoker
variable is a factor with two levels: No and Yes. The age variable is a
continuous one, and it is classified as an integer.

``` r
# class check
class(Whickham$age)
```

    ## [1] "integer"

``` r
class(Whickham$outcome)
```

    ## [1] "factor"

``` r
class(Whickham$smoker)
```

    ## [1] "factor"

``` r
# display age
ggplot(Whickham, aes(age)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-06_files/figure-gfm/display-vars-1.png)<!-- -->

``` r
# display smoker
ggplot(Whickham, aes(smoker)) +
  geom_bar()
```

![](lab-06_files/figure-gfm/display-vars-2.png)<!-- -->

``` r
# display outcome
ggplot(Whickham, aes(outcome)) +
  geom_bar()
```

![](lab-06_files/figure-gfm/display-vars-3.png)<!-- -->

…

### Exercise 4

What would you expect the relationship between smoking status and health
outcome to be?

I would expect that smoking status and the health outcome have YEs as
smoking status would be positively associated with Dead in the outcome.

### Exercise 5

Create a visualization depicting the relationship between smoking status
and health outcome. Briefly describe the relationship, and evaluate
whether this meets your expectations. Additionally, calculate the
relevant conditional probabilities to help your narrative. Here is some
code to get you started:

``` r
Whickham %>%
  count(smoker, outcome)
```

    ##   smoker outcome   n
    ## 1     No   Alive 502
    ## 2     No    Dead 230
    ## 3    Yes   Alive 443
    ## 4    Yes    Dead 139

``` r
# visual
ggplot(Whickham, aes(smoker,outcome)) +
  geom_count() +
  theme_dark()
```

![](lab-06_files/figure-gfm/visual-1.png)<!-- -->

``` r
# calculate probabilities
```

…

### Exercise 6

``` r
# new age variable (categories)
age_cat <- Whickham %>%
  mutate(age_cat = case_when(
    age <= 44 ~ "18-44",
    age > 44 & age <= 64 ~ "45-64",
    age > 64 ~ "65+"
  ))

# check it worked 
head(age_cat)
```

    ##   outcome smoker age age_cat
    ## 1   Alive    Yes  23   18-44
    ## 2   Alive    Yes  18   18-44
    ## 3    Dead    Yes  71     65+
    ## 4   Alive     No  67     65+
    ## 5   Alive     No  64   45-64
    ## 6   Alive    Yes  38   18-44

### Exercise 7

Re-create the visualization depicting the relationship between smoking
status and health outcome, faceted by age_cat.

``` r
# Whickham %>%
  # count(smoker, age_cat, outcome)
```
