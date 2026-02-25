Lab 06 - Ugly charts and Simpson’s paradox
================
Anaelle Gackiere
02-20-2026

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
library(dplyr)
library(ggplot2)
```

### Ugly Charts

### Instructional staff employment trends

``` r
staff <- read_csv("data/instructional-staff.csv")
```

    ## Rows: 5 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): faculty_type
    ## dbl (11): 1975, 1989, 1993, 1995, 1999, 2001, 2003, 2005, 2007, 2009, 2011
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# pivot to long format 
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))

# take a look
staff_long %>% head()
```

    ## # A tibble: 6 × 3
    ##   faculty_type              year  value
    ##   <chr>                     <chr> <dbl>
    ## 1 Full-Time Tenured Faculty 1975   29  
    ## 2 Full-Time Tenured Faculty 1989   27.6
    ## 3 Full-Time Tenured Faculty 1993   25  
    ## 4 Full-Time Tenured Faculty 1995   24.8
    ## 5 Full-Time Tenured Faculty 1999   21.8
    ## 6 Full-Time Tenured Faculty 2001   20.3

``` r
# plot
staff_long %>%
  ggplot(aes(x = year, y = value, color = faculty_type)) +
  geom_line()
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](lab-06_files/figure-gfm/set-up-1.png)<!-- -->

This wasn’t a good plot. Let’s change that:

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) + 
  geom_line() +
  labs(title = "Instructional Staff Employment Trends",
       y = "Proportion of Faculty Type",
       x = "year") +
  scale_x_discrete(guide = guide_axis(angle = 45))
```

![](lab-06_files/figure-gfm/good-plot-1.png)<!-- -->

If the objective of this plot was to show that the proportion of
part-time faculty have gone up over time compared to other instructional
staff types I would maybe make the year continuous, make the part-time
faculty employment bold, and add a subtitle focused on this trend. I
might also group all the other ones together so that viewers can compare
part time to other (so it really stands out).

### Fisheries

Improvement ideas:

- Show total production by country (separating capture and aquaculture)

  - either in amount of percentage

- Show by continent and not country perhaps since there are so many
  countries

  - Or show by country by flip axis

  - Use top 20 countries so it’s more readable (not all 215)

- It should be a bar chart since it’s not continuous data

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
names(fisheries)
```

    ## [1] "country"     "capture"     "aquaculture" "total"

``` r
n_distinct(fisheries$country) # many countries so cut down to 20 with most production
```

    ## [1] 215

``` r
library(tidyverse)
library(scales)

fisheries <- read_csv("data/fisheries.csv", show_col_types = FALSE)

# using slice max to keep rows with the largest values 
myDF <- fisheries %>%
  slice_max(total, n = 20, with_ties = FALSE) %>%
  pivot_longer( # turning into long format so easier to graph
    cols = c(capture, aquaculture),
    names_to = "source",
    values_to = "tons"
  ) %>%
  mutate(
    source = recode(source,
      capture = "Capture (wild)",
      aquaculture = "Aquaculture (farmed)"
    ),
    country = fct_reorder(country, tons, .fun = sum), # i'm reordering by total production
    tons_m = tons / 1e6 # changing to millions so it's readable
  )

ggplot(myDF, aes(x = country, y = tons_m, fill = source)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "M")) +
  labs(
    x = NULL,
    y = "Production (million tons, 2016)",
    title = "Top 20 fish-producing countries (capture vs aquaculture)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](lab-06_files/figure-gfm/fisheries-1.png)<!-- -->

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

There are 1314 observations on women, each of which has their age,
smoking status at baseline, and survival status.

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
  geom_histogram(fill = "plum") +
  labs(title = "Age of Women in the Whickham Data Frame",
       y = "Proportion",
       x = "Age") +
  theme_linedraw()
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-06_files/figure-gfm/display-vars-1.png)<!-- -->

``` r
# display smoker
ggplot(Whickham, aes(x = smoker, fill = smoker)) +
  geom_bar() +
  labs(title = "Smoker Status of Women in the Whickham Data Frame",
       y = "Proportion",
       x = "Smoker") +
  scale_fill_manual(values = c("Yes" = "plum", "No" = "plum4")) +
  theme_linedraw()
```

![](lab-06_files/figure-gfm/display-vars-2.png)<!-- -->

``` r
# display outcome
ggplot(Whickham, aes(x = outcome, fill = outcome)) +
  geom_bar() +
  labs(y = "Proportion",
       x = "Outcome",
       title = "Outcomes of Women in the Whickham Data Frame") +
  scale_fill_manual(values = c("Alive" = "plum", "Dead" = "plum4")) +
  theme_linedraw()
```

![](lab-06_files/figure-gfm/display-vars-3.png)<!-- -->

…

### Exercise 4

What would you expect the relationship between smoking status and health
outcome to be?

I would expect yes in the smoking status would be positively associated
with Dead in the outcome.

### Exercise 5

I did not expect to see that more women who did NOT smoke died, compared
to women who smoked.

``` r
# visual
ggplot(Whickham, aes(x = smoker, fill = outcome)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion",
       x = "Smoking Status",
       title = "Smoking Status vs. Outcomes") +
  scale_fill_manual(values = c("Alive" = "plum", "Dead" = "plum4")) +
  theme_linedraw()
```

![](lab-06_files/figure-gfm/visual-1.png)<!-- -->

``` r
# calculate proportions and make a table of it
Whickham %>%
  count(smoker, outcome) %>%
  group_by(smoker) %>%
  mutate(prop = n / sum(n))
```

    ## # A tibble: 4 × 4
    ## # Groups:   smoker [2]
    ##   smoker outcome     n  prop
    ##   <fct>  <fct>   <int> <dbl>
    ## 1 No     Alive     502 0.686
    ## 2 No     Dead      230 0.314
    ## 3 Yes    Alive     443 0.761
    ## 4 Yes    Dead      139 0.239

…

### Exercise 6

``` r
# new age variable (categories)
Whickham <- Whickham %>%
  mutate(age_cat = case_when(
    age <= 44 ~ "18-44",
    age > 44 & age <= 64 ~ "45-64",
    age > 64 ~ "65+"
  ))
# check it worked 
head(Whickham$age_cat)
```

    ## [1] "18-44" "18-44" "65+"   "65+"   "45-64" "18-44"

### Exercise 7

Now that we take age into consideration, the outcome is what I
predicted: out of women who died, there were more of them who smoked
than those who did not. As age increases, so does the proportion of
people who die, and the proportion of women who smoke and died is
greater than the proportion of women who did not smoke and died for all
age groups.

``` r
# visual 
ggplot(Whickham, aes(x = smoker, fill = outcome)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(age_cat)) +
  labs(y = "Proportion") +
  scale_fill_manual(values = c("Alive" = "plum", "Dead" = "plum4")) +
  theme_linedraw()
```

![](lab-06_files/figure-gfm/new-visual-1.png)<!-- -->

``` r
# contingency table
Whickham %>%
  count(age_cat, smoker, outcome) %>%
  group_by(age_cat, smoker) %>%
  mutate(prop = n / sum(n))
```

    ## # A tibble: 12 × 5
    ## # Groups:   age_cat, smoker [6]
    ##    age_cat smoker outcome     n   prop
    ##    <chr>   <fct>  <fct>   <int>  <dbl>
    ##  1 18-44   No     Alive     327 0.965 
    ##  2 18-44   No     Dead       12 0.0354
    ##  3 18-44   Yes    Alive     270 0.947 
    ##  4 18-44   Yes    Dead       15 0.0526
    ##  5 45-64   No     Alive     147 0.735 
    ##  6 45-64   No     Dead       53 0.265 
    ##  7 45-64   Yes    Alive     167 0.676 
    ##  8 45-64   Yes    Dead       80 0.324 
    ##  9 65+     No     Alive      28 0.145 
    ## 10 65+     No     Dead      165 0.855 
    ## 11 65+     Yes    Alive       6 0.12  
    ## 12 65+     Yes    Dead       44 0.88
