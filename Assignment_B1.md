STAT545 Assignment B1
================

## Writing, Using, and Testing Functions

``` r
#Load these libraries to run the code contained in this document

library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.1

    ## Warning: package 'tidyr' was built under R version 4.3.1

    ## Warning: package 'readr' was built under R version 4.3.1

    ## Warning: package 'purrr' was built under R version 4.3.1

    ## Warning: package 'dplyr' was built under R version 4.3.1

    ## Warning: package 'stringr' was built under R version 4.3.1

    ## Warning: package 'forcats' was built under R version 4.3.1

    ## Warning: package 'lubridate' was built under R version 4.3.1

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(dplyr)
library(palmerpenguins)
```

    ## Warning: package 'palmerpenguins' was built under R version 4.3.1

``` r
library(datateachr)
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.3.1

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

### Writing a Function

While exploring the *vancouver_trees* dataset from the **datateachr**
package to complete MDA 1&2 for STAT545A, I found myself repeatedly
using several code chunks. One of these was a group_by() %\>%
summarise() workflow that I used to calculate the number of observations
for a given group or groups of variables within the dataset. I decided
to write a function that would bundle this workflow and include the
option of filtering by a specified condition, so that the function will
have multiple uses - counting the number of observations based on one or
more grouping variables, or the number of observations that meet a
specified condition, or the number of observations based on one or more
grouping variables that meet a specified condition.

``` r
#Function count_by_groups_filter_by_condition()

count_by_groups_filter_by_condition <- function(df, groups, cond) { 
  df %>% 
    group_by(pick({{ groups }})) %>% 
    filter({{ cond }}) %>% 
    summarise(n=n())
}

#' Count observations by groups and/or those which meet a condition
#' 
#' This is a function that summarises the number of observations for one or more variables within a data frame. It can be used to compute the number of  observations of one or more grouped variables, the number of observations that meet a specified condition, or a combination of the two.
#'
#'@param df The first argument is named 'df' as a shorthand for data frame. This is a required argument.
#'
#'@param groups This argument is named 'groups' to refer to the grouping variable(s) that will be input to the group_by function. It must be an object or use the form c(x,y) if multiple variables are used for grouping. It is not required.
#'
#'@param cond This argument is named 'cond' as a shorthand for condition, to refer to the condition that will be specified in the filter function. This argument must be a logical vector (eg sex=="female"). It is not required.
#'
#'@return An object of the same type as `.df`containing the grouped variables and the corresponding number of observations (n) for each row; if no grouping was  applied and only a filtering condition was used, the output will be a 1x1 object containing the number of observations (n) that meet the condition.
#'
#'  The output has the following properties:
#' * Each group is summarised down to one row.
#' * Columns are not modified; column "n" for observations is created.
#' * Data frame attributes are preserved.
#'
#'#'@examples Below are three examples of how to use this function using datasets contained in the **datateachr** and **penguins** packages.
```

### Examples

It is good practice to check whether a new function is working as
intended by running some examples.

``` r
#Example 1: Vancouver Trees

#In the first example, I will demonstrate how to use the function to group by multiple variables and filter by a condition in the vancouver_trees dataset. I will group by neighbourhood name and height range and specify the condition to be date planted > 2000-01-01 to find the number of trees in each neighbourhood for each height range that were planted after 2000-01-01.

count_by_groups_filter_by_condition(df=vancouver_trees, c(neighbourhood_name, genus_name), cond= date_planted >"2000-01-01")
```

    ## `summarise()` has grouped output by 'neighbourhood_name'. You can override
    ## using the `.groups` argument.

    ## # A tibble: 906 × 3
    ## # Groups:   neighbourhood_name [22]
    ##    neighbourhood_name genus_name         n
    ##    <chr>              <chr>          <int>
    ##  1 ARBUTUS-RIDGE      ACER             357
    ##  2 ARBUTUS-RIDGE      AESCULUS           9
    ##  3 ARBUTUS-RIDGE      AMELANCHIER       14
    ##  4 ARBUTUS-RIDGE      CALOCEDRUS         1
    ##  5 ARBUTUS-RIDGE      CARPINUS         128
    ##  6 ARBUTUS-RIDGE      CEDRUS             4
    ##  7 ARBUTUS-RIDGE      CELTIS             4
    ##  8 ARBUTUS-RIDGE      CERCIDIPHYLLUM    63
    ##  9 ARBUTUS-RIDGE      CORNUS            25
    ## 10 ARBUTUS-RIDGE      CRATAEGUS        105
    ## # ℹ 896 more rows

``` r
#Example 2: Palmer Penguins

#In this example, I will just use the grouping part of the function and I won't include a condition argument to demonstrate that the function can work just by grouping. I will group by island and species and the output should provide the number of observations for each island/species group.

count_by_groups_filter_by_condition(df=penguins, c(island, species))
```

    ## `summarise()` has grouped output by 'island'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 3
    ## # Groups:   island [3]
    ##   island    species       n
    ##   <fct>     <fct>     <int>
    ## 1 Biscoe    Adelie       44
    ## 2 Biscoe    Gentoo      124
    ## 3 Dream     Adelie       56
    ## 4 Dream     Chinstrap    68
    ## 5 Torgersen Adelie       52

``` r
#Example 3: Palmer Penguins

#In the last example, I will specify a condition and no groups. I will set the condition to select only female observations, and the output should be a 1x1 dataframe with n = number of female observations.

count_by_groups_filter_by_condition(df=penguins, cond=sex=="female")
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   165

### Testing the Function

Next I will write formal tests for my function to ensure that it will
give errors when I expect. I will use functions from the **testthat**
package.

``` r
#Test 1: Does the function give an error when the input is numeric?

test_that("numeric vector causes error", {
  expect_error(count_by_groups_filter_by_condition(15, groups=8))
  })
```

    ## Test passed 🥳

``` r
#Test 2: Does the function give an error if no data frame is specified?

test_that("no data frame argument causes error",{
  expect_error(count_by_groups_filter_by_condition(groups=species))
  })
```

    ## Test passed 🥳

``` r
#Test 3: Does the function give an error if the condition is not a logical vector?

test_that("condition as object causes error",{
  expect_error(count_by_groups_filter_by_condition(penguins, c(species, sex), cond=island))
  })
```

    ## Test passed 😀
