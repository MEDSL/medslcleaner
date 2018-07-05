Extracting election data from spreadsheets
========================================================
author: James Dunham, MIT MEDSL
date: July 3, 2018
autosize: true

The problem
========================================================

![plot of chunk unnamed-chunk-1](merrimack-long.png)

Approach
========================================================

1. Identify which cells are **data** and which are **headers**
2. Define the **relationships** between data cells and header cells

Tools:

```r
library(tidyxl)
library(unpivotr)
library(tidyverse)
library(medslcleaner)
```

Read data
====================

We read the data with function `xlsx_cells` from the `tidyxl` package.


```r
merrimack_path = spreadsheet_example('merrimack')
d = xlsx_cells(merrimack_path, sheet = 1)
```

Take a look
======


```r
d %>%
  filter(!is_blank) %>%
  select(address, row, col, data_type, character, numeric) %>%
  head()
```

```
# A tibble: 6 x 6
  address   row   col data_type character                          numeric
  <chr>   <int> <int> <chr>     <chr>                                <dbl>
1 B1          1     2 character State of New Hampshire - General …      NA
2 B2          2     2 character "Merrimack County Offices  "            NA
3 A3          3     1 date      <NA>                                    NA
4 B3          3     2 character Sheriff                                 NA
5 D3          3     4 character Attorney                                NA
6 F3          3     6 character Treasurer                               NA
```

Resources
====================

* [Spreadsheet Munging Strategies](https://nacnudus.github.io/spreadsheet-munging-strategies): <https://nacnudus.github.io/spreadsheet-munging-strategies>

