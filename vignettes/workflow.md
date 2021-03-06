Workflow
================

## Overview

This is a typical workflow for processing election returns:

1.  Read from the disk returns collected from states or administrative
    jurisdictions in assorted format
2.  Shuffle the data around to match our schema
3.  Standardize certain values that refer to candidate names, federal or
    state legislative office, or party
4.  Assign observations to one of our releases (e.g., the [U.S. House
    dataverse](https://dataverse.harvard.edu/dataverse/medsl_house))
5.  Add state, county, and candidate identifiers
6.  Check for data quality issues
7.  Save the result

The sections that follow describe package functionality for each step
and offer some guidance.

### Read returns

#### CSV

Column *type* problems often arise when reading returns from delimited
text (e.g., CSV) files. We also may encounter column names that are
duplicated, missing, or contain special characters. Base R’s `read.csv`
doesn’t handle these situations as safely as `fread` from `data.table`
and `read_csv` from `readr`.

`fread` and `read_csv` produce warnings on parser errors that we can
resolve by setting column types explicitly via the `colClasses` argument
for `fread` or `col_types` for `read_*`. Other than the two functions’
return types, `data.table` and `tibble`, the most relevant difference in
implementation for us is that the `read_` family infers column types
from the first `guess_max` rows, whereas `fread` uses a sample.

We’d ideally read returns from a delimited-text format only once, and
for any intermediate writes to the disk during processing use a binary
format that preserves types, e.g. an R data format. But we haven’t
always done this, and in the 2016 returns there are a lot of
intermediate CSVs.

Read these intermediate CSVs with the `read_legacy_csv` function. It
promotes parser warnings to errors, and can read expected columns with
the appropriate types if `use_types = TRUE`.

#### Excel

We deal with quite a few Excel files. Some can be handled with the
[readxl](http://readxl.tidyverse.org) package. It works well when
spreadsheets already look like sane tabular data, with a single header
row followed by observations whose identifiers are in columns.

This isn’t always so. Consider an excerpt:

![Excel Example](excel-example.png)

The top of the sheet has three headers, which give:

  - Elected office (“United States President,” more values to the right
    not shown) in rows `1:3`
  - Candidate party (“IND”, “DEM”, …) in row `4`
  - Candidate name (“Darrell L. Castle”, “Hillary Rodham Clinton”, …) in
    row `5`

But there’s also an interstitial header spanning `A6:I6` (“ADA”). This
is a jurisdiction, and describes all the rows below it until the next
such header. (Which happens to be “ADA (Continued)” in `A31:I31`). These
headers appear about every 25 rows. Otherwise, `A` gives precinct
numbers.

At a high level, we want to do the following:

1.  Associate each `office`, `party` and `candidate` in `B:I` with the
    votes in its column;
2.  Associate each `jurisdiction` in `A` with all the votes below it,
    until the next interstitial header;
3.  Associate each `precinct` in `A` with all the votes in its row;
4.  Avoid including total rows (not shown above).

The desired result would start like
this:

``` r
                    office party              candidate jurisdiction precinct votes
1: UNITED STATES PRESIDENT   IND      Darrell L. Castle          ADA     1401     4
2: UNITED STATES PRESIDENT   DEM Hillary Rodham Clinton          ADA     1401   206
3: UNITED STATES PRESIDENT   CON         Scott Copeland          ADA     1401     0
4: UNITED STATES PRESIDENT   IND     Rocky De La Fuente          ADA     1401     0
5: UNITED STATES PRESIDENT   LIB           Gary Johnson          ADA     1401    35
6: UNITED STATES PRESIDENT   IND          Evan McMullin          ADA     1401    63
```

The package includes functionality for arriving at such a result with
reasonable speed and safely. See the vignette on Excel-file processing
for more details.

### Shuffle data

Each row in released precinct data gives the `votes` cast via some
`mode` (e.g., by mail) for a `candidate` running for an `office` either
on a `party` line or as a `writein`, in a particular `precinct` within
an election `jurisdiction`. So, observations should be unique by
`jurisdiction`, `precinct`, `candidate`, `office`, `party`, `writein`
and `mode`.

We wrangle the raw data into these columns. For most states this
requires at minimum splitting a single raw column between `jurisdiction`
and `precinct`, and another between `office` and `district`.

For readable and maintainable code, we recommend using the `stringr`
package for this, in combination with either `dplyr` or `data.table`.
There is some syntactic sugar in the package for text-value wrangling
with regular expressions.

These infix operators are vectorized over a left-hand-side character
vector and apply a pattern from their right-hand side (ignoring case).
They correspond with functions in `stringr`:

| function | stringr verb |
| :------: | :----------- |
|  `%=%`   | detect       |
|  `%-%`   | remove\_all  |
|  `%e%`   | extract      |

These functions operate on specified columns of a `data.table`:

| function | operation           |
| :------: | :------------------ |
|  `move`  | extract then remove |

### Standardize values

We standardize certain values of `office` and `candidate`. For example,
in MEDSL data, all presidential candidates run for the `office` of `US
President`; to achieve this consistency we replace alternative
references to the presidency.

#### office

The standardized values of `office` are `US President`, `US Senate`, `US
House`, `State Senate`, `State House`, and `Governor`. Other values are
left as they appear in the raw data. In the future, this might change.
See the `normalize_office` function.

``` r
library(medslcleaner)
d = data.frame(
  office = 'President and Vice President of the United States',
  candidate = 'Write-ins')
normalize_office(d)
##          office candidate
## 1: US President Write-ins
```

#### candidate

We standardize presidential candidate names. Typically this is a
one-liner using `normalize_presidential_candidates`, which searches for
known candidate last names where `office` is `US President` and assigns
the standardized form.

Generally, we want to standardize candidates’ names across all the
jurisdictions in which they appear on the ballot. In practice, candidate
names appear consistently at least within jurisdictions, and usually
within states.

``` r
d = data.frame(
  office = c('US President'),
  candidate = c('TRUMP', 'CLINTON'))
normalize_presidential_candidates(d)
##          office       candidate
## 1: US President    Donald Trump
## 2: US President Hillary Clinton
```

#### party

We standardize the names of parties where it seems useful. Exactly how
could change in the future, but we use `expand_party_abbr()`.

``` r
d = data.frame(party = c('DEM', 'REP'))
expand_party_abbr(d$party)
## [1] "democratic" "republican"
```

### Assign dataverses

The `dataverse` variable takes one of these values: `president`,
`senate`, `house`, `state`, `local`, and `all`. These values determine
the dataverse into which we’ll release the data.

To assign standardized offices to their dataverses use `assign_fixed`.
If `office` is `US President`, `dataverse` will be set to `president`,
and so forth for `US Senate`, `US House`, `State Senate`, and `State
House`.

Then use the `dvna` function to show the offices not yet assigned to a
dataverse. The output also gives the number of jurisdictions in which
they appeared on the ballot. Where it’s unclear whether an office is
state or local, this can be a useful heuristic.

The `ogrep` function is similar but only shows offices that match a
(regular-expression, case-insensitive) `pattern`. Develop an `ogrep`
`pattern` that matches the desired offices, then use this pattern with
the `assign_match` function to assign all rows with matching `office`
values to a given `dataverse`.

We iterate over these steps until all offices have been assigned to
dataverses, and `dvna` no longer produces an error.

First assign standardized
offices:

``` r
d = data.frame(office = c('US House', 'County Commissioner'), jurisdiction =
  c('Appleton', 'Racine'))
d
##                office jurisdiction
## 1            US House     Appleton
## 2 County Commissioner       Racine

d = assign_fixed(d)
d
##                 office jurisdiction dataverse
## 1:            US House     Appleton     house
## 2: County Commissioner       Racine      <NA>
```

`dvna` would now show unassigned offices and stop. To assign them to
dataverses:

``` r
# We test out a pattern that matches some number of the `office` values
ogrep(d, 'County')
##                 office n_j   dv
## 1: County Commissioner   1 <NA>

# Then assign matching rows a given `dataverse` value, in this case `local`
d = assign_match(d, 'County', 'local')
d
##                 office jurisdiction dataverse
## 1:            US House     Appleton     house
## 2: County Commissioner       Racine     local

# `dvna` no longer produces an error because all rows have a `dataverse`;
# Leave it in the script here, as confirmation this remains true
dvna(d)
## Empty data.table (0 rows) of 1 col: office
## [1] TRUE
```

### Add geo IDs

Before saving the result, we attach state and county identifiers from
package datasets. In most states, `jurisdiction` values are county
names, and in some, municipalities (notable exceptions are Alaska and
DC). We map `jurisdiction` values to county or county-equivalent FIPS
codes using the EAVS dataset (primarily), the tabular data included with
the 2018 Census Partnership Shapefiles of voting districts, and 2017
Census gazetteers.

The function `add_jurisdiction_fips()` will try to match jurisdictions
by name with EAVS data, and if all jurisdictions can be found in the
EAVS data, will merge in the corresponding county FIPS codes. In some
states, more work is required; see for example the scripts for Vermont
or Wisconsin.

Given county FIPS codes, we use `add_geo_ids` to join other county
identifiers from the `county_ids` dataset in the `elections` package.

### Add candidate IDs

The `medslcleaner` package includes a `candidates` dataset with various
identifiers for (at this point only) federal candidates, pulled from the
FEC and the @unitedstates project. Linking ballot names to these
datasets is already done, so merging in the identifiers only requires a
call to `add_candidate_ids`.

If this fails, then the `candidates` dataset needs to be rebuilt to
include new candidates; see the `data-ext/candidates` directory. In the
future this should be more robust.

### Save the result

We use `write_precincts` to write the final result to disk. The
important changes that `write_precincts` makes before writing to disk
are to keep only the expected columns and normalize whitespace in
character columns. If certain columns with default values are missing
(e.g., `stage = "gen"`), they’ll be created.
