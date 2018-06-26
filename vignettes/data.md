
More details are in the R documentation for each
dataset.

## geography

| dataset            | class        | description                                                              |
| :----------------- | :----------- | :----------------------------------------------------------------------- |
| `census_precincts` | `data.frame` | Precincts, as identified in Census data as voting districts.             |
| `eavs`             | `data.frame` | Precincts, as reported in the 2016 Election Administration Voting Survey |

## candidates

| dataset      | class        | description                            |
| :----------- | :----------- | :------------------------------------- |
| `candidates` | `data.frame` | Candidate identifiers                  |
| `fec`        | `data.frame` | FEC identifiers                        |
| `members`    | `data.frame` | Current and former members of Congress |

## validation

| dataset                 | class       | description                                              |
| :---------------------- | :---------- | :------------------------------------------------------- |
| `precinct_validity`     | `list`      | Expected properties of columns in MEDSL precinct returns |
| `precinct_columns`      | `character` | Expected column names in MEDSL precinct returns          |
| `precinct_column_types` | `character` | Expected column types in MEDSL precinct returns          |
