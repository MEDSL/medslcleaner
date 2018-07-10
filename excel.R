1 / 200 * 30

(59 + 73 + 2) / 3

sin(pi / 2)

x <- 3 * 4
x

object_name <- value

value <- 2

favorite_number <- value

seq(1, 10)

x <- seq(1, 10)

?seq

library(tidyverse)

mpg %>%
  select(manufacturer, model, displ) %>%
  head(3)

mpg %>%
  select(manufacturer, model, displ) %>%
  filter(displ > 2) %>%
  head(3)

mpg %>%
  select(manufacturer, model, displ) %>%
  filter(displ > 2) %>%
  mutate(displ_squared = displ ^ 2) %>%
  head(3)

library(tidyverse)
library(tidyxl)
library(medslcleaner)

# Get the path to the packaged example
merrimack_path <- spreadsheet_example('merrimack')
merrimack_path

# Use `read_excel` from the `readxl` package
sheet <- read_excel(merrimack_path)