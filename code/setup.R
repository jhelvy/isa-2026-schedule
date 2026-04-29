library(tidyverse)
library(here)
options(dplyr.width = Inf)

submissions <- read_csv(
  here("data", "submissions.csv"),
  show_col_types = FALSE
)
