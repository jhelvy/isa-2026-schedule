library(readr)
library(dplyr)
library(here)

session_files <- list.files(
  here("data", "sessions"),
  pattern = "\\.csv$",
  full.names = TRUE
)

for (path in session_files) {
  df <- read_csv(path, show_col_types = FALSE)
  other_cols <- setdiff(names(df), c("session_id", "id"))
  df <- df %>%
    select(session_id, id, all_of(other_cols)) %>%
    arrange(session_id, id)
  write_csv(df, path)
  cat("Sorted:", basename(path), "\n")
}
