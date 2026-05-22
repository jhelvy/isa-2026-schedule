library(readr)
library(dplyr)
library(here)

source(here("code", "config.R"))

# ── Remove excluded IDs from session-ids.csv ──────────────────────────────────

session_ids <- read_csv(here("data", "session-ids.csv"), show_col_types = FALSE) %>%
  mutate(id = as.character(id))

removed_from_sessions <- session_ids %>% filter(id %in% excluded_ids)
session_ids_clean <- session_ids %>% filter(!id %in% excluded_ids)

write_csv(session_ids_clean, here("data", "session-ids.csv"))

if (nrow(removed_from_sessions) > 0) {
  cat(sprintf(
    "session-ids.csv: removed %d excluded row(s): %s\n",
    nrow(removed_from_sessions),
    paste(removed_from_sessions$id, collapse = ", ")
  ))
} else {
  cat("session-ids.csv: no excluded IDs found\n")
}

# ── Remove excluded IDs from schedule.csv ─────────────────────────────────────

schedule <- read_csv(here("data", "schedule.csv"), show_col_types = FALSE) %>%
  mutate(id = as.character(id))

removed_from_schedule <- schedule %>% filter(id %in% excluded_ids)
schedule_clean <- schedule %>% filter(!id %in% excluded_ids)

write_csv(schedule_clean, here("data", "schedule.csv"))

if (nrow(removed_from_schedule) > 0) {
  cat(sprintf(
    "schedule.csv: removed %d excluded row(s): %s\n",
    nrow(removed_from_schedule),
    paste(removed_from_schedule$id, collapse = ", ")
  ))
} else {
  cat("schedule.csv: no excluded IDs found\n")
}

# ── Warn about sessions that now have fewer than 3 papers ─────────────────────

session_sizes <- schedule_clean %>%
  count(session_id, session_name, name = "n_papers")

undersized <- session_sizes %>% filter(n_papers < 3)

if (nrow(undersized) > 0) {
  cat(sprintf(
    "\nWARNING: %d session(s) now have fewer than 3 papers after exclusion:\n",
    nrow(undersized)
  ))
  for (i in seq_len(nrow(undersized))) {
    cat(sprintf(
      "  session %s ('%s'): %d paper(s)\n",
      undersized$session_id[i],
      undersized$session_name[i],
      undersized$n_papers[i]
    ))
  }
  cat("Update data/sessions/ CSVs and re-run steps 1-2 to fix.\n\n")
} else {
  cat("All sessions have 3 or more papers.\n")
}
