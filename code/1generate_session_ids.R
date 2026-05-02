source(here::here("code", "setup.R"))

# This script generates data/session-ids.csv from the per-category session CSVs
# in data/sessions/ and the self-organized panel entries in data/self-sessions.csv.
# Input:  data/sessions/*.csv, data/self-sessions.csv
# Output: data/session-ids.csv

# --- Read paper sessions from data/sessions/ CSVs ---

sessions_dir <- here("data", "sessions")
csv_files <- list.files(sessions_dir, pattern = "\\.csv$", full.names = TRUE)

paper_sessions <- map(csv_files, \(f) read_csv(f, show_col_types = FALSE)) |>
  list_rbind() |>
  filter(!is.na(session_id), !is.na(session_name)) |>
  select(id, session_id, session_name)

# --- Read self-organized panel sessions from data/self-sessions.csv ---

self_sessions_raw <- read_csv(
  here("data", "self-sessions.csv"),
  show_col_types = FALSE
)

max_paper_session_id <- max(paper_sessions$session_id, na.rm = TRUE)

panel_sessions <- self_sessions_raw |>
  distinct(id, session_name) |>
  arrange(id) |>
  mutate(session_id = max_paper_session_id + row_number()) |>
  left_join(self_sessions_raw |> select(id), by = "id") |>
  select(id, session_id, session_name)

# --- Combine and write ---

session_ids <- bind_rows(paper_sessions, panel_sessions) |>
  arrange(session_id, id)

write_csv(session_ids, here("data", "session-ids.csv"))
