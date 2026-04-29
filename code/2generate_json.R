# Generate schedule JSON for web app
# Run this script after updating data/schedule.csv to regenerate the JSON data

library(readr)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)

# Read schedule and submissions data
schedule <- read_csv('data/schedule.csv', show_col_types = FALSE)
submissions <- read_csv('data/submissions.csv', show_col_types = FALSE)

# Join schedule with abstracts and other submission details.
# Self-organized panels (e.g. ids 32, 131) store all papers under the same id
# in both schedule and submissions, so deduplicate schedule on id first to avoid
# a many-to-many cartesian product — submissions already has one row per paper.
paper_sessions <- schedule %>%
  distinct(id, .keep_all = TRUE) %>%
  left_join(
    submissions %>%
      select(id, title, abstract, authors, category, type),
    by = 'id'
  ) %>%
  mutate(
    id = as.character(id),
    session_id = as.character(session_id),
    start_time = as.character(start_time),
    end_time = as.character(end_time)
  ) %>%
  arrange(time_order, session_id)

# Read and format panel sessions
# Panels get synthetic time_slot codes (WP1, TP1, TP2, TP3, FP1) and
# fractional time_orders so they interleave correctly with paper sessions
# (paper slots use integer time_orders 1-7).
panels <- read_csv('data/panels.csv', show_col_types = FALSE) %>%
  mutate(
    date_parsed = mdy(date),
    date = as.Date(date_parsed),
    day = case_when(
      date_parsed == as.Date("2026-06-03") ~ "Wednesday",
      date_parsed == as.Date("2026-06-04") ~ "Thursday",
      date_parsed == as.Date("2026-06-05") ~ "Friday",
      TRUE ~ format(date_parsed, "%A")
    ),
    room = str_trim(str_replace_all(room, "\\s+", " ")),
    start_time = as.character(time_start),
    end_time = as.character(time_end),
    start_secs = as.numeric(time_start),
    time_slot = case_when(
      day == "Wednesday"                                           ~ "WP1",
      day == "Thursday" & start_secs < 43200                     ~ "TP1",
      day == "Thursday" & start_secs >= 43200 & start_secs < 57600 ~ "TP2",
      day == "Thursday"                                           ~ "TP3",
      day == "Friday"                                             ~ "FP1"
    ),
    time_order = case_when(
      time_slot == "WP1" ~ 0.5,
      time_slot == "TP1" ~ 4.5,
      time_slot == "TP2" ~ 5.5,
      time_slot == "TP3" ~ 6.5,
      time_slot == "FP1" ~ 6.75
    ),
    type = if_else(id == "P1", "plenary", "special_session"),
    session_id = id,
    session_name = title,
    panelists = str_trim(panelists),
    moderator = str_trim(moderator)
  ) %>%
  select(
    id, session_id, session_name, type, category, date, day,
    start_time, end_time, time_slot, time_order, room,
    title,
    authors = panelists,
    abstract = description,
    moderator,
    panelists
  )

# Combine and write JSON
all_data <- bind_rows(paper_sessions, panels) %>%
  arrange(time_order, session_id, id)

write_json(
  all_data,
  'schedule_data.json',
  auto_unbox = TRUE,
  pretty = TRUE
)

cat(sprintf(
  "Written %d paper entries + %d panel entries to schedule_data.json\n",
  nrow(paper_sessions),
  nrow(panels)
))
