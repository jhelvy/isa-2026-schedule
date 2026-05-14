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

# Paper presentation order within each session:
#   Regular sessions   → row order in data/sessions/*.csv
#   Self-organized     → row order in data/self-sessions.csv
# This preserves the intended presentation order regardless of numeric id order.

sessions_order <- bind_rows(lapply(
  list.files('data/sessions', pattern = '\\.csv$', full.names = TRUE),
  \(f) read_csv(f, show_col_types = FALSE)
)) %>%
  group_by(session_id) %>%
  mutate(paper_order = row_number()) %>%
  ungroup() %>%
  mutate(id = as.character(id), session_id = as.character(session_id)) %>%
  select(id, session_id, paper_order)

self_sessions_raw <- read_csv('data/self-sessions.csv', show_col_types = FALSE)

# Row order within each self-organized session (matched to paper_sessions by id + title)
self_sessions_order <- self_sessions_raw %>%
  group_by(id) %>%
  mutate(paper_order = row_number()) %>%
  ungroup() %>%
  mutate(id = as.character(id)) %>%
  select(id, title, paper_order)

# Discussant: one per self-organized session (first non-empty value)
self_discussants <- self_sessions_raw %>%
  group_by(id) %>%
  summarize(
    discussant = first(discussant[!is.na(discussant) & str_trim(discussant) != ""]),
    .groups = "drop"
  ) %>%
  mutate(id = as.character(id))

# Build paper sessions and attach ordering + discussant
# Self-organized panels store all papers under the same id in schedule and
# submissions, so distinct(id) on schedule avoids a many-to-many join.
paper_sessions <- schedule %>%
  distinct(id, .keep_all = TRUE) %>%
  left_join(
    submissions %>% select(id, title, abstract, authors, category, type),
    by = 'id'
  ) %>%
  mutate(
    id = as.character(id),
    session_id = as.character(session_id),
    start_time = as.character(start_time),
    end_time = as.character(end_time)
  ) %>%
  left_join(sessions_order, by = c('id', 'session_id')) %>%
  left_join(
    self_sessions_order %>% rename(self_paper_order = paper_order),
    by = c('id', 'title')
  ) %>%
  mutate(paper_order = coalesce(paper_order, self_paper_order)) %>%
  select(-self_paper_order) %>%
  left_join(self_discussants, by = 'id') %>%
  arrange(time_order, session_id, paper_order)

# Session chair: first author of the first paper in each session
session_chairs <- paper_sessions %>%
  filter(paper_order == 1) %>%
  mutate(
    session_chair = coalesce(
      str_trim(str_extract(authors, "^.+?\\)")),
      str_trim(str_extract(authors, "^[^;,]+"))
    )
  ) %>%
  select(session_id, session_chair)

paper_sessions <- paper_sessions %>%
  left_join(session_chairs, by = 'session_id')

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
  mutate(
    session_chair = NA_character_,
    discussant    = NA_character_
  ) %>%
  select(
    id, session_id, session_name, type, category, date, day,
    start_time, end_time, time_slot, time_order, room,
    title,
    authors = panelists,
    abstract = description,
    moderator,
    panelists,
    session_chair,
    discussant
  )

# Read and format overview events (breaks, meals, receptions, etc.)
# Sessions already covered by paper schedule or panels.csv
covered_sessions <- c("Paper Sessions", "Opening Plenary", "Concurrent Panel Sessions")

# Force time columns to character so readr doesn't mis-parse "8:00 AM" as hms
events <- read_csv(
  'data/overview.csv',
  show_col_types = FALSE,
  col_types = cols(.default = "c")
) %>%
  rename_with(str_trim) %>%
  rename(
    weekday        = Weekday,
    time_start_str = `Time Start`,
    time_end_str   = `Time End`,
    session        = Session,
    building       = Building,
    room_raw       = Room
  ) %>%
  mutate(
    weekday        = str_trim(weekday),
    time_start_str = str_trim(time_start_str),
    time_end_str   = str_trim(time_end_str),
    session        = str_trim(session)
  ) %>%
  filter(!session %in% covered_sessions) %>%
  mutate(
    date = case_when(
      weekday == "Wednesday" ~ as.Date("2026-06-03"),
      weekday == "Thursday"  ~ as.Date("2026-06-04"),
      weekday == "Friday"    ~ as.Date("2026-06-05")
    ),
    day  = weekday,
    room = str_trim(paste(
      building,
      str_trim(str_replace_all(room_raw, "\\s+", " ")),
      sep = " — "
    )),
    # Parse "H:MM AM/PM" to seconds since midnight; derive HH:MM:SS strings
    start_secs = {
      t <- parse_date_time(time_start_str, orders = "I:M p", quiet = TRUE)
      hour(t) * 3600L + minute(t) * 60L
    },
    end_secs = {
      t <- parse_date_time(time_end_str, orders = "I:M p", quiet = TRUE)
      hour(t) * 3600L + minute(t) * 60L
    },
    start_time = sprintf("%02d:%02d:00", start_secs %/% 3600L, (start_secs %% 3600L) %/% 60L),
    end_time   = sprintf("%02d:%02d:00", end_secs   %/% 3600L, (end_secs   %% 3600L) %/% 60L),
    # time_order: interleaves events with paper slots (1–7) and panel slots (0.5–6.75)
    # Computed from start_secs to avoid fragile string joins
    time_order = case_when(
      weekday == "Wednesday" & start_secs == 28800L ~ 0.10,  # 8:00 AM
      weekday == "Wednesday" & start_secs == 43200L ~ 0.30,  # 12:00 PM
      weekday == "Wednesday" & start_secs == 50400L ~ 0.95,  # 2:00 PM (PDW)
      weekday == "Wednesday" & start_secs == 54600L ~ 1.50,  # 3:10 PM
      weekday == "Wednesday" & start_secs == 64800L ~ 3.50,  # 6:00 PM
      weekday == "Thursday"  & start_secs == 30000L ~ 3.80,  # 8:20 AM
      weekday == "Thursday"  & start_secs == 36600L ~ 4.25,  # 10:10 AM
      weekday == "Thursday"  & start_secs == 42000L ~ 4.75,  # 11:40 AM
      weekday == "Thursday"  & start_secs == 54300L ~ 5.75,  # 3:05 PM
      weekday == "Thursday"  & start_secs == 64800L ~ 6.55,  # 6:00 PM
      weekday == "Friday"    & start_secs == 30000L ~ 6.65,  # 8:20 AM
      weekday == "Friday"    & start_secs == 36000L ~ 6.80,  # 10:00 AM
      weekday == "Friday"    & start_secs == 42000L ~ 7.50,  # 11:40 AM
      TRUE                                           ~ 99.0
    ),
    id           = paste0("EV", row_number()),
    session_id   = id,
    session_name = session,
    type         = "event",
    time_slot    = NA_character_,
    category     = NA_character_,
    title        = session,
    authors      = NA_character_,
    abstract     = NA_character_,
    moderator     = NA_character_,
    panelists     = NA_character_,
    session_chair = NA_character_,
    discussant    = NA_character_
  ) %>%
  select(
    id, session_id, session_name, type, category, date, day,
    start_time, end_time, time_slot, time_order, room,
    title, authors, abstract, moderator, panelists, session_chair, discussant
  )

# Combine and write JSON
all_data <- bind_rows(paper_sessions, panels, events) %>%
  arrange(time_order, session_id)

write_json(
  all_data,
  'schedule_data.json',
  auto_unbox = TRUE,
  pretty = TRUE
)

cat(sprintf(
  "Written %d paper entries + %d panel entries + %d event entries to schedule_data.json\n",
  nrow(paper_sessions),
  nrow(panels),
  nrow(events)
))

# Generate bios JSON for speaker profiles
bios <- read_csv('data/bios.csv', show_col_types = FALSE)
write_json(bios, 'bios_data.json', auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("Written %d bio entries to bios_data.json\n", nrow(bios)))
