library(testthat)

source(here::here("code", "setup.R"))
source(here::here("code", "config.R"))

schedule <- read_csv(
  here("data", "schedule.csv"),
  show_col_types = FALSE
) %>%
  mutate(id = as.character(id), session_id = as.character(session_id))
submissions <- read_csv(
  here("data", "submissions.csv"),
  show_col_types = FALSE
)

# IDs that should never appear in the schedule
excluded_ids <- as.character(c(
  reject_ids,
  withdraw_ids,
  dissertation_ids,
  panel_discussion_ids
))

# IDs that should appear in the schedule (papers + self-organized panels)
expected_ids <- as.character(
  submissions %>% filter(!id %in% excluded_ids) %>% pull(id)
)

cat(sprintf(
  "Schedule: %d rows | %d unique submissions | %d sessions\n\n",
  nrow(schedule),
  n_distinct(schedule$id),
  n_distinct(schedule$session_id)
))

# ── 1. Excluded IDs ────────────────────────────────────────────────────────────

test_that("No rejected IDs in schedule", {
  bad <- intersect(schedule$id, as.character(reject_ids))
  expect_equal(sort(bad), character(0))
})

test_that("No withdrawn IDs in schedule", {
  bad <- intersect(schedule$id, as.character(withdraw_ids))
  expect_equal(sort(bad), character(0))
})

test_that("No dissertation IDs in schedule", {
  bad <- intersect(schedule$id, as.character(dissertation_ids))
  expect_equal(sort(bad), character(0))
})

test_that("No panel discussion IDs in schedule", {
  bad <- intersect(schedule$id, as.character(panel_discussion_ids))
  expect_equal(sort(bad), character(0))
})

# ── 2. Expected IDs present ────────────────────────────────────────────────────

test_that("All accepted (non-dissertation) submissions are scheduled", {
  missing <- setdiff(expected_ids, schedule$id)
  expect_equal(sort(missing), character(0))
})

test_that("No unexpected IDs in schedule", {
  unexpected <- setdiff(schedule$id, expected_ids)
  expect_equal(sort(unexpected), character(0))
})

# ── 3. No duplicate IDs ────────────────────────────────────────────────────────

test_that("Each submission ID appears exactly once", {
  dup_ids <- schedule %>%
    filter(!id %in% as.character(self_organized_panel_ids)) %>%
    count(id) %>%
    filter(n > 1) %>%
    pull(id)
  expect_equal(sort(dup_ids), character(0))
})

# ── 4. Session consistency ─────────────────────────────────────────────────────

test_that("All rows within a session share the same slot, room, and name", {
  inconsistent <- schedule %>%
    group_by(session_id) %>%
    summarise(
      n_slots = n_distinct(time_slot),
      n_rooms = n_distinct(room),
      n_names = n_distinct(session_name),
      .groups = "drop"
    ) %>%
    filter(n_slots > 1 | n_rooms > 1 | n_names > 1) %>%
    pull(session_id)
  expect_equal(sort(inconsistent), character(0))
})

# ── 5. No room conflicts ───────────────────────────────────────────────────────

test_that("No two sessions share the same room and time slot", {
  conflicts <- schedule %>%
    filter(time_slot != 'WE') |>
    distinct(session_id, time_slot, room) %>%
    count(time_slot, room) %>%
    filter(n > 1)
  expect_equal(nrow(conflicts), 0L)
})

# ── 6. Date restrictions honoured ─────────────────────────────────────────────

test_that("All date-restricted submissions are on their required date", {
  restriction_ids <- as.integer(names(date_restrictions))
  scheduled_restricted <- schedule %>%
    filter(id %in% as.character(restriction_ids)) %>%
    distinct(id, date)

  violations <- map_dfr(restriction_ids, function(sid) {
    required <- as.Date(date_restrictions[[as.character(sid)]])
    row <- filter(scheduled_restricted, id == sid)
    if (nrow(row) == 0) {
      return(tibble(id = sid, issue = "not scheduled"))
    }
    if (row$date != required) {
      return(tibble(
        id = sid,
        issue = sprintf("scheduled %s, required %s", row$date, required)
      ))
    }
    tibble()
  })

  expect_equal(
    nrow(violations),
    0L,
    label = if (nrow(violations) > 0) {
      paste(
        paste0("id ", violations$id, ": ", violations$issue),
        collapse = "; "
      )
    }
  )
})

# ── 7. Date exclusions honoured ───────────────────────────────────────────────

test_that("No date-excluded submissions are on their excluded date", {
  exclusion_ids <- as.integer(names(date_exclusions))
  scheduled_excluded <- schedule %>%
    filter(id %in% as.character(exclusion_ids)) %>%
    select(id, date)

  violations <- map_dfr(exclusion_ids, function(sid) {
    excluded <- as.Date(date_exclusions[[as.character(sid)]])
    row <- filter(scheduled_excluded, id == sid)
    if (nrow(row) == 0) {
      return(tibble())
    }
    if (row$date == excluded) {
      return(tibble(
        id = sid,
        issue = sprintf("scheduled on excluded date %s", row$date)
      ))
    }
    tibble()
  })

  expect_equal(
    nrow(violations),
    0L,
    label = if (nrow(violations) > 0) {
      paste(
        paste0("id ", violations$id, ": ", violations$issue),
        collapse = "; "
      )
    }
  )
})

# ── 8. Session sizes ───────────────────────────────────────────────────────────

session_sizes <- schedule %>%
  count(session_id, name = "n_papers")

test_that("No paper session has fewer than 3 papers", {
  undersized <- session_sizes %>%
    filter(n_papers < 3) %>%
    pull(session_id)
  expect_equal(sort(undersized), character(0))
})

test_that("No paper session has more than 5 papers", {
  oversized <- session_sizes %>%
    filter(n_papers > 5) %>%
    pull(session_id)
  expect_equal(sort(oversized), character(0))
})

# ── 8b. PDW registrants not in W1 or W2 ───────────────────────────────────────

test_that("No PDW-registered author is scheduled in W1 or W2", {
  pdw_registered <- read_csv(
    here("data", "pdw-registered.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(author_name = str_to_lower(`Full Name`)) %>%
    pull(author_name)

  violations <- schedule %>%
    filter(time_slot %in% c("W1", "W2")) %>%
    left_join(
      submissions %>% mutate(id = as.character(id)) %>% select(id, authors),
      by = "id",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(authors)) %>%
    mutate(author_list = str_split(authors, ";")) %>%
    unnest(author_list) %>%
    mutate(
      author_name = str_trim(str_replace(author_list, "\\s*\\(.*", ""))
    ) %>%
    filter(author_name != "") %>%
    mutate(author_name = str_to_lower(author_name)) %>%
    filter(author_name %in% pdw_registered) %>%
    distinct(id, session_id, time_slot, author_name)

  expect_equal(
    nrow(violations),
    0L,
    label = if (nrow(violations) > 0) {
      paste(
        paste0(
          violations$author_name,
          " (id ",
          violations$id,
          ", slot ",
          violations$time_slot,
          ")"
        ),
        collapse = "; "
      )
    }
  )
})

# ── 9. No author double-booked in same time slot ──────────────────────────────

# ── 9b. Slot restrictions honoured ────────────────────────────────────────────

test_that("All slot-restricted submissions are in their required time slot", {
  if (!exists("slot_restrictions") || length(slot_restrictions) == 0) {
    skip("No slot restrictions defined")
  }

  session_ids_csv <- read_csv(
    here("data", "session-ids.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(id = as.character(id), session_id = as.character(session_id))

  violations <- map_dfr(names(slot_restrictions), function(pid) {
    required_slot <- slot_restrictions[[pid]]
    sess <- session_ids_csv %>%
      filter(id == pid) %>%
      pull(session_id) %>%
      unique()
    if (length(sess) == 0) {
      return(tibble(id = pid, issue = "not in session-ids.csv"))
    }
    row <- schedule %>%
      filter(session_id %in% sess) %>%
      distinct(session_id, time_slot)
    if (nrow(row) == 0) {
      return(tibble(id = pid, issue = "session not scheduled"))
    }
    if (row$time_slot[1] != required_slot) {
      return(tibble(
        id = pid,
        issue = sprintf(
          "session %s is in %s, required %s",
          row$session_id[1],
          row$time_slot[1],
          required_slot
        )
      ))
    }
    tibble()
  })

  expect_equal(
    nrow(violations),
    0L,
    label = if (nrow(violations) > 0) {
      paste(
        paste0("id ", violations$id, ": ", violations$issue),
        collapse = "; "
      )
    }
  )
})

# ── 10. No author double-booked in same time slot ─────────────────────────────

test_that("No author appears in multiple sessions at the same time", {
  conflicts <- schedule %>%
    left_join(
      submissions %>% mutate(id = as.character(id)) %>% select(id, authors),
      by = "id",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(authors)) %>%
    select(id, session_id, time_slot, authors) %>%
    mutate(author_list = str_split(authors, ";")) %>%
    unnest(author_list) %>%
    mutate(
      author_name = str_trim(str_replace(author_list, "\\s*\\(.*", ""))
    ) %>%
    filter(author_name != "") %>%
    mutate(author_name = str_to_lower(author_name)) %>%
    distinct(session_id, time_slot, author_name) %>%
    group_by(author_name, time_slot) %>%
    filter(n_distinct(session_id) > 1) %>%
    summarise(
      sessions = paste(sort(unique(session_id)), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(time_slot, author_name)

  print(conflicts)

  conflicts <- conflicts %>%
    filter(!str_detect(author_name, "narayanan|combemale")) # Ignore known conflict for Narayanan and Combemale (neither are attending))

  expect_equal(
    nrow(conflicts),
    0L,
    label = if (nrow(conflicts) > 0) {
      paste(
        paste0(
          conflicts$author_name,
          " (slot ",
          conflicts$time_slot,
          ", sessions: ",
          conflicts$sessions,
          ")"
        ),
        collapse = "; "
      )
    }
  )
})

# ── Distribution check ─────────────────────────────────────────────────────────
# Sessions per category per time slot — useful for spotting category clustering.

cat("\n\n=== Session distribution by category and time slot ===\n")

schedule %>%
  left_join(
    submissions %>%
      mutate(id = as.character(id)) %>%
      select(id, category) %>%
      distinct(),
    by = "id"
  ) %>%
  distinct(session_id, time_slot, category) %>%
  count(time_slot, category, name = "n") %>%
  tidyr::pivot_wider(
    names_from = time_slot,
    values_from = n,
    values_fill = 0L
  ) %>%
  arrange(category) %>%
  print(n = 100)

cat("\nSessions per time slot:\n")
schedule %>%
  distinct(session_id, time_slot) %>%
  count(time_slot, name = "n_sessions") %>%
  arrange(time_slot) %>%
  print()
