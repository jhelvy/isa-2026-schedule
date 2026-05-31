source(here::here("code", "setup.R"))
source(here::here("code", "config.R"))

# Searches for a SCHEDULE_SEED where:
#   (1) All scheduling constraints in 5verify.R pass
#   (2) No more than 3 sessions from the same category fall in any one time slot
# Tries seeds sequentially and stops at the first winner.

config_path <- here::here("code", "config.R")
config_lines <- readLines(config_path)

schedule_path <- here::here("data", "schedule.csv")

# Pre-load static data (doesn't change between seeds)
submissions_all <- read_csv(
  here::here("data", "submissions.csv"),
  show_col_types = FALSE
) %>%
  mutate(id = as.character(id))

pdw_registered_names <- read_csv(
  here::here("data", "pdw-registered.csv"),
  show_col_types = FALSE
) %>%
  mutate(author_name = str_to_lower(`Full Name`)) %>%
  pull(author_name)

# Returns "OK" or a short failure reason string.
check_schedule <- function() {
  schedule <- tryCatch(
    read_csv(schedule_path, show_col_types = FALSE) %>%
      mutate(id = as.character(id), session_id = as.character(session_id)),
    error = function(e) NULL
  )
  if (is.null(schedule)) {
    return("could not read schedule.csv")
  }

  # PDW: no PDW-registered author scheduled in W1 or W2
  pdw_violations <- schedule %>%
    filter(time_slot %in% c("W1", "W2")) %>%
    left_join(
      submissions_all %>% select(id, authors),
      by = "id",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(authors)) %>%
    mutate(author_list = str_split(authors, ";")) %>%
    unnest(author_list) %>%
    mutate(
      author_name = str_to_lower(str_trim(str_replace(
        author_list,
        "\\s*\\(.*",
        ""
      )))
    ) %>%
    filter(author_name != "", author_name %in% pdw_registered_names)
  if (nrow(pdw_violations) > 0) {
    return(sprintf("PDW violation (%d author(s))", nrow(pdw_violations)))
  }

  # Author conflicts: no author double-booked in the same time slot
  author_conflicts <- schedule %>%
    left_join(
      submissions_all %>% select(id, authors),
      by = "id",
      relationship = "many-to-many"
    ) %>%
    filter(!is.na(authors)) %>%
    mutate(author_list = str_split(authors, ";")) %>%
    unnest(author_list) %>%
    mutate(
      author_name = str_to_lower(str_trim(str_replace(
        author_list,
        "\\s*\\(.*",
        ""
      )))
    ) %>%
    filter(author_name != "") %>%
    distinct(session_id, time_slot, author_name) %>%
    group_by(author_name, time_slot) %>%
    filter(n_distinct(session_id) > 1) %>%
    ungroup()
  if (nrow(author_conflicts) > 0) {
    return(sprintf("author conflict (%d)", nrow(author_conflicts)))
  }

  # Session sizes: 3-5 papers (excluding self-organized panels)
  sizes <- schedule %>%
    filter(!id %in% as.character(self_organized_panel_ids)) %>%
    count(session_id, name = "n")
  if (any(sizes$n < 3)) {
    return(sprintf("session too small (min %d)", min(sizes$n)))
  }
  if (any(sizes$n > 5)) {
    return(sprintf("session too large (max %d)", max(sizes$n)))
  }

  # Slot restrictions: slot-restricted sessions must be in their required slot
  if (exists("slot_restrictions") && length(slot_restrictions) > 0) {
    session_ids_csv <- read_csv(
      here::here("data", "session-ids.csv"),
      show_col_types = FALSE
    ) %>%
      mutate(id = as.character(id), session_id = as.character(session_id))

    for (pid in names(slot_restrictions)) {
      required_slot <- slot_restrictions[[pid]]
      sess <- session_ids_csv %>%
        filter(id == pid) %>%
        pull(session_id) %>%
        unique()
      if (length(sess) == 0) {
        next
      }
      row <- schedule %>% filter(session_id %in% sess) %>% distinct(time_slot)
      if (nrow(row) > 0 && row$time_slot[1] != required_slot) {
        return(sprintf(
          "slot restriction violated (paper %s: got %s, need %s)",
          pid,
          row$time_slot[1],
          required_slot
        ))
      }
    }
  }

  # Category distribution: no more than 2 sessions from the same category in any slot
  dist <- schedule %>%
    left_join(
      submissions_all %>% select(id, category) %>% distinct(),
      by = "id"
    ) %>%
    distinct(session_id, time_slot, category) %>%
    count(time_slot, category, name = "n_sessions")
  max_cat <- max(dist$n_sessions, na.rm = TRUE)
  if (max_cat > 2) {
    return(sprintf(
      "category clustering (max %d sessions in one slot)",
      max_cat
    ))
  }

  "OK"
}

# --- Search ---
max_seed <- 10000
cat(sprintf("Searching seeds 1:%d for a valid schedule...\n\n", max_seed))

found_seed <- NA_integer_

for (seed in seq_len(max_seed)) {
  # Update the seed line in config.R
  writeLines(
    sub(
      "^SCHEDULE_SEED <- [0-9]+",
      sprintf("SCHEDULE_SEED <- %d", seed),
      config_lines
    ),
    config_path
  )

  # Run slot assignment + exclusion, suppressing all output
  run_ok <- tryCatch(
    {
      sink(tempfile())
      on.exit(sink(), add = TRUE)
      suppressMessages(suppressWarnings({
        source(here::here("code", "2assign_slots.R"))
        source(here::here("code", "3exclude.R"))
      }))
      sink()
      TRUE
    },
    error = function(e) {
      sink()
      FALSE
    }
  )

  status <- if (!run_ok) "assignment error" else check_schedule()

  cat(sprintf("\rSeed %5d | %s          ", seed, status))
  flush.console()

  if (identical(status, "OK")) {
    found_seed <- seed
    break
  }
}

cat("\n\n")

if (!is.na(found_seed)) {
  cat(sprintf("Valid seed found: %d\n", found_seed))
  cat(sprintf(
    "code/config.R has been updated: SCHEDULE_SEED <- %d\n",
    found_seed
  ))
  cat("Re-run the full pipeline to regenerate all outputs with this seed.\n")
} else {
  cat(sprintf(
    "No valid seed found in 1:%d — try increasing max_seed.\n",
    max_seed
  ))
  # Restore the original config
  writeLines(config_lines, config_path)
}
