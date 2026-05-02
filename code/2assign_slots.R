source(here::here("code", "setup.R"))
source(here::here("code", "config.R"))

# This script assigns sessions to time slots and rooms
# Input: data/session-ids.csv, data/session-slots.csv
# Output: data/schedule.csv (with time_slot, room, day, date, start_time, end_time)

# --- Read session IDs ---
session_ids <- read_csv(
  file.path("data", "session-ids.csv"),
  show_col_types = FALSE
)

# --- Read available slots ---
session_slots <- read_csv(
  file.path("data", "session-slots.csv"),
  show_col_types = FALSE
)

# --- Get unique sessions with category info ---
# Need to get category for each session
session_categories <- session_ids %>%
  left_join(
    submissions %>% select(id, category) %>% distinct(),
    by = "id"
  ) %>%
  group_by(session_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(session_id, session_name, category)

# --- Extract authors for each session ---
# For author conflict avoidance

# Extract authors from paper submissions
all_authors_by_session <- session_ids %>%
  left_join(
    submissions %>% select(id, authors) %>% distinct(),
    by = "id"
  ) %>%
  filter(!is.na(authors)) %>%
  select(session_id, authors) %>%
  mutate(
    author_list = str_split(authors, ";")
  ) %>%
  unnest(author_list) %>%
  mutate(
    author_name = str_trim(str_replace(author_list, "\\s*\\(.*", ""))
  ) %>%
  filter(author_name != "") %>%
  mutate(author_name = str_to_lower(author_name)) %>%
  select(session_id, author_name) %>%
  distinct()

# Create a lookup: session_id -> vector of author names
session_authors_map <- all_authors_by_session %>%
  group_by(session_id) %>%
  summarise(authors = list(author_name), .groups = "drop") %>%
  deframe()

cat("Total sessions to assign:", nrow(session_categories), "\n")

# --- Load PDW registrants (cannot present in W1 or W2) ---
# PDW = Professional Development Workshop, June 3, 2–4:40pm (slots W1 and W2)
pdw_slots <- c("W1", "W2")

pdw_registered <- read_csv(
  file.path("data", "pdw-registered.csv"),
  show_col_types = FALSE
) %>%
  mutate(author_name = str_to_lower(`Full Name`)) %>%
  pull(author_name)

pdw_sessions <- all_authors_by_session %>%
  filter(author_name %in% pdw_registered) %>%
  pull(session_id) %>%
  unique()

cat(
  "Sessions with PDW-registered authors (cannot be in W1 or W2):",
  paste(pdw_sessions, collapse = ", "), "\n"
)

# --- Define time slot order (chronological) ---
# Only use slots that actually exist in session-slots.csv
available_time_slots <- session_slots %>%
  distinct(time_slot) %>%
  pull(time_slot)

# Define desired chronological order and filter to available slots
time_slot_order_full <- c("W1", "W2", "W3", "T1", "T2", "T3", "T4", "F1")
time_slot_order <- time_slot_order_full[
  time_slot_order_full %in% available_time_slots
]

cat("\nUsing time slots:", paste(time_slot_order, collapse = ", "), "\n")

# Track which authors are assigned to each time slot
authors_per_slot <- setNames(
  vector("list", length(time_slot_order)),
  time_slot_order
)
for (slot in time_slot_order) {
  authors_per_slot[[slot]] <- character(0)
}

# Add time order to slots
session_slots <- session_slots %>%
  mutate(
    time_order = match(time_slot, time_slot_order)
  ) %>%
  arrange(time_order, room)

# Count available rooms per time slot
slots_per_time <- session_slots %>%
  count(time_slot, name = "n_rooms")

cat("\nAvailable rooms per time slot:\n")
print(slots_per_time)

total_slots <- nrow(session_slots)
cat("\nTotal available slots:", total_slots, "\n")

if (nrow(session_categories) > total_slots) {
  warning(
    "More sessions (",
    nrow(session_categories),
    ") than available slots (",
    total_slots,
    ")"
  )
}

# --- Assign sessions to slots ---
# Random assignment followed by conflict resolution through swapping

cat("\nTotal sessions to assign:", nrow(session_categories), "\n")

# Build capacity table: named vector of time_slot -> n_rooms
slot_capacity <- setNames(slots_per_time$n_rooms, slots_per_time$time_slot)
slot_capacity <- slot_capacity[time_slot_order] # ensure chronological order

# Create list of all available slots (with repetition for capacity)
all_slots <- rep(time_slot_order, slot_capacity[time_slot_order])

cat("Total available slots:", length(all_slots), "\n")

if (nrow(session_categories) > length(all_slots)) {
  stop(
    "More sessions (",
    nrow(session_categories),
    ") than available slots (",
    length(all_slots),
    ")"
  )
}

# Build slot-to-date lookup and session constraint maps early so that
# pre-assignment and all swap safety checks throughout the script can use them.
slot_to_date <- session_slots %>%
  distinct(time_slot, date) %>%
  { setNames(as.Date(.$date), .$time_slot) }

build_session_constraint_map <- function(config_list) {
  if (length(config_list) == 0) return(list())
  df <- tibble(
    id = as.integer(names(config_list)),
    value = as.Date(unlist(config_list))
  )
  mapped <- session_ids %>%
    inner_join(df, by = "id") %>%
    select(session_id, value) %>%
    distinct()
  setNames(as.list(mapped$value), as.character(mapped$session_id))
}

session_required_date_map <- build_session_constraint_map(date_restrictions)
session_excluded_date_map  <- build_session_constraint_map(date_exclusions)

# Universal date + PDW safety check for early-phase swaps (assigned_time_slot column)
swap_is_date_safe_early <- function(asgn, sess1, sess2) {
  ts1 <- asgn$assigned_time_slot[asgn$session_id == sess1]
  ts2 <- asgn$assigned_time_slot[asgn$session_id == sess2]
  if (ts1 == ts2) return(TRUE)

  if (sess1 %in% pdw_sessions && ts2 %in% pdw_slots) return(FALSE)
  if (sess2 %in% pdw_sessions && ts1 %in% pdw_slots) return(FALSE)

  d1 <- slot_to_date[[ts1]]
  d2 <- slot_to_date[[ts2]]
  if (identical(d1, d2)) return(TRUE)

  s1 <- as.character(sess1); s2 <- as.character(sess2)

  req1 <- session_required_date_map[[s1]]
  if (!is.null(req1) && d2 != req1) return(FALSE)

  req2 <- session_required_date_map[[s2]]
  if (!is.null(req2) && d1 != req2) return(FALSE)

  excl1 <- session_excluded_date_map[[s1]]
  if (!is.null(excl1) && d2 == excl1) return(FALSE)

  excl2 <- session_excluded_date_map[[s2]]
  if (!is.null(excl2) && d1 == excl2) return(FALSE)

  TRUE
}

# Set seed for reproducibility (defined in config.R)
if (exists("SCHEDULE_SEED")) {
  seed_val <- as.integer(SCHEDULE_SEED)
  set.seed(seed_val)
  cat("\nUsing random seed:", seed_val, "\n")
} else {
  set.seed(42)
  cat("\nUsing default seed: 42 (set SCHEDULE_SEED in config.R to change)\n")
}

# Pre-assign date-restricted sessions to their required date slots BEFORE the
# random shuffle, guaranteeing the constraint is met from the start.
constrained_session_ids <- as.integer(names(session_required_date_map))

remaining_all_slots <- all_slots
pre_assigned <- tibble()

for (sess_id in sample(constrained_session_ids)) {
  req_date <- session_required_date_map[[as.character(sess_id)]]
  valid <- remaining_all_slots[slot_to_date[remaining_all_slots] == req_date]
  if (length(valid) == 0) {
    warning("No slot on ", req_date, " for session ", sess_id, " — skipping pre-assignment")
    next
  }
  chosen <- sample(valid, 1)
  pre_assigned <- bind_rows(
    pre_assigned,
    tibble(session_id = sess_id, assigned_time_slot = chosen)
  )
  remaining_all_slots <- remaining_all_slots[-which(remaining_all_slots == chosen)[1]]
}

pre_assigned <- pre_assigned %>%
  left_join(session_categories, by = "session_id")

# Randomly assign remaining (unconstrained) sessions to the leftover slots
unconstrained_sessions <- session_categories %>%
  filter(!session_id %in% pre_assigned$session_id) %>%
  slice(sample(n()))

unconstrained_assignments <- unconstrained_sessions %>%
  mutate(assigned_time_slot = sample(remaining_all_slots, nrow(unconstrained_sessions)))

session_assignments <- bind_rows(pre_assigned, unconstrained_assignments)

cat(sprintf(
  "Pre-assigned %d date-restricted sessions; randomly assigned %d remaining.\n",
  nrow(pre_assigned), nrow(unconstrained_assignments)
))

# --- Conflict resolution: try to swap sessions to eliminate author conflicts ---

cat("\n\n=== Attempting to resolve author conflicts ===\n")

# Build author -> sessions mapping for conflict detection
author_to_sessions <- all_authors_by_session %>%
  group_by(author_name) %>%
  summarise(sessions = list(session_id), .groups = "drop")

# Function to detect conflicts in current assignment
detect_conflicts <- function(assignments) {
  conflicts <- list()

  for (i in seq_len(nrow(author_to_sessions))) {
    author <- author_to_sessions$author_name[i]
    sessions <- author_to_sessions$sessions[[i]]

    # Get slots for all this author's sessions
    author_slots <- assignments %>%
      filter(session_id %in% sessions) %>%
      select(session_id, assigned_time_slot)

    # Check for duplicate slots
    dup_slots <- author_slots %>%
      group_by(assigned_time_slot) %>%
      filter(n() > 1) %>%
      ungroup()

    if (nrow(dup_slots) > 0) {
      for (slot in unique(dup_slots$assigned_time_slot)) {
        slot_sessions <- dup_slots %>%
          filter(assigned_time_slot == slot) %>%
          pull(session_id)

        conflicts[[length(conflicts) + 1]] <- list(
          author = author,
          slot = slot,
          sessions = slot_sessions
        )
      }
    }
  }

  conflicts
}

# Function to try swapping two sessions
try_swap <- function(assignments, sess1, sess2) {
  idx1 <- which(assignments$session_id == sess1)
  idx2 <- which(assignments$session_id == sess2)

  # Swap slots
  slot1 <- assignments$assigned_time_slot[idx1]
  slot2 <- assignments$assigned_time_slot[idx2]

  assignments$assigned_time_slot[idx1] <- slot2
  assignments$assigned_time_slot[idx2] <- slot1

  assignments
}

# Iteratively try to resolve conflicts
max_iterations <- 500
iteration <- 0
conflicts <- detect_conflicts(session_assignments)

cat(sprintf("Initial conflicts: %d\n", length(conflicts)))

while (length(conflicts) > 0 && iteration < max_iterations) {
  iteration <- iteration + 1
  resolved_any <- FALSE

  for (conflict in conflicts) {
    author <- conflict$author
    slot <- conflict$slot
    conflict_sessions <- conflict$sessions

    # Try to swap one of the conflicting sessions to a different slot
    for (sess in conflict_sessions) {
      # Get all slots that don't have this author
      author_sessions <- author_to_sessions %>%
        filter(author_name == author) %>%
        pull(sessions) %>%
        .[[1]]

      author_slots <- session_assignments %>%
        filter(session_id %in% author_sessions) %>%
        pull(assigned_time_slot) %>%
        unique()

      # Find slots without this author
      available_slots <- setdiff(time_slot_order, author_slots)

      # Try swapping with a session in an available slot
      for (target_slot in available_slots) {
        candidates <- session_assignments %>%
          filter(
            assigned_time_slot == target_slot,
            !session_id %in% author_sessions # Don't swap with another session by same author
          ) %>%
          pull(session_id)

        for (swap_candidate in candidates) {
          # Skip swaps that would violate date or PDW constraints
          if (!swap_is_date_safe_early(session_assignments, sess, swap_candidate)) next

          # Test the swap
          test_assignments <- try_swap(
            session_assignments,
            sess,
            swap_candidate
          )
          new_conflicts <- detect_conflicts(test_assignments)

          # If it reduces conflicts, accept the swap
          if (length(new_conflicts) < length(conflicts)) {
            session_assignments <- test_assignments
            conflicts <- new_conflicts
            resolved_any <- TRUE
            cat(sprintf(
              "  Iteration %d: Swapped sessions %d and %d (reduced conflicts to %d)\n",
              iteration,
              sess,
              swap_candidate,
              length(conflicts)
            ))
            break
          }
        }
      }

      if (resolved_any) break
    }

    if (resolved_any) break
  }

  if (!resolved_any) {
    cat(sprintf(
      "  Iteration %d: No swaps improved conflicts. Stopping.\n",
      iteration
    ))
    break
  }
}

if (length(conflicts) > 0) {
  cat(sprintf(
    "\nRemaining conflicts after %d iterations: %d\n",
    iteration,
    length(conflicts)
  ))
  for (conflict in conflicts) {
    cat(sprintf(
      "  %s in slot %s: sessions %s\n",
      conflict$author,
      conflict$slot,
      paste(conflict$sessions, collapse = ", ")
    ))
  }
} else {
  cat(sprintf("\nAll conflicts resolved after %d iterations!\n", iteration))
}

# --- Category spread optimization ---
# After resolving author conflicts, try to spread sessions from the same
# category across different time slots.

cat("\n\n=== Optimizing category spread ===\n")

cat_score <- function(asgn) {
  asgn %>%
    count(assigned_time_slot, category) %>%
    filter(n > 1) %>%
    pull(n) %>%
    {
      if (length(.) == 0) 0L else sum(. - 1L)
    }
}

# Check: would swapping sess1 and sess2 introduce new author, date, or PDW conflicts?
swap_is_author_safe <- function(asgn, sess1, sess2) {
  if (!swap_is_date_safe_early(asgn, sess1, sess2)) return(FALSE)

  slot1 <- asgn$assigned_time_slot[asgn$session_id == sess1]
  slot2 <- asgn$assigned_time_slot[asgn$session_id == sess2]
  if (slot1 == slot2) {
    return(TRUE)
  }

  a1 <- session_authors_map[[as.character(sess1)]]
  a2 <- session_authors_map[[as.character(sess2)]]

  if (!is.null(a1)) {
    others_in_slot2 <- asgn %>%
      filter(assigned_time_slot == slot2, session_id != sess2) %>%
      pull(session_id)
    if (length(others_in_slot2) > 0) {
      other_authors <- unlist(session_authors_map[as.character(
        others_in_slot2
      )])
      if (any(a1 %in% other_authors)) return(FALSE)
    }
  }

  if (!is.null(a2)) {
    others_in_slot1 <- asgn %>%
      filter(assigned_time_slot == slot1, session_id != sess1) %>%
      pull(session_id)
    if (length(others_in_slot1) > 0) {
      other_authors <- unlist(session_authors_map[as.character(
        others_in_slot1
      )])
      if (any(a2 %in% other_authors)) return(FALSE)
    }
  }

  TRUE
}

current_cat_score <- cat_score(session_assignments)
cat(sprintf("Initial category clustering score: %d\n", current_cat_score))

max_cat_iter <- 500
cat_iter <- 0
cat_improved <- TRUE

while (cat_improved && cat_iter < max_cat_iter && current_cat_score > 0) {
  cat_improved <- FALSE
  cat_iter <- cat_iter + 1

  clustered <- session_assignments %>%
    add_count(assigned_time_slot, category, name = "slot_cat_n") %>%
    filter(slot_cat_n > 1) %>%
    arrange(desc(slot_cat_n))

  if (nrow(clustered) == 0) {
    break
  }

  found_swap <- FALSE
  for (i in seq_len(nrow(clustered))) {
    if (found_swap) {
      break
    }
    sess1 <- clustered$session_id[i]

    swap_candidates <- session_assignments %>%
      filter(session_id != sess1) %>%
      pull(session_id)

    for (sess2 in swap_candidates) {
      test <- try_swap(session_assignments, sess1, sess2)
      new_score <- cat_score(test)

      if (
        new_score < current_cat_score &&
          swap_is_author_safe(session_assignments, sess1, sess2)
      ) {
        session_assignments <- test
        current_cat_score <- new_score
        cat_improved <- TRUE
        found_swap <- TRUE
        cat(sprintf(
          "  Iter %d: swapped sessions %d <-> %d (category score now %d)\n",
          cat_iter,
          sess1,
          sess2,
          current_cat_score
        ))
        break
      }
    }
  }
}

cat(sprintf(
  "Category spread optimization complete after %d iterations. Final score: %d\n",
  cat_iter,
  current_cat_score
))

# Distribution check: sessions per category per time slot
cat("\nSessions per category per time slot:\n")
distribution_check <- session_assignments %>%
  count(assigned_time_slot, category, name = "n") %>%
  tidyr::pivot_wider(
    names_from = assigned_time_slot,
    values_from = n,
    values_fill = 0L
  ) %>%
  arrange(category)
print(distribution_check, n = 100)

max_per_slot <- session_assignments %>%
  count(assigned_time_slot, category, name = "n") %>%
  pull(n) %>%
  max()
cat("Max sessions from one category in a single time slot:", max_per_slot, "\n")

# Assign rooms within each time slot
session_assignments <- session_assignments %>%
  group_by(assigned_time_slot) %>%
  arrange(category) %>%
  mutate(room_index = row_number()) %>%
  ungroup()

# Get room lookup
room_lookup <- session_slots %>%
  group_by(time_slot) %>%
  mutate(room_index = row_number()) %>%
  ungroup() %>%
  select(
    time_slot,
    room_index,
    room,
    date,
    day,
    start_time,
    end_time,
    time_order
  )

# Join to get room assignments
session_assignments <- session_assignments %>%
  left_join(
    room_lookup,
    by = c("assigned_time_slot" = "time_slot", "room_index")
  ) %>%
  select(
    session_id,
    session_name,
    category,
    time_slot = assigned_time_slot,
    room,
    date,
    day,
    start_time,
    end_time,
    time_order
  )

# --- Verify date restrictions ---
# Sessions were pre-assigned to their required dates before the random shuffle.
# This block confirms the constraint survived all optimization phases.

cat("\n\n=== Verifying date restrictions ===\n")
if (length(date_restrictions) > 0) {
  restriction_df <- tibble(
    id = as.integer(names(date_restrictions)),
    required_date = as.Date(unlist(date_restrictions))
  )
  session_date_req <- session_ids %>%
    inner_join(restriction_df, by = "id") %>%
    select(session_id, required_date) %>%
    distinct()

  for (i in seq_len(nrow(session_date_req))) {
    req_session_id <- session_date_req$session_id[i]
    req_date        <- session_date_req$required_date[i]
    current_row     <- session_assignments %>% filter(session_id == req_session_id)
    if (nrow(current_row) == 0) next
    if (as.Date(current_row$date) != req_date) {
      warning(sprintf(
        "Date restriction VIOLATED: session %d is on %s but required %s",
        req_session_id, current_row$date, req_date
      ))
    } else {
      cat(sprintf("  OK session %d on %s\n", req_session_id, format(req_date)))
    }
  }
}

# --- Apply date exclusions ---
# For each paper with a date exclusion, find its session and if it lands on an
# excluded date, swap it with a session that is NOT on that excluded date.

if (length(date_exclusions) > 0) {
  exclusion_df <- tibble(
    id = as.integer(names(date_exclusions)),
    excluded_date = as.Date(unlist(date_exclusions))
  )

  session_date_excl <- session_ids %>%
    inner_join(exclusion_df, by = "id") %>%
    select(session_id, excluded_date) %>%
    distinct()

  slot_cols <- c(
    "time_slot",
    "room",
    "date",
    "day",
    "start_time",
    "end_time",
    "time_order"
  )

  for (i in seq_len(nrow(session_date_excl))) {
    excl_session_id <- session_date_excl$session_id[i]
    excl_date <- session_date_excl$excluded_date[i]

    current_row <- session_assignments %>% filter(session_id == excl_session_id)
    if (nrow(current_row) == 0) {
      next
    }
    if (as.Date(current_row$date) != excl_date) {
      next
    } # already off the excluded date

    # Find another session NOT on the excluded date, preferring those that
    # don't share authors with the excluded session (to avoid new conflicts).
    excl_authors <- session_authors_map[[as.character(excl_session_id)]]
    candidates_off_date <- session_assignments %>%
      filter(as.Date(date) != excl_date, session_id != excl_session_id)

    if (!is.null(excl_authors) && nrow(candidates_off_date) > 0) {
      conflicting_sessions <- all_authors_by_session %>%
        filter(author_name %in% excl_authors) %>%
        pull(session_id) %>%
        unique()
      conflict_free <- candidates_off_date %>%
        filter(!session_id %in% conflicting_sessions)
      candidate <- if (nrow(conflict_free) > 0) {
        slice(conflict_free, 1)
      } else {
        slice(candidates_off_date, 1)
      }
    } else {
      candidate <- slice(candidates_off_date, 1)
    }

    if (nrow(candidate) == 0) {
      warning(
        "No session available off ",
        excl_date,
        " to swap with session ",
        excl_session_id,
        " — date exclusion cannot be satisfied"
      )
      next
    }

    # Swap slot assignments between the two sessions
    idx_excl <- which(session_assignments$session_id == excl_session_id)
    idx_cand <- which(session_assignments$session_id == candidate$session_id)

    excl_old_slot <- session_assignments$time_slot[idx_excl]
    cand_old_slot <- session_assignments$time_slot[idx_cand]

    tmp <- session_assignments[idx_excl, slot_cols]
    session_assignments[idx_excl, slot_cols] <- session_assignments[
      idx_cand,
      slot_cols
    ]
    session_assignments[idx_cand, slot_cols] <- tmp

    # Update author tracking after swap
    excl_authors <- session_authors_map[[as.character(excl_session_id)]]
    cand_authors <- session_authors_map[[as.character(candidate$session_id)]]

    if (!is.null(excl_authors) && !is.null(cand_authors)) {
      authors_per_slot[[excl_old_slot]] <- setdiff(
        authors_per_slot[[excl_old_slot]],
        excl_authors
      )
      authors_per_slot[[cand_old_slot]] <- setdiff(
        authors_per_slot[[cand_old_slot]],
        cand_authors
      )
      authors_per_slot[[cand_old_slot]] <- c(
        authors_per_slot[[cand_old_slot]],
        excl_authors
      )
      authors_per_slot[[excl_old_slot]] <- c(
        authors_per_slot[[excl_old_slot]],
        cand_authors
      )
    }

    cat(
      "  - Session",
      excl_session_id,
      "moved off",
      format(excl_date),
      "(swapped with session",
      candidate$session_id,
      ")\n"
    )
  }
}

# --- Apply PDW slot exclusions ---
# PDW registrants cannot present during W1 or W2 (June 3, 2–4:40pm).
# For each affected session currently in W1 or W2, swap it to another slot.

cat("\n\n=== Applying PDW slot exclusions ===\n")

for (sess_id in pdw_sessions) {
  current_row <- session_assignments %>% filter(session_id == sess_id)
  if (nrow(current_row) == 0) next
  if (!current_row$time_slot %in% pdw_slots) next

  sess_authors <- session_authors_map[[as.character(sess_id)]]
  sess_old_slot <- current_row$time_slot
  pdw_date <- slot_to_date[pdw_slots[1]]  # June 3

  # Candidates: outside W1/W2, not PDW-restricted, and safe to land in W1/W2
  # (i.e., no date restriction requiring a different date, no exclusion for June 3)
  candidates <- session_assignments %>%
    filter(!time_slot %in% pdw_slots, !session_id %in% pdw_sessions) %>%
    filter(map_lgl(session_id, ~ {
      key <- as.character(.x)
      req  <- session_required_date_map[[key]]
      if (!is.null(req)  && req  != pdw_date) return(FALSE)
      excl <- session_excluded_date_map[[key]]
      if (!is.null(excl) && excl == pdw_date) return(FALSE)
      TRUE
    }))

  if (!is.null(sess_authors) && nrow(candidates) > 0) {
    conflicting_sessions <- all_authors_by_session %>%
      filter(author_name %in% sess_authors) %>%
      pull(session_id) %>%
      unique()
    conflict_free <- candidates %>%
      filter(!session_id %in% conflicting_sessions)
    candidate <- if (nrow(conflict_free) > 0) {
      slice(conflict_free, 1)
    } else {
      slice(candidates, 1)
    }
  } else {
    candidate <- slice(candidates, 1)
  }

  if (nrow(candidate) == 0) {
    warning(
      "No session available outside W1/W2 to swap with session ", sess_id,
      " — PDW slot exclusion cannot be satisfied"
    )
    next
  }

  idx_sess <- which(session_assignments$session_id == sess_id)
  idx_cand <- which(session_assignments$session_id == candidate$session_id)

  tmp <- session_assignments[idx_sess, slot_cols]
  session_assignments[idx_sess, slot_cols] <- session_assignments[idx_cand, slot_cols]
  session_assignments[idx_cand, slot_cols] <- tmp

  cat(sprintf(
    "  - Session %d moved from %s to %s (swapped with session %d)\n",
    sess_id,
    sess_old_slot,
    session_assignments$time_slot[idx_sess],
    candidate$session_id
  ))
}

# --- Post-constraint author conflict resolution ---
# Re-run conflict resolution to fix any author conflicts introduced by
# date restriction / exclusion enforcement. Swaps must respect all date
# constraints for both sessions involved.

cat("\n\n=== Post-constraint author conflict resolution ===\n")

# session_required_date_map and session_excluded_date_map are already built
# early in the script and reused here.

# Check if swapping two sessions' full slot assignments would violate date or PDW constraints
swap_is_date_safe_post <- function(asgn, sess1, sess2) {
  slot1 <- asgn$time_slot[asgn$session_id == sess1]
  slot2 <- asgn$time_slot[asgn$session_id == sess2]

  # PDW constraint: PDW-registered sessions cannot be swapped into W1 or W2
  if (sess1 %in% pdw_sessions && slot2 %in% pdw_slots) return(FALSE)
  if (sess2 %in% pdw_sessions && slot1 %in% pdw_slots) return(FALSE)

  date1 <- as.Date(asgn$date[asgn$session_id == sess1])
  date2 <- as.Date(asgn$date[asgn$session_id == sess2])
  if (identical(date1, date2)) {
    return(TRUE)
  }

  s1 <- as.character(sess1)
  s2 <- as.character(sess2)

  req1 <- session_required_date_map[[s1]]
  if (!is.null(req1) && date2 != req1) {
    return(FALSE)
  }

  req2 <- session_required_date_map[[s2]]
  if (!is.null(req2) && date1 != req2) {
    return(FALSE)
  }

  excl1 <- session_excluded_date_map[[s1]]
  if (!is.null(excl1) && date2 == excl1) {
    return(FALSE)
  }

  excl2 <- session_excluded_date_map[[s2]]
  if (!is.null(excl2) && date1 == excl2) {
    return(FALSE)
  }

  TRUE
}

# Detect author conflicts using the post-room-assignment "time_slot" column
detect_conflicts_post <- function(asgn) {
  conflicts <- list()
  for (i in seq_len(nrow(author_to_sessions))) {
    author <- author_to_sessions$author_name[i]
    sessions <- author_to_sessions$sessions[[i]]
    author_slots <- asgn %>%
      filter(session_id %in% sessions) %>%
      select(session_id, time_slot)
    dup_slots <- author_slots %>%
      group_by(time_slot) %>%
      filter(n() > 1) %>%
      ungroup()
    if (nrow(dup_slots) > 0) {
      for (ts in unique(dup_slots$time_slot)) {
        slot_sessions <- dup_slots %>%
          filter(time_slot == ts) %>%
          pull(session_id)
        conflicts[[length(conflicts) + 1]] <- list(
          author = author,
          slot = ts,
          sessions = slot_sessions
        )
      }
    }
  }
  conflicts
}

slot_cols_post <- c(
  "time_slot",
  "room",
  "date",
  "day",
  "start_time",
  "end_time",
  "time_order"
)

try_swap_post <- function(asgn, sess1, sess2) {
  idx1 <- which(asgn$session_id == sess1)
  idx2 <- which(asgn$session_id == sess2)
  tmp <- asgn[idx1, slot_cols_post]
  asgn[idx1, slot_cols_post] <- asgn[idx2, slot_cols_post]
  asgn[idx2, slot_cols_post] <- tmp
  asgn
}

post_conflicts <- detect_conflicts_post(session_assignments)
cat(sprintf(
  "Conflicts after constraint enforcement: %d\n",
  length(post_conflicts)
))

max_post_iter <- 500
post_iter <- 0

while (length(post_conflicts) > 0 && post_iter < max_post_iter) {
  post_iter <- post_iter + 1
  resolved_any <- FALSE

  for (conflict in post_conflicts) {
    author <- conflict$author
    conflict_sessions <- conflict$sessions

    author_sessions <- author_to_sessions %>%
      filter(author_name == author) %>%
      pull(sessions) %>%
      .[[1]]

    author_slots <- session_assignments %>%
      filter(session_id %in% author_sessions) %>%
      pull(time_slot) %>%
      unique()

    other_slots <- setdiff(time_slot_order, author_slots)

    for (sess in conflict_sessions) {
      for (target_slot in other_slots) {
        candidates <- session_assignments %>%
          filter(time_slot == target_slot, !session_id %in% author_sessions) %>%
          pull(session_id)

        for (swap_candidate in candidates) {
          if (
            !swap_is_date_safe_post(session_assignments, sess, swap_candidate)
          ) {
            next
          }

          test <- try_swap_post(session_assignments, sess, swap_candidate)
          new_conflicts <- detect_conflicts_post(test)

          if (length(new_conflicts) < length(post_conflicts)) {
            session_assignments <- test
            post_conflicts <- new_conflicts
            resolved_any <- TRUE
            cat(sprintf(
              "  Iter %d: Swapped sessions %d <-> %d (conflicts now %d)\n",
              post_iter,
              sess,
              swap_candidate,
              length(post_conflicts)
            ))
            break
          }
        }
        if (resolved_any) break
      }
      if (resolved_any) break
    }
    if (resolved_any) break
  }

  if (!resolved_any) {
    cat(sprintf("  Iter %d: No valid swaps found. Stopping.\n", post_iter))
    break
  }
}

if (length(post_conflicts) > 0) {
  cat(sprintf(
    "\nRemaining conflicts after post-constraint resolution (%d iter): %d\n",
    post_iter,
    length(post_conflicts)
  ))
  for (conflict in post_conflicts) {
    cat(sprintf(
      "  %s in slot %s: sessions %s\n",
      conflict$author,
      conflict$slot,
      paste(conflict$sessions, collapse = ", ")
    ))
  }
} else {
  cat(sprintf(
    "\nAll conflicts resolved after %d post-constraint iterations!\n",
    post_iter
  ))
}

# --- Final category spread optimization (post all constraints) ---
# Re-run after date restrictions, exclusions, and author conflict resolution
# since those steps can reintroduce category clustering.

cat("\n\n=== Final category spread optimization ===\n")

cat_score_final <- function(asgn) {
  asgn %>%
    count(time_slot, category, name = "n") %>%
    filter(n > 1) %>%
    pull(n) %>%
    {
      if (length(.) == 0) 0L else sum(. - 1L)
    }
}

swap_is_author_safe_final <- function(asgn, sess1, sess2) {
  slot1 <- asgn$time_slot[asgn$session_id == sess1]
  slot2 <- asgn$time_slot[asgn$session_id == sess2]
  if (slot1 == slot2) return(TRUE)

  a1 <- session_authors_map[[as.character(sess1)]]
  a2 <- session_authors_map[[as.character(sess2)]]

  if (!is.null(a1)) {
    others <- asgn$session_id[asgn$time_slot == slot2 & asgn$session_id != sess2]
    if (length(others) > 0) {
      if (any(a1 %in% unlist(session_authors_map[as.character(others)]))) {
        return(FALSE)
      }
    }
  }

  if (!is.null(a2)) {
    others <- asgn$session_id[asgn$time_slot == slot1 & asgn$session_id != sess1]
    if (length(others) > 0) {
      if (any(a2 %in% unlist(session_authors_map[as.character(others)]))) {
        return(FALSE)
      }
    }
  }

  TRUE
}

current_final_score <- cat_score_final(session_assignments)
cat(sprintf("Category score entering final pass: %d\n", current_final_score))

max_final_iter <- 1000
final_iter <- 0
final_improved <- TRUE

while (final_improved && final_iter < max_final_iter && current_final_score > 0) {
  final_improved <- FALSE
  final_iter <- final_iter + 1

  clustered <- session_assignments %>%
    add_count(time_slot, category, name = "slot_cat_n") %>%
    filter(slot_cat_n > 1) %>%
    arrange(desc(slot_cat_n))

  if (nrow(clustered) == 0) break

  found_swap <- FALSE
  for (i in seq_len(nrow(clustered))) {
    if (found_swap) break
    sess1 <- clustered$session_id[i]

    swap_candidates <- session_assignments %>%
      filter(session_id != sess1) %>%
      pull(session_id)

    for (sess2 in swap_candidates) {
      if (!swap_is_author_safe_final(session_assignments, sess1, sess2)) next
      if (!swap_is_date_safe_post(session_assignments, sess1, sess2)) next

      test <- try_swap_post(session_assignments, sess1, sess2)
      new_score <- cat_score_final(test)

      if (new_score < current_final_score) {
        session_assignments <- test
        current_final_score <- new_score
        final_improved <- TRUE
        found_swap <- TRUE
        cat(sprintf(
          "  Iter %d: Swapped sessions %d <-> %d (score now %d)\n",
          final_iter,
          sess1,
          sess2,
          current_final_score
        ))
        break
      }
    }
  }

  if (!found_swap) {
    cat(sprintf(
      "  Iter %d: No valid swaps found. Stopping.\n",
      final_iter
    ))
    break
  }
}

cat(sprintf(
  "Final category spread complete: score %d after %d iterations\n",
  current_final_score,
  final_iter
))

# Distribution check after final optimization
cat("\nFinal sessions per category per time slot:\n")
session_assignments %>%
  count(time_slot, category, name = "n") %>%
  tidyr::pivot_wider(
    names_from = time_slot,
    values_from = n,
    values_fill = 0L
  ) %>%
  arrange(category) %>%
  print(n = 100)

# --- Merge back with full session data ---
schedule <- session_ids %>%
  left_join(
    session_assignments %>%
      select(
        session_id,
        time_slot,
        room,
        date,
        day,
        start_time,
        end_time,
        time_order
      ),
    by = "session_id"
  ) %>%
  arrange(time_order, room, id)

# --- Write output ---
write_csv(schedule, file.path("data", "schedule.csv"))

# Summary
schedule %>%
  distinct(session_id, time_slot, day) %>%
  count(time_slot, day) %>%
  print()
