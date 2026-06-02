# check_missing_badges.R
#
# Compares attendees_final.csv against the three badge source files
# (registrants.csv, guests.csv, extra.csv) and reports anyone who is
# in the final attendee list but not yet covered by a badge file.
# Missing people need to be added to data/extra.csv.

library(tidyverse)

# ── Load badge source files ────────────────────────────────────────────────

registrants <- read_csv("data/registrants.csv", col_types = cols(.default = "c")) %>%
  mutate(source = "registrants")

guests <- read_csv("data/guests.csv", col_types = cols(.default = "c")) %>%
  select(name, email) %>%
  mutate(source = "guests")

extra <- read_csv("data/extra.csv", col_types = cols(.default = "c")) %>%
  select(name, email) %>%
  mutate(source = "extra")

# Combine all badge sources; normalise email for matching
badge_emails <- bind_rows(
  registrants %>% select(name, email, source),
  guests,
  extra
) %>%
  mutate(email_key = str_trim(str_to_lower(email))) %>%
  filter(!is.na(email_key), email_key != "")

# ── Load final attendee list ───────────────────────────────────────────────

attendees <- read_csv("data/attendees_final.csv", col_types = cols(.default = "c")) %>%
  mutate(email_key = str_trim(str_to_lower(email)))

# ── Find missing attendees ─────────────────────────────────────────────────

missing <- attendees %>%
  filter(!email_key %in% badge_emails$email_key) %>%
  select(name, email, affiliation)

# ── Report ─────────────────────────────────────────────────────────────────

cat("Badge source file totals:\n")
cat(sprintf("  registrants.csv : %d\n", nrow(registrants)))
cat(sprintf("  guests.csv      : %d\n", nrow(guests)))
cat(sprintf("  extra.csv       : %d\n", nrow(extra)))
cat(sprintf("  attendees_final : %d\n", nrow(attendees)))
cat("\n")

if (nrow(missing) == 0) {
  cat("All attendees are covered — no missing badges.\n")
} else {
  cat(sprintf("%d attendee(s) not found in any badge file:\n\n", nrow(missing)))
  print(missing, n = Inf)
  cat("\n")

  # Append missing entries to extra.csv (title left blank)
  extra_full <- read_csv("data/extra.csv", col_types = cols(.default = "c"))
  to_add <- missing %>%
    mutate(title = NA_character_) %>%
    select(name, email, title, affiliation)

  extra_updated <- bind_rows(extra_full, to_add)
  write_csv(extra_updated, "data/extra.csv", na = "")

  cat(sprintf("Added %d missing attendee(s) to data/extra.csv.\n", nrow(to_add)))
  cat("Title field left blank — fill in manually as needed.\n")
}
