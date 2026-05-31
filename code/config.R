SCHEDULE_SEED <- 2

source(here::here("code", "setup.R"))

# Edit this file to manage which submissions are recategorized, rejected, or panels.

# --- Configuration ---

# Papers to reject (excluded from all outputs)
reject_ids <- c(104, 139)

# Panel discussion sessions (handled separately via panels.csv)
panel_discussion_ids <- c(70, 101, 115, 117, 136, 174, 182)

# Self-organized panel IDs
self_organized_panel_ids <- c(32, 131)

# Dissertations (accepted but not scheduled for presentation)
dissertation_ids <- c(53, 20, 79, 127, 202, 212, 207, 213)

# Withdrawn submissions (excluded from all outputs)
withdraw_ids <- c(
  153,
  81,
  115,
  173,
  165,
  152,
  158,
  105,
  106,
  40,
  155,
  29,
  112,
  22,
  26,
  102,
  91,
  31,
  35,
  57,
  107,
  23,
  149,
  80,
  111,
  13,
  14,
  15,
  16,
  17,
  18,
  19,
  34,
  38,
  42,
  43,
  47,
  58,
  60,
  68,
  69,
  78,
  82,
  87,
  88,
  121,
  129,
  130,
  145,
  160,
  164,
  166,
  167,
  187,
  188,
  189,
  198
)

# Combined exclusion list used by 3exclude.R and 5verify.R
excluded_ids <- as.character(c(reject_ids, withdraw_ids, dissertation_ids))

# Scheduling restrictions: id -> required date (format: "YYYY-MM-DD")
date_restrictions <- list(
  `200` = "2026-06-05", # Innovation, Entrepreneurship
  `199` = "2026-06-05",
  `131` = "2026-06-03", # Sergey has to travel on 6/4-6/5
  `71` = "2026-06-04",
  `128` = "2026-06-04",
  `141` = "2026-06-05",
  `203` = "2026-06-04",
  `21` = "2026-06-04"
)

# Scheduling exclusions: id -> dates the session must NOT be on (format: "YYYY-MM-DD")
date_exclusions <- list(
  `133` = "2026-06-03", # Sustainable Innovation, Energy, and Mobility
  `148` = "2026-06-03",
  `50` = "2026-06-03",
  `36` = "2026-06-03",
  `51` = "2026-06-03",
  `120` = "2026-06-05",
  `191` = "2026-06-05"
)

# Scheduling slot restrictions: id -> required time slot
# Format: one of "W1","W2","W3","T1","T2","T3","T4","F1"
# These are more specific than date_restrictions and are pre-assigned first.
slot_restrictions <- list(
  `170` = "T2", # Akshay Mutha (session 32: Sustainable and Circular Supply Chains) cannot present in T1
  `147` = "W3", # session 18: Generative AI and the Future of Work — move to Wednesday W3
  `100` = "W3", # session 23: Hiring, Skills, and Labor Market Dynamics — move to Wednesday W2
  `159` = "W3"
)
