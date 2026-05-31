SCHEDULE_SEED <- 123

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
  `21` = "2026-06-04",
  `32` = "2026-06-04", # Labor Markets, Organizations
  `64` = "2026-06-05",
  # Date anchors to preserve current session days when re-running slot assignment
  `56` = "2026-06-03", # session 1: Geographic Location and Transportation Cost
  `85` = "2026-06-04", # session 2: Public Policy and ESG
  `103` = "2026-06-04", # session 3: Biodiversity and Agricultural Industry
  `25` = "2026-06-04", # session 4: Private Equity and Capital Markets
  `24` = "2026-06-05", # session 6: Industry Classification and Regulatory Policy (moved from session 5)
  `73` = "2026-06-05", # session 6: Industry Classification and Regulatory Policy
  `46` = "2026-06-04", # session 2: Public Policy and ESG (moved from session 8)
  `50` = "2026-06-04", # session 9: Drug Quality and Safety
  `124` = "2026-06-04", # session 12: Healthcare Operations and Capacity
  `33` = "2026-06-04", # session 15: Advanced Technologies and Digital Innovation
  `77` = "2026-06-05", # session 16: Entrepreneurial Ecosystems and Clusters
  `108` = "2026-06-03", # session 17: Entrepreneurship Research and Policy
  `62` = "2026-06-04", # session 18: Generative AI and the Future of Work
  `54` = "2026-06-04", # session 20: Innovation Strategy and Resources
  `179` = "2026-06-03", # session 22: Technology Legitimacy and Emerging Markets
  `72` = "2026-06-03", # session 23: Hiring, Skills, and Labor Market Dynamics
  `144` = "2026-06-05", # session 26: Platform Work (moved from session 24)
  `96` = "2026-06-05", # session 25: Manufacturing Transformation and Workforce Development
  `100` = "2026-06-03", # session 27: Organizational Structure and Human Capital
  `116` = "2026-06-04", # session 40: Industrial Policy Frameworks (moved from session 28)
  `37` = "2026-06-03", # session 36: AI and Data Tools for Manufacturing (moved from session 29)
  `36` = "2026-06-04", # session 30: Supply Chain Risk and Resilience
  `135` = "2026-06-03", # session 31: Retail and Manufacturing Operations
  `83` = "2026-06-04", # session 32: Sustainable and Circular Supply Chains (moved to T1 Thu to resolve session 10 conflict)
  `51` = "2026-06-04", # session 33: Healthcare and People-Centric Operations
  `28` = "2026-06-03", # session 34: Digital Technology in Manufacturing
  `110` = "2026-06-03", # session 36: AI and Data Tools for Manufacturing
  `157` = "2026-06-03", # session 37: AI and Digital Economy Policy
  `161` = "2026-06-04", # session 42: Industrial Statecraft (moved to T3 Thu)
  `146` = "2026-06-04", # session 42: Industrial Statecraft (pulled from session 19)
  `171` = "2026-06-03", # session 39: Humanitarian and Development Policy
  `90` = "2026-06-04", # session 40: Industrial Policy Frameworks
  `45` = "2026-06-04", # session 41: Regulatory Dynamics and Business Strategy
  `41` = "2026-06-04", # session 42: Industrial Statecraft and Emerging Technology Policy (moved to T3 Thu)
  `67` = "2026-06-04", # session 44: EV Transition and Automotive Industry
  `142` = "2026-06-05", # session 43: Industrial Decarbonization Pathways (moved from session 45)
  `44` = "2026-06-04", # session 46: Energy Systems and Reliability
  `66` = "2026-06-04" # session 47: Green Innovation Diffusion
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
