# ISA 2026 Schedule — Public Repo

## Overview

This is the public-facing repository for the ISA 2026 conference schedule. It contains everything needed to assign papers to time slots, generate the schedule PDF, and update the web app. All earlier pipeline steps (cleaning submissions, track chair review, session grouping) have already been completed; only the scheduling and publishing steps remain here.

## Workflow

Run these scripts in order when the schedule needs to be updated:

```
Rscript code/1assign_slots.R     # Assign sessions to time slots → data/schedule.csv
Rscript code/2generate_json.R    # Build JSON for the web app → schedule_data.json
quarto render schedule.qmd       # Render the PDF program → schedule.pdf
Rscript code/3verify.R           # Verify all scheduling constraints are satisfied
```

## File Structure

```
isa-2026-schedule.Rproj

code/
  config.R            # Configuration: seed, reject/withdraw/dissertation IDs, date constraints
  setup.R             # Loads libraries and reads data/submissions.csv
  1assign_slots.R     # Assigns sessions to time slots and rooms → data/schedule.csv
  2generate_json.R    # Generates schedule_data.json from schedule + panels
  3verify.R           # Test suite for scheduling constraints

data/
  submissions.csv     # All accepted submissions (cleaned, with type/category/authors)
  session-ids.csv     # Paper → session mapping (edit to change session groupings)
  session-slots.csv   # Available time slots and rooms
  schedule.csv        # OUTPUT: full schedule with time slots and rooms
  panels.csv          # Special sessions and plenaries
  self-sessions.csv   # Papers within self-organized panels (used by schedule.qmd)
  pdw-registered.csv  # PDW registrants who cannot present in W1 or W2
  special-slots.csv   # Special slot definitions

index.html            # Web app (served by GitHub Pages)
logo.png              # Conference logo
schedule_data.json    # OUTPUT: web app data (served by GitHub Pages)
schedule.pdf          # OUTPUT: printed program
schedule.qmd          # Quarto source for the printed program PDF
```

## Configuration (`code/config.R`)

- `SCHEDULE_SEED` — random seed for reproducible slot assignment
- `reject_ids` / `withdraw_ids` — excluded from all outputs
- `dissertation_ids` — accepted but not scheduled for presentation
- `panel_discussion_ids` — excluded from paper scheduling (handled via `data/panels.csv`)
- `self_organized_panel_ids` — IDs for self-organized panel submissions
- `date_restrictions` — `id → required date` (session must fall on this date)
- `date_exclusions` — `id → excluded date` (session must NOT fall on this date)

## Time Slots

| slot | day | time |
|---|---|---|
| W1 | Wednesday June 3 | 2:00–3:20 pm |
| W2 | Wednesday June 3 | 3:20–4:40 pm |
| W3 | Wednesday June 3 | 4:40–6:00 pm |
| T1 | Thursday June 4 | morning |
| T2 | Thursday June 4 | late morning |
| T3 | Thursday June 4 | afternoon |
| T4 | Thursday June 4 | late afternoon |
| F1 | Friday June 5 | morning |

W1 and W2 overlap with the Professional Development Workshop — no PDW registrants (`data/pdw-registered.csv`) may present in those slots.

## Common Tasks

**Reassign time slots**: just re-run `Rscript code/1assign_slots.R` (adjust `SCHEDULE_SEED` in `config.R` to get a different random assignment).

**Change session groupings**: edit `data/session-ids.csv` (`session_id` and `session_name` columns), then re-run `1assign_slots.R`.

**Add a date restriction**: add an entry to `date_restrictions` in `code/config.R`, then re-run `1assign_slots.R`.

**Update panels/special sessions**: edit `data/panels.csv`, then re-run `2generate_json.R` and re-render `schedule.qmd`.

## Workflow Rules

- **Never run R scripts or render `.qmd` files.** The user always does this after edits are made.
- After making edits to any `.R` or `.qmd` file, ask the user to run/render it and provide the exact command.
