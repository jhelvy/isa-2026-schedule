---
name: remove-paper
description: Handle paper removal and session reallocation for the ISA 2026 schedule. Invoke with a paper ID to determine whether reallocation is needed and carry out the correct edits across all three data files.
metadata:
  version: "1.0.0"
---

# Remove Paper: Session Reallocation Decision Logic

You are handling a paper withdrawal or removal from the ISA 2026 schedule. The user has provided a paper ID. Follow every step below in order. Do not skip steps.

## Step 1 — Identify the paper and its session

Read `data/schedule.csv` and `data/session-ids.csv`. Find the row(s) with the given paper ID and record:

- `paper_id` — the ID being removed
- `session_id` — the session this paper belongs to
- `session_name` — the session name
- `time_slot` — e.g. T1, W2, F1
- `date` — e.g. 2026-06-04
- `day` — Wednesday / Thursday / Friday
- `room`

Also identify which category CSV in `data/sessions/` this paper appears in (match on `id` column).

## Step 2 — Count papers in the affected session

Count how many rows in `data/schedule.csv` share the same `session_id` as the paper being removed (including the paper itself, before removal).

- **4 papers → session will have 3 after removal**: No reallocation needed. Proceed to Step 6 (simple removal).
- **3 papers → session will have 2 after removal**: Reallocation is required. Proceed to Step 3.
- **Any other count**: Surface the anomaly to the user before proceeding.

## Step 3 — Check config.R constraints for the two remaining papers

Read `code/config.R`. For each of the two remaining papers in the now-undersized session, check:

- `date_restrictions`: does the paper have a required date? Record it.
- `date_exclusions`: does the paper have an excluded date? Record it.
- Check `data/pdw-registered.csv` (match on `Email` column against author emails in `data/submissions.csv`): if any author of a remaining paper is a PDW registrant, that paper cannot be in W1 or W2.

Summarize constraints for each remaining paper before choosing a reallocation strategy.

## Step 4 — Decide: Dissolve or Merge

You have two options. Choose based on what produces the cleanest result with the fewest file changes and the fewest constraint violations.

### Option A — Dissolve the session
Move both remaining papers into two different existing sessions that currently have exactly 3 papers, bringing each to 4.

**Candidate sessions must satisfy ALL of the following:**
1. Currently have exactly 3 papers in `data/schedule.csv`.
2. Same `date` (prefer) or at minimum the same `day` as the dissolved session.
3. Same category (i.e., same source CSV in `data/sessions/`) as the paper being moved into it — **strongly prefer this**; only cross categories if no same-category option exists.
4. No author overlap: none of the authors of the paper being moved appear in the target session (check `data/submissions.csv` `author_names` columns for both papers in the same time slot).
5. No `date_restrictions` or `date_exclusions` violations for the paper being moved.
6. If the target slot is W1 or W2, no PDW registrant among the moved paper's authors.

### Option B — Pull a paper from a 4-paper session
Find one existing session with exactly 4 papers. Pull one of its papers into the undersized session, giving two sessions of 3 each.

**The donor session must satisfy ALL of the following:**
1. Currently has exactly 4 papers.
2. Same `date` (prefer) or same `day` as the undersized session.
3. Same category as the undersized session — strongly prefer.

**The pulled paper must satisfy ALL of the following:**
1. No `date_restrictions` or `date_exclusions` violations in its new slot.
2. No author overlap with the remaining papers in the destination session.
3. If destination slot is W1 or W2, no PDW registrant among the pulled paper's authors.

### How to choose between A and B

- Prefer the option that keeps moves within the **same category**.
- If both are equally valid, prefer **Option B** (fewer moves = less disruption).
- If neither option has any valid same-category candidate, expand the search to adjacent categories and explain this to the user before proceeding.
- If no valid reallocation exists at all, surface this to the user with a clear explanation of which constraints block every candidate.

## Step 5 — Present the plan to the user before making any edits

State clearly:
- Which option (A or B) you chose and why.
- Exactly which papers move where (paper ID, from session → to session, from slot/room → to slot/room if changing).
- Any constraint that was borderline or worth noting.

Ask the user to confirm before proceeding to edits.

## Step 6 — Execute the edits

For a **simple removal** (session had 4 papers, now has 3):

1. **`data/sessions/<category>.csv`** — delete the row with the removed paper's `id`.
2. **`data/session-ids.csv`** — delete the row with the removed paper's `id`.
3. **`data/schedule.csv`** — delete the row(s) with the removed paper's `id`.
4. **`code/config.R`** — add the paper's ID to `withdraw_ids` (unless the user specifies a different list).

For a **reallocation** (session had 3 papers, now needs restructuring):

Perform the simple removal steps above, then for each paper being moved to a different session:

1. **`data/sessions/<category>.csv`** — update `session_id` and `session_name` to match the target session.
2. **`data/session-ids.csv`** — update `session_id` and `session_name`.
3. **`data/schedule.csv`** — update `session_id`, `session_name`, `time_slot`, `room`, `date`, `day`, `start_time`, `end_time`, and `time_order` to match the target session (copy these values from any other row in `data/schedule.csv` that belongs to the target session).

If dissolving the session entirely (Option A), also delete the now-empty session's rows from all three files (there will be none left after the removed paper and both moved papers are gone — just confirm no rows remain).

## Step 7 — Verify and prompt follow-up commands

After all edits are saved, tell the user to run the pipeline steps needed to propagate the changes:

```
Rscript code/3exclude.R
Rscript code/4generate_json.R
Rscript code/5verify.R
```

If any session groupings changed (papers moved between sessions), also prompt:
```
# After 3exclude.R completes, verify no constraints are violated:
Rscript code/5verify.R
```

Remind the user to re-render `schedule.qmd` if the PDF needs to be updated.

## Hard Rules

- **Never run R scripts or render .qmd files yourself.** Always give the user the exact command to run.
- **Never move a paper if it would violate a `date_restriction` or `date_exclusion` in `code/config.R`.**
- **Never place a PDW-registered author in W1 or W2.**
- **Never create a session with fewer than 3 or more than 4 papers.**
- **Always confirm the plan with the user before editing files.**
- **When in doubt about author identity, check `data/submissions.csv` for the full author list.**
