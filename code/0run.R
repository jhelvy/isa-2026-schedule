source(here::here("code", "setup.R"))
source(here::here("code", "config.R"))
source(here::here("code", "1generate_session_ids.R"))
source(here::here("code", "2assign_slots.R"))
source(here::here("code", "3generate_json.R"))
source(here::here("code", "4verify.R"))

# Render the schedule.qmd file
quarto::quarto_render("schedule.qmd")

# servr::httd()

# Changes without needing to re-assign slots:
#  - Schedule structure (session_name, room, slot) → edit schedule.csv
#  - Paper content → edit submissions.csv
#  - Panel content → edit panels.csv directly
#  - Then just run steps 3 + render
