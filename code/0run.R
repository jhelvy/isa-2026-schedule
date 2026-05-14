source(here::here("code", "setup.R"))
source(here::here("code", "config.R"))
source(here::here("code", "1generate_session_ids.R"))
source(here::here("code", "2assign_slots.R"))
source(here::here("code", "3generate_json.R"))
source(here::here("code", "4verify.R"))

# Render the detailed session program (PDF) and Word docs for hand editing
quarto::quarto_render("sessions.qmd")
quarto::quarto_render("frontmatter.qmd")
quarto::quarto_render("endmatter.qmd")

# After hand-editing frontmatter.docx and endmatter.docx and saving as PDFs,
# combine all three into the final schedule:
# source(here::here("code", "5combine_pdfs.R"))

# servr::httd()
# servr::daemon_stop(2)

# Changes without needing to re-assign slots:
#  - Schedule structure (session_name, room, slot) → edit schedule.csv
#  - Paper content → edit submissions.csv
#  - Panel content → edit panels.csv directly
#  - Then just run steps 3 + render
