library(pdftools)

# Run after rendering and hand-editing all three source files:
#   frontmatter.qmd → frontmatter.docx → (hand edit) → frontmatter.pdf
#   sessions.qmd    → sessions.pdf
#   endmatter.qmd   → endmatter.docx   → (hand edit) → endmatter.pdf

files <- c(
  "frontmatter.pdf",
  "sessions.pdf",
  "endmatter.pdf"
)

pdftools::pdf_combine(
  input = files,
  output = "schedule.pdf"
)
