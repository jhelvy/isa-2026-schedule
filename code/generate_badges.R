# code/generate_badges.R
# Generates print-ready ISA 2026 name badge PDF
# Layout: 6 badges per page (2 cols × 3 rows), each 4.25" × 3.67" on US Letter

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)

# ---- Configuration -------------------------------------------------------

CONF_NAME <- "2026 ISA Annual Conference"
CONF_DATES <- "June 3–5  ·  Washington, DC"
BANNER_COL <- "#1e3a5f"
PAGE_W <- 8.5
PAGE_H <- 11.0
BADGE_COLS <- 2L
BADGE_ROWS <- 3L

# ---- Load data -----------------------------------------------------------

registrants <- read_csv("data/registrants.csv", show_col_types = FALSE)
guests      <- read_csv("data/guests.csv",      show_col_types = FALSE)

# ---- Prepare registrants --------------------------------------------------

registrants_prep <- registrants |>
  transmute(name = str_trim(name), institution = replace_na(institution, ""))

# ---- Prepare guests -------------------------------------------------------

guests_prep <- guests |>
  transmute(name = str_trim(name), institution = replace_na(affiliation, ""))

# ---- Combine and parse names ----------------------------------------------
# Split on last whitespace: last token = last name, rest = first name

all_badges <- bind_rows(registrants_prep, guests_prep) |>
  distinct(name, .keep_all = TRUE) |>
  arrange(name) |>
  mutate(
    last_name = word(name, -1L),
    first_name = str_trim(str_remove(name, "\\s+\\S+$"))
  )

message(sprintf(
  "Preparing %d badges (%d registrants, %d guests)",
  nrow(all_badges),
  nrow(registrants_prep),
  nrow(guests_prep)
))

# ---- Load and prepare logo -----------------------------------------------
# Make near-white pixels transparent so logo sits on the dark banner

logo_raw <- readPNG("images/logo.png")
if (dim(logo_raw)[3L] >= 3L) {
  is_near_white <- logo_raw[,, 1L] > 0.9 &
    logo_raw[,, 2L] > 0.9 &
    logo_raw[,, 3L] > 0.9
  if (dim(logo_raw)[3L] == 3L) {
    logo <- array(
      c(logo_raw, array(1, dim(logo_raw)[1:2])),
      dim = c(dim(logo_raw)[1:2], 4L)
    )
  } else {
    logo <- logo_raw
  }
  logo[,, 4L][is_near_white] <- 0
}

# Logo is 500×200 px (2.5:1 width:height).
# In normalized badge coords (x: 0–1 = 4.25", y: 0–1 = 3.67"):
#   logo_h_units = desired height in y-units
#   logo_w_units = logo_h * (3.67/4.25) * 2.5  (preserves aspect ratio)
logo_h <- 0.15
logo_w <- logo_h * (3.67 / 4.25) * 2.5 # ≈ 0.324

# White backing rect for logo (with a small margin around the logo)
logo_pad  <- 0.01
logo_xmin <- 0.03
logo_xmax <- 0.03 + logo_w
logo_ymin <- 0.05
logo_ymax <- 0.05 + logo_h

# ---- Name font-size helper -----------------------------------------------
# Scales down for names longer than ~14 chars so they stay within the badge.

fit_name_size <- function(name, base = 10.5, threshold = 14L) {
  n <- nchar(name)
  if (n <= threshold) return(base)
  max(base * threshold / n, 5.5)
}

# ---- Badge drawing function ----------------------------------------------

make_badge <- function(first, last, institution) {
  ggplot() +
    # Dark blue banner (bottom 28%)
    annotate(
      "rect",
      xmin = 0, xmax = 1, ymin = 0, ymax = 0.28,
      fill = BANNER_COL, color = NA
    ) +
    # White backing so the blue ISA logo is visible on the dark banner
    annotate(
      "rect",
      xmin = logo_xmin - logo_pad,
      xmax = logo_xmax + logo_pad,
      ymin = logo_ymin - logo_pad,
      ymax = logo_ymax + logo_pad,
      fill = "white", color = NA
    ) +
    # ISA logo
    annotation_raster(
      logo,
      xmin = logo_xmin, xmax = logo_xmax,
      ymin = logo_ymin, ymax = logo_ymax
    ) +
    # Banner: conference name (right of logo)
    annotate(
      "text",
      x = 0.68, y = 0.205, hjust = 0.5, vjust = 0.5,
      label = CONF_NAME,
      size = 3.0, fontface = "bold", color = "white"
    ) +
    # Banner: dates & location
    annotate(
      "text",
      x = 0.68, y = 0.085, hjust = 0.5, vjust = 0.5,
      label = CONF_DATES,
      size = 2.6, color = "white"
    ) +
    # First name (large, bold — scaled for long names)
    annotate(
      "text",
      x = 0.5, y = 0.76, hjust = 0.5, vjust = 0.5,
      label = first,
      size = fit_name_size(first), fontface = "bold", color = "black"
    ) +
    # Last name (large, bold — scaled for long names)
    annotate(
      "text",
      x = 0.5, y = 0.60, hjust = 0.5, vjust = 0.5,
      label = last,
      size = fit_name_size(last), fontface = "bold", color = "black"
    ) +
    # Institution
    annotate(
      "text",
      x = 0.5, y = 0.44, hjust = 0.5, vjust = 0.5,
      label = str_wrap(institution, width = 35L),
      size = 4.0, color = "#333333", lineheight = 1.15
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "#bbbbbb", linewidth = 0.4),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# ---- QR code back-page setup ---------------------------------------------

qr_img <- readPNG("images/qr-schedule.png")

# QR code is square. Preserve 1:1 aspect in normalized badge coords
# (1 x-unit = 4.25", 1 y-unit = 3.67").
qr_h <- 0.65
qr_w <- qr_h * (3.67 / 4.25)  # ≈ 0.561

make_qr_badge <- function() {
  ggplot() +
    annotation_raster(
      qr_img,
      xmin = 0.5 - qr_w / 2, xmax = 0.5 + qr_w / 2,
      ymin = 0.5 - qr_h / 2, ymax = 0.5 + qr_h / 2
    ) +
    annotate(
      "text",
      x = 0.5, y = 0.08, hjust = 0.5, vjust = 0.5,
      label = "Scan for full schedule",
      size = 3.0, color = "gray40"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "#bbbbbb", linewidth = 0.4),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# Helper: render a list of n_per_page plots into the 2×3 viewport grid
draw_grid <- function(plots) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(BADGE_ROWS, BADGE_COLS)))
  for (i in seq_len(n_per_page)) {
    row <- ceiling(i / BADGE_COLS)
    col <- (i - 1L) %% BADGE_COLS + 1L
    pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
    print(plots[[i]], newpage = FALSE)
    popViewport()
  }
  popViewport()
}

# ---- Render to PDF -------------------------------------------------------

n_per_page <- BADGE_COLS * BADGE_ROWS
n_total    <- nrow(all_badges)
n_pages    <- ceiling(n_total / n_per_page)

# Pre-build the QR back page (same for every sheet)
qr_back <- replicate(n_per_page, make_qr_badge(), simplify = FALSE)

pdf("badges.pdf", width = PAGE_W, height = PAGE_H, onefile = TRUE)

for (pg in seq_len(n_pages)) {
  i1      <- (pg - 1L) * n_per_page + 1L
  i2      <- min(pg * n_per_page, n_total)
  page_df <- all_badges[i1:i2, ]

  # Pad last page to fill the grid
  if (nrow(page_df) < n_per_page) {
    n_blank <- n_per_page - nrow(page_df)
    page_df <- bind_rows(
      page_df,
      tibble(
        first_name  = rep("", n_blank),
        last_name   = rep("", n_blank),
        institution = rep("", n_blank)
      )
    )
  }

  front <- pmap(
    list(
      first       = page_df$first_name,
      last        = page_df$last_name,
      institution = page_df$institution
    ),
    make_badge
  )

  draw_grid(front)   # front: badge faces
  draw_grid(qr_back) # back:  QR codes (print duplex to align with fronts)
}

# Extra blank sheet for walk-in badges (front + QR back)
blank_front <- replicate(n_per_page, make_badge("", "", ""), simplify = FALSE)
draw_grid(blank_front)
draw_grid(qr_back)

dev.off()

message(sprintf(
  "Done: badges.pdf  (%d named badges + 6 blanks, %d pages total)",
  n_total, n_pages * 2L + 2L
))

# ---- Report: names missing an affiliation --------------------------------

no_affil <- all_badges |>
  filter(institution == "") |>
  select(name)

if (nrow(no_affil) == 0L) {
  message("All badges have an affiliation.")
} else {
  message(sprintf("\n%d badge(s) have no affiliation — add manually:\n", nrow(no_affil)))
  walk(no_affil$name, ~ message("  ", .x))
}
