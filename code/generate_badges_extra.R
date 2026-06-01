# code/generate_badges_extra.R
# Generates badges PDF for late-added guests from data/extra.csv
# Same layout as generate_badges.R (Avery 5392, 4" × 3", duplex long-edge)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(png)

# ---- Configuration -------------------------------------------------------

CONF_NAME    <- "2026 ISA Annual Conference"
CONF_DATES   <- "June 3–5, 2026  ·  Washington, DC"
BANNER_COL   <- "#1e3a5f"
PAGE_W       <- 8.5
PAGE_H       <- 11.0
BADGE_W      <- 4.0
BADGE_H      <- 3.0
BADGE_COLS   <- 2L
BADGE_ROWS   <- 3L
MARGIN_SIDE  <- 0.25
MARGIN_TOP   <- 1.0

# ---- Load data -----------------------------------------------------------

extra <- read_csv("data/extra.csv", show_col_types = FALSE)

all_badges <- extra |>
  transmute(name = str_trim(name), institution = replace_na(affiliation, "")) |>
  distinct(name, .keep_all = TRUE) |>
  arrange(name) |>
  mutate(
    last_name  = word(name, -1L),
    first_name = str_trim(str_remove(name, "\\s+\\S+$"))
  )

message(sprintf("Preparing %d extra badges", nrow(all_badges)))

# ---- Load and prepare logo -----------------------------------------------

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

logo_h <- 0.13
logo_w <- logo_h * (BADGE_H / BADGE_W) * 2.5

logo_pad  <- 0.012
logo_xmin <- 0.5 - logo_w / 2
logo_xmax <- 0.5 + logo_w / 2
logo_ymin <- 0.24
logo_ymax <- logo_ymin + logo_h

# ---- Helpers -------------------------------------------------------------

fit_name_size <- function(name, base = 10.0, threshold = 14L) {
  n <- nchar(name)
  if (n <= threshold) return(base)
  max(base * threshold / n, 5.0)
}

# ---- Badge front ---------------------------------------------------------

make_badge <- function(first, last, institution) {
  ggplot() +
    annotate("rect",
      xmin = 0, xmax = 1, ymin = 0, ymax = 0.42,
      fill = BANNER_COL, color = NA
    ) +
    annotate("rect",
      xmin = logo_xmin - logo_pad, xmax = logo_xmax + logo_pad,
      ymin = logo_ymin - logo_pad, ymax = logo_ymax + logo_pad,
      fill = "white", color = NA
    ) +
    annotation_raster(logo,
      xmin = logo_xmin, xmax = logo_xmax,
      ymin = logo_ymin, ymax = logo_ymax
    ) +
    annotate("text",
      x = 0.5, y = 0.165, hjust = 0.5, vjust = 0.5,
      label = CONF_NAME, size = 4.5, fontface = "bold", color = "white"
    ) +
    annotate("text",
      x = 0.5, y = 0.075, hjust = 0.5, vjust = 0.5,
      label = CONF_DATES, size = 3.0, color = "white"
    ) +
    annotate("text",
      x = 0.5, y = 0.83, hjust = 0.5, vjust = 0.5,
      label = first, size = fit_name_size(first), fontface = "bold", color = "black"
    ) +
    annotate("text",
      x = 0.5, y = 0.68, hjust = 0.5, vjust = 0.5,
      label = last, size = fit_name_size(last), fontface = "bold", color = "black"
    ) +
    annotate("text",
      x = 0.5, y = 0.54, hjust = 0.5, vjust = 0.5,
      label = str_wrap(institution, width = 35L),
      size = 3.8, color = "#333333", lineheight = 1.15
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "#bbbbbb", linewidth = 0.4),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# ---- Badge back (QR + name) ----------------------------------------------

qr_img <- readPNG("images/qr-schedule.png")

qr_h <- 0.35
qr_w <- qr_h * (BADGE_H / BADGE_W)

make_qr_badge <- function(first = "", last = "") {
  full_name <- trimws(paste(first, last))

  p <- ggplot() +
    annotation_raster(qr_img,
      xmin = 0.5 - qr_w / 2, xmax = 0.5 + qr_w / 2,
      ymin = 0.30,            ymax = 0.30 + qr_h
    ) +
    annotate("text",
      x = 0.5, y = 0.20, hjust = 0.5, vjust = 0.5,
      label = "Scan for full schedule", size = 2.8, color = "gray40"
    )

  if (nchar(full_name) > 0L) {
    p <- p +
      annotate("text",
        x = 0.5, y = 0.88, hjust = 0.5, vjust = 0.5,
        label = full_name,
        size  = fit_name_size(full_name, base = 8.0, threshold = 20L),
        fontface = "bold", color = "black"
      )
  }

  p +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = "#bbbbbb", linewidth = 0.4),
      plot.margin = margin(0, 0, 0, 0)
    )
}

# ---- Grid renderer -------------------------------------------------------

draw_grid <- function(plots, mirror = FALSE) {
  n <- length(plots)
  grid.newpage()
  pushViewport(viewport(
    layout = grid.layout(
      nrow    = BADGE_ROWS + 2L,
      ncol    = BADGE_COLS + 2L,
      widths  = unit(c(MARGIN_SIDE, rep(BADGE_W, BADGE_COLS), MARGIN_SIDE), "inches"),
      heights = unit(c(MARGIN_TOP,  rep(BADGE_H, BADGE_ROWS), MARGIN_TOP),  "inches")
    )
  ))
  for (i in seq_len(n)) {
    grid_row <- ceiling(i / BADGE_COLS) + 1L
    grid_col <- if (mirror) {
      BADGE_COLS - (i - 1L) %% BADGE_COLS + 1L
    } else {
      (i - 1L) %% BADGE_COLS + 2L
    }
    pushViewport(viewport(layout.pos.row = grid_row, layout.pos.col = grid_col))
    print(plots[[i]], newpage = FALSE)
    popViewport()
  }
  popViewport()
}

# ---- Render to PDF -------------------------------------------------------

n_per_page <- BADGE_COLS * BADGE_ROWS
n_total    <- nrow(all_badges)
n_pages    <- ceiling(n_total / n_per_page)

pdf("badges/badges_extra.pdf", width = PAGE_W, height = PAGE_H, onefile = TRUE)

for (pg in seq_len(n_pages)) {
  i1      <- (pg - 1L) * n_per_page + 1L
  i2      <- min(pg * n_per_page, n_total)
  page_df <- all_badges[i1:i2, ]

  if (nrow(page_df) < n_per_page) {
    n_blank <- n_per_page - nrow(page_df)
    page_df <- bind_rows(
      page_df,
      tibble(first_name = rep("", n_blank), last_name = rep("", n_blank), institution = rep("", n_blank))
    )
  }

  front <- pmap(
    list(first = page_df$first_name, last = page_df$last_name, institution = page_df$institution),
    make_badge
  )
  back <- pmap(
    list(first = page_df$first_name, last = page_df$last_name),
    make_qr_badge
  )

  draw_grid(front)
  draw_grid(back, mirror = TRUE)
}

dev.off()

message(sprintf(
  "Done: badges/badges_extra.pdf  (%d badges, %d pages total)",
  n_total, n_pages * 2L
))

# ---- Report missing affiliations -----------------------------------------

no_affil <- all_badges |> filter(institution == "") |> select(name)
if (nrow(no_affil) == 0L) {
  message("All badges have an affiliation.")
} else {
  message(sprintf("\n%d badge(s) have no affiliation:\n", nrow(no_affil)))
  walk(no_affil$name, ~ message("  ", .x))
}
