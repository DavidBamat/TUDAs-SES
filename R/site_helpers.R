# Shared data loading and chart helpers for the site pages.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(plotly)
})

naep <- read_csv(here::here("data/naep_tuda_ses.csv"), show_col_types = FALSE)

ses_levels <- c("Low SES", "Middle SES", "High SES")
reference_juris <- c("National public", "Large city")

# Ordinal blue ramp (light -> dark = low -> high SES)
ses_colors <- c("Low SES" = "#86b6ef", "Middle SES" = "#2a78d6", "High SES" = "#104281")

ink <- list(
  primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
  grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb", reference = "#898781"
)

stat_labels <- c(
  "MN:MN"  = "Average scale score",
  "ALC:AB" = "% at or above NAEP Basic",
  "ALC:AP" = "% at or above NAEP Proficient",
  "RP:RP"  = "% of students"
)

# Wide table: one row per jurisdiction x assessment x SES group with all stats
naep_wide <- naep |>
  select(subject, grade, jurisLabel, group, stattype, value) |>
  pivot_wider(names_from = stattype, values_from = value) |>
  rename(mean = `MN:MN`, pct = `RP:RP`, basic = `ALC:AB`, proficient = `ALC:AP`)

# Summary figures used in page text
gaps <- naep_wide |>
  filter(group %in% c("Low SES", "High SES")) |>
  select(subject, grade, jurisLabel, group, mean) |>
  pivot_wider(names_from = group, values_from = mean) |>
  mutate(gap = `High SES` - `Low SES`)
ref_gap <- function(j, s, g) round(gaps$gap[gaps$jurisLabel == j & gaps$subject == s & gaps$grade == g])
low_share <- function(j, s, g) round(naep_wide$pct[naep_wide$jurisLabel == j & naep_wide$subject == s & naep_wide$grade == g & naep_wide$group == "Low SES"])
n_tuda <- n_distinct(setdiff(naep$jurisLabel, reference_juris))
gap_range <- function(s, g) {
  x <- gaps$gap[gaps$subject == s & gaps$grade == g & !gaps$jurisLabel %in% reference_juris]
  round(range(x, na.rm = TRUE))
}

assessment_data <- function(subj, grd) {
  naep_wide |> filter(subject == subj, grade == grd)
}

# Pretty y-axis labels: bold the reference jurisdictions
axis_label <- function(x) {
  ifelse(x %in% reference_juris, paste0("<b>", x, "</b>"), x)
}

base_layout <- function(p, xtitle, height) {
  # Widget height (not layout height) controls the rendered size in Quarto
  p$height <- height
  p |>
    layout(
      font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif",
                  color = ink$secondary, size = 12),
      paper_bgcolor = ink$surface, plot_bgcolor = ink$surface,
      xaxis = list(title = xtitle, gridcolor = ink$grid, zeroline = FALSE,
                   linecolor = ink$axis, tickfont = list(color = ink$muted)),
      yaxis = list(title = "", gridcolor = "rgba(0,0,0,0)", automargin = TRUE,
                   tickfont = list(color = ink$primary)),
      legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom",
                    traceorder = "normal",
                    font = list(color = ink$primary)),
      margin = list(t = 40, r = 20, b = 50),
      hoverlabel = list(bgcolor = "white", font = list(color = ink$primary))
    ) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

# Dot plot: one row per district, one dot per SES level, connected by a
# hairline from lowest to highest. Rows sorted by the Low-SES value.
ses_dotplot <- function(subj, grd, stat = "mean") {
  d <- assessment_data(subj, grd) |>
    filter(group %in% ses_levels) |>
    mutate(value = .data[[stat]], group = factor(group, ses_levels))

  order <- d |>
    filter(group == "Low SES") |>
    arrange(value) |>
    pull(jurisLabel)
  # Districts with every SES value suppressed go to the bottom
  order <- c(setdiff(unique(d$jurisLabel), order), order)

  d <- d |>
    mutate(row = factor(axis_label(jurisLabel), axis_label(order))) |>
    arrange(row, group)

  span <- d |>
    filter(!is.na(value)) |>
    group_by(row) |>
    summarise(lo = min(value), hi = max(value), .groups = "drop")

  fmt <- if (stat == "mean") "%{x:.0f}" else "%{x:.0f}%"
  xtitle <- switch(stat, mean = stat_labels[["MN:MN"]],
                   basic = stat_labels[["ALC:AB"]],
                   proficient = stat_labels[["ALC:AP"]])

  p <- plot_ly() |>
    add_segments(data = span, x = ~lo, xend = ~hi, y = ~row, yend = ~row,
                 line = list(color = ink$axis, width = 2),
                 hoverinfo = "skip", showlegend = FALSE)

  for (g in ses_levels) {
    dg <- filter(d, group == g, !is.na(value))
    p <- p |>
      add_markers(
        data = dg, x = ~value, y = ~row, name = g,
        marker = list(color = ses_colors[[g]], size = 10,
                      line = list(color = ink$surface, width = 2)),
        customdata = ~jurisLabel, text = ~sprintf("%.0f%%", pct),
        hovertemplate = paste0("<b>%{customdata}</b><br>", g, ": ", fmt,
                               "<br>Share of students: %{text}<extra></extra>")
      )
  }

  base_layout(p, xtitle, height = 24 * nlevels(d$row) + 120) |>
    layout(yaxis = list(categoryorder = "array",
                        categoryarray = levels(d$row)))
}

# Horizontal bar chart of the High - Low SES gap in average scores
ses_gap_chart <- function(subj, grd) {
  d <- assessment_data(subj, grd) |>
    filter(group %in% c("Low SES", "High SES")) |>
    select(jurisLabel, group, mean) |>
    pivot_wider(names_from = group, values_from = mean) |>
    mutate(gap = `High SES` - `Low SES`) |>
    filter(!is.na(gap)) |>
    arrange(gap) |>
    mutate(row = factor(axis_label(jurisLabel), axis_label(jurisLabel)),
           color = ifelse(jurisLabel %in% reference_juris,
                          ink$reference, ses_colors[["Middle SES"]]))

  plot_ly(
    d, x = ~gap, y = ~row, type = "bar", orientation = "h",
    marker = list(color = ~color, line = list(color = ink$surface, width = 1)),
    customdata = ~jurisLabel,
    text = ~sprintf("High %.0f · Low %.0f", `High SES`, `Low SES`),
    textposition = "none",
    hovertemplate = paste0("<b>%{customdata}</b><br>Gap: %{x:.0f} points",
                           "<br>%{text}<extra></extra>")
  ) |>
    base_layout("High SES minus Low SES average score (points)",
                height = 24 * nrow(d) + 100) |>
    layout(showlegend = FALSE, bargap = 0.35)
}

# 100% stacked bars: share of each district's students in each SES level
ses_composition_chart <- function(subj, grd) {
  d <- assessment_data(subj, grd) |>
    filter(group %in% ses_levels) |>
    mutate(group = factor(group, ses_levels))

  order <- d |>
    filter(group == "Low SES") |>
    arrange(desc(pct)) |>
    pull(jurisLabel)
  order <- c(setdiff(unique(d$jurisLabel), order), order)

  d <- d |> mutate(row = factor(axis_label(jurisLabel), axis_label(order)))

  p <- plot_ly()
  for (g in ses_levels) {
    dg <- filter(d, group == g)
    p <- p |>
      add_bars(
        data = dg, x = ~pct, y = ~row, name = g, orientation = "h",
        marker = list(color = ses_colors[[g]],
                      line = list(color = ink$surface, width = 2)),
        customdata = ~jurisLabel,
        hovertemplate = paste0("<b>%{customdata}</b><br>", g,
                               ": %{x:.0f}% of students<extra></extra>")
      )
  }

  base_layout(p, "% of students", height = 24 * nlevels(d$row) + 120) |>
    layout(barmode = "stack", bargap = 0.3,
           xaxis = list(range = c(0, 100), ticksuffix = "%"),
           yaxis = list(categoryorder = "array", categoryarray = levels(d$row)))
}

# Downloadable table of every value on the site
naep_table <- function() {
  naep_wide |>
    mutate(group = factor(group, c("All students", ses_levels))) |>
    arrange(subject, grade, jurisLabel, group) |>
    transmute(
      Subject = subject, Grade = grade, Jurisdiction = jurisLabel, Group = group,
      `% of students` = round(pct), `Average score` = round(mean),
      `% at/above Basic` = round(basic), `% at/above Proficient` = round(proficient)
    ) |>
    DT::datatable(
      rownames = FALSE, filter = "top", extensions = "Buttons",
      options = list(pageLength = 25, dom = "Bfrtip", buttons = c("copy", "csv", "excel"))
    )
}
