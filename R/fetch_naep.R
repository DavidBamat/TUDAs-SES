# Fetch NAEP 2024 TUDA results by SES index from the NAEP Data Service API
# and cache them to data/naep_tuda_ses.csv (Low/Middle/High SES categories) and
# data/naep_tuda_ses_points.csv (results at each point of the SES index scale).
#
# Run from the project root:  Rscript R/fetch_naep.R
#
# API docs: https://www.nationsreportcard.gov/api_documentation.aspx

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)

api_url <- "https://www.nationsreportcard.gov/DataService/GetAdhocData.aspx"
year <- 2024

# National public (NP), Large city (NL), and the 26 TUDA districts assessed in 2024
jurisdictions <- c(
  "NP", "NL",
  "XQ", "XA", "XU", "XM", "XB", "XT", "XC", "XX", "XV", "XS", "XY",
  "XR", "XW", "XE", "XZ", "XG", "XH", "XJ", "XL", "XI", "XK", "XN",
  "XO", "XP", "XD", "YB"
)

# SES index variable by grade (NAEP reports these categorized as Low / Middle / High SES):
#   SESCAT  = SES index, 3 components (9-point scale); used for grade 4
#   SESCATP = SES index, 4 components incl. parental education (12-point scale);
#             available only at grades 8 and 12, so used for grade 8
# The point-scale (uncategorized) versions of the same indexes, called SES3VAL and
# SES4VAL on NAEP data files, are exposed by the API as:
#   SESCMP  = SES index, 3 components, index points 0-9; used for grade 4
#   SESCMPP = SES index, 4 components, index points 0-12; used for grade 8
assessments <- tribble(
  ~subject,      ~grade, ~subscale, ~ses_var,   ~ses_pts_var,
  "mathematics", 4,      "MRPCM",   "SESCAT",   "SESCMP",
  "mathematics", 8,      "MRPCM",   "SESCATP",  "SESCMPP",
  "reading",     4,      "RRPCM",   "SESCAT",   "SESCMP",
  "reading",     8,      "RRPCM",   "SESCATP",  "SESCMPP"
)

# MN = mean scale score; RP = row percent (share of students in each SES level);
# ALC:AB / ALC:AP = percent at or above Basic / Proficient
stattypes <- c("MN:MN", "RP:RP", "ALC:AB", "ALC:AP")


fetch_one <- function(subject, grade, subscale, stattype, variable) {
  message(sprintf("Fetching %s g%d %s %s", subject, grade, variable, stattype))
  resp <- request(api_url) |>
    req_url_query(
      type = "data",
      subject = subject,
      grade = grade,
      subscale = subscale,
      variable = variable,
      jurisdiction = paste(jurisdictions, collapse = ","),
      stattype = stattype,
      Year = year
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()

  # The service occasionally emits raw control characters inside strings
  body <- gsub("[[:cntrl:]]", " ", resp_body_string(resp))
  parsed <- fromJSON(body)
  if (!is.data.frame(parsed$result)) {
    stop("NAEP API error: ", substr(as.character(parsed$result), 1, 300))
  }
  as_tibble(parsed$result)
}

tidy_naep <- function(raw) {
  raw |>
    transmute(
      year,
      subject = recode(subject, MAT = "Mathematics", RED = "Reading"),
      grade,
      jurisdiction,
      jurisLabel,
      variable,
      group = if_else(variable == "TOTAL", "All students", varValueLabel),
      stattype,
      # 999 is the API's placeholder for suppressed estimates
      value = if_else(isStatDisplayable == 1, value, NA_real_),
      errorFlag
    )
}

dir.create("data", showWarnings = FALSE)

# Categorized index: for each assessment, fetch its SES variable plus TOTAL (all students)
naep <- assessments |>
  tidyr::crossing(stattype = stattypes, which = c("ses", "total")) |>
  mutate(variable = if_else(which == "ses", ses_var, "TOTAL")) |>
  select(subject, grade, subscale, stattype, variable) |>
  pmap(fetch_one) |>
  list_rbind() |>
  tidy_naep()

write_csv(naep, "data/naep_tuda_ses.csv")
message("Wrote ", nrow(naep), " rows to data/naep_tuda_ses.csv")

# Point-scale index: one row per index point (varValueLabel is the point, 0-9 or 0-12)
naep_points <- assessments |>
  tidyr::crossing(stattype = stattypes) |>
  select(subject, grade, subscale, stattype, variable = ses_pts_var) |>
  pmap(fetch_one) |>
  list_rbind() |>
  tidy_naep() |>
  rename(ses_points = group) |>
  mutate(ses_points = as.integer(ses_points)) |>
  arrange(subject, grade, jurisdiction, stattype, ses_points)

write_csv(naep_points, "data/naep_tuda_ses_points.csv")
message("Wrote ", nrow(naep_points), " rows to data/naep_tuda_ses_points.csv")
