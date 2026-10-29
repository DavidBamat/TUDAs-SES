# Fetch NAEP 2024 TUDA results by SES index from the NAEP Data Service API
# and cache them to data/naep_tuda_ses.csv.
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

assessments <- tribble(
  ~subject,      ~grade, ~subscale,
  "mathematics", 4,      "MRPCM",
  "mathematics", 8,      "MRPCM",
  "reading",     4,      "RRPCM",
  "reading",     8,      "RRPCM"
)

# MN = mean scale score; RP = row percent (share of students in each SES level);
# ALC:AB / ALC:AP = percent at or above Basic / Proficient
stattypes <- c("MN:MN", "RP:RP", "ALC:AB", "ALC:AP")

# SESCAT = NAEP SES index (3 components), categorized: Low / Middle / High
# TOTAL  = all students (used for the overall district result)
variables <- c("SESCAT", "TOTAL")

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

raw <- assessments |>
  tidyr::crossing(stattype = stattypes, variable = variables) |>
  pmap(fetch_one) |>
  list_rbind()

naep <- raw |>
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

dir.create("data", showWarnings = FALSE)
write_csv(naep, "data/naep_tuda_ses.csv")
message("Wrote ", nrow(naep), " rows to data/naep_tuda_ses.csv")
