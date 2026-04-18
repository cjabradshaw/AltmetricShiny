resolve_altmetric_api_key <- function(apikey = NULL) {
  candidate_keys <- c(
    apikey,
    Sys.getenv("ALTMETRIC_API_KEY", unset = ""),
    Sys.getenv("ALTMETRIC_KEY", unset = ""),
    getOption("altmetricKey", default = "")
  )
  candidate_keys <- trimws(as.character(candidate_keys))
  candidate_keys <- candidate_keys[!is.na(candidate_keys) & nzchar(candidate_keys)]

  if (!length(candidate_keys)) {
    stop(
      paste(
        "Altmetric now requires an API key.",
        "Enter one in the app or set ALTMETRIC_API_KEY (or ALTMETRIC_KEY) before starting the app."
      ),
      call. = FALSE
    )
  }

  candidate_keys[[1]]
}

altmetrics_with_key <- function(doi, apikey = NULL) {
  altmetrics(doi = doi, apikey = resolve_altmetric_api_key(apikey))
}

safe_altmetrics <- safely(altmetrics_with_key, otherwise = NULL)
