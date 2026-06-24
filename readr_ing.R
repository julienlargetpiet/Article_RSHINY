
load_raw_data <- function(file_path) {

    log_step("RAW Ingestion", {
     df <-  data.table::as.data.table(readr::read_tsv(
        file_path,
        col_names = c("ip", "ts", "target", "status", "ua"),
        col_types = readr::cols(
          ip = readr::col_character(),
          ts = readr::col_double(),
          target = readr::col_character(),
          status = readr::col_integer(),
          ua = readr::col_character()
        ),
        progress = FALSE
      ))
    })

    log_step("Date mutation", {
      df[, date := as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")]
    })

    log_step("Selection", {
      data.table::setcolorder(df, c("ip", "ts", "target", "status", "ua"))
    })

    log_step("Filtering", {
      df <- df[!is.na(date) & 
               !is.na(target) & 
               !is.na(status) & 
               status == 200]

    })

    log_step("Status Drop", {
      df[, status := NULL]
    })

    df

}
