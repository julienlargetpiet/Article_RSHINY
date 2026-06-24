
load_raw_data <- function(file_path) {

    log_step("RAW Ingestion", {
     df <-  data.table::as.data.table(vroom::vroom(
        file_path,
        delim = "\t",
        col_names = c("ip", "ts", "target", "status", "ua"),
        col_types = vroom::cols(
          ip = vroom::col_character(),
          ts = vroom::col_double(),
          target = vroom::col_character(),
          status = vroom::col_integer(),
          ua = vroom::col_character()
        ),
        progress = FALSE
      ))
    })

    print(class(df))

    print(names(attributes(df)))

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



