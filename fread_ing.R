
load_raw_data <- function(file_path) {

    log_step("RAW Ingestion", {
      df <- data.table::fread(input = file_path,
                        sep="\t",
                        quote = "\"",
                        col.names = c("ip", "ts", "target", "status", "ua"),
                        header = FALSE,
                        colClasses = list(
                                          character = c(1, 3, 5),
                                          double = 2,
                                          integer = 4
                                         ),
                        showProgress = FALSE
            ) 

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


