
load_raw_data <- function(file_path) {
  
  log_step('RAW Ingestion', {
    df <- tibble::as_tibble(data.table::fread(input = file_path,
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
          ))
  })

  cat("\n ICI \n")

  print(class(df))

  print(names(attributes(df)))

   #cat("\n ip: ")
   #.Internal(inspect(df$ip))
   #
   # cat("\n ts: ")
   #.Internal(inspect(df$ts))
   #
   # cat("\n target: ")
   #.Internal(inspect(df$target))
   #
   # cat("\n status: ")
   #.Internal(inspect(df$status))
   #
   # cat("\n ua: ")
   #.Internal(inspect(df$ua))

  log_step("Date mutation", {
    df <- df %>%
      mutate(
        date = as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")
      )

  })

  log_step('Selection', {
    df <- df %>%  select(ip, date, target, status, ua)
  })

  log_step('Filtering', {
    df <- df %>% filter(
        !is.na(date),
        !is.na(target),
        !is.na(status),
        status == 200
      )
  })

  log_step('Col Drop', {
    df <- df %>% select(-status)
  })

  df

}


