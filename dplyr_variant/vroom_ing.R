
load_raw_data <- function(file_path) {
  
  log_step('RAW Ingestion', {

    df <- vroom::vroom(
      file = file_path,
      delim = "\t",
      col_names = c("ip", "ts", "target", "status", "ua"),
      col_types = vroom::cols(
        ip     = vroom::col_character(),
        ts     = vroom::col_double(),
        target = vroom::col_character(),
        status = vroom::col_integer(),
        ua     = vroom::col_character()
      ),
      quote = "\"",
      show_col_types = FALSE,
      progress = FALSE
    )

  })

   cat("\n ip: ")
.  Internal(inspect(df$ip))

   cat("\n ts: ")
.  Internal(inspect(df$ts))

   cat("\n target: ")
.  Internal(inspect(df$target))

   cat("\n status: ")
.  Internal(inspect(df$status))

   cat("\n ua: ")
.  Internal(inspect(df$ua))

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


