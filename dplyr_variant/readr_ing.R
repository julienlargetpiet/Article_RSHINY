
load_raw_data <- function(file_path) {
  
  log_step('RAW Ingestion', {

    df <- readr::read_tsv(
      file = file_path,
      col_names = c("ip", "ts", "target", "status", "ua"),
      col_types = readr::cols(
        ip     = readr::col_character(),
        ts     = readr::col_double(),
        target = readr::col_character(),
        status = readr::col_integer(),
        ua     = readr::col_character()
      ),
      quote = "\"",
      show_col_types = FALSE,
      progress = FALSE
    )

  })

  cat("\n ICI \n")

  print(class(df))

  print(names(attributes(df)))

  #print(attr(df, "problems"))

  print(readr::problems(df))

  print(lobstr::obj_size(readr::problems(df)))

  print(lobstr::obj_size(readr::spec(df)))

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

