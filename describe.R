library(ggplot2)
library(data.table)

dir.create("describes", showWarnings = FALSE)

plot_df <- data.frame(
  log_files = numeric(),
  unique_ip = numeric(),
  unique_ts = numeric(),
  unique_target = numeric(),
  unique_status = numeric(),
  unique_ua = numeric()
)

for (i in 1:20) {

  file_path <- paste0("logs/out", i, ".log")

  tb <- data.table::fread(
    input = file_path,
    sep = "\t",
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

  cat("\nNEW\n")
  print(colnames(tb))
  print(paste("nrow:", nrow(tb)))

  print(data.table::uniqueN(tb$ip))
  print(data.table::uniqueN(tb$ts))
  print(data.table::uniqueN(tb$target))
  print(data.table::uniqueN(tb$status))
  print(data.table::uniqueN(tb$ua))

  plot_df <- rbind(
    plot_df,
    data.frame(
      log_files = nrow(tb),
      unique_ip = data.table::uniqueN(tb$ip),
      unique_ts = data.table::uniqueN(tb$ts),
      unique_target = data.table::uniqueN(tb$target),
      unique_status = data.table::uniqueN(tb$status),
      unique_ua = data.table::uniqueN(tb$ua)
    )
  )

  cat("\n\n")
}

cur_colnames <- setdiff(colnames(plot_df), "log_files")

for (cl in cur_colnames) {

  plt <- ggplot(plot_df, aes(x = log_files, y = .data[[cl]])) +
    geom_line() +
    geom_point() +
    labs(
      title = cl,
      x = "Number of rows",
      y = cl
    ) +
    theme_bw()

  ggsave(
    filename = paste0("describes/", cl, ".png"),
    plot = plt,
    width = 8,
    height = 5,
    dpi = 150
  )
}



