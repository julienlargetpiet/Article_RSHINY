
bench_data <- data.frame(
  elapsed = numeric(),
  user = numeric(),
  system = numeric(),
  nrows = numeric(),
  name = character()
)

log_step <- function(name, expr) {
  start <- proc.time()

  result <- eval.parent(substitute(expr))

  delta <- proc.time() - start

  elapsed <- delta[["elapsed"]]
  user <- delta[["user.self"]]
  system <- delta[["sys.self"]]

  nrows <- if (!is.null(result) && (is.data.frame(result) || data.table::is.data.table(result))) {
    nrow(result)
  } else {
    NA_integer_
  }

  bench_data <<- rbind(
    bench_data,
    data.frame(
      elapsed = elapsed,
      user = user,
      system = system,
      nrows = nrows,
      name = name
    )
  )

  result
}

speed_dir <- Sys.getenv("BENCH_VARIANT")

all_results_file <- c("1.result",
                      "2.result",
                      "3.result",
                      "4.result",
                      "5.result",
                      "6.result",
                      "7.result",
                      "8.result",
                      "9.result",
                      "10.result",
                      "11.result",
                      "12.result",
                      "13.result",
                      "14.result",
                      "15.result",
                      "16.result",
                      "17.result",
                      "18.result",
                      "19.result",
                      "20.result"
                     )

write_benchs <- function() {

    write.table(x = bench_data, 
                file = paste0(speed_dir, "/", all_results_file[bench_index]),
                sep = ",", 
                row.names = FALSE,
                col.names = FALSE,
                append = TRUE
               )

}


