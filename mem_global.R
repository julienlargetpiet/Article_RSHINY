
bench_data <- data.frame(
  max_ncells_bytes = numeric(),
  max_vcells_bytes = numeric(),
  current_ncells_bytes = numeric(),
  current_vcells_bytes = numeric(),
  nrows = numeric(),
  name = character()
)

log_step <- function(name, expr) {
  gc(reset = TRUE)

  result <- eval.parent(substitute(expr))

  gc_after <- gc()

  nrows <- if (!is.null(result) && (is.data.frame(result) || data.table::is.data.table(result))) {
    nrow(result)
  } else {
    NA_integer_
  }

  max_ncells_bytes <- gc_after["Ncells", "max used"] * 56
  max_vcells_bytes <- gc_after["Vcells", "max used"] * 8

  current_ncells_bytes <- gc_after["Ncells", "used"] * 56
  current_vcells_bytes <- gc_after["Vcells", "used"] * 8

  bench_data <<- rbind(
    bench_data,
    data.frame(
      max_ncells_bytes = data.table::fcoalesce(max_ncells_bytes, -1),
      max_vcells_bytes = data.table::fcoalesce(max_vcells_bytes, -1),
      current_ncells_bytes = data.table::fcoalesce(current_ncells_bytes, -1),
      current_vcells_bytes = data.table::fcoalesce(current_vcells_bytes, -1),
      nrows = nrows,
      name = name
    )
  )

  result
}

mem_dir <- Sys.getenv("BENCH_VARIANT")

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
                file = paste0(mem_dir, "/", all_results_file[bench_index]),
                sep = ",", 
                row.names = FALSE,
                col.names = FALSE,
                append = TRUE
               )

}
