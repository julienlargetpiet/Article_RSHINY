library("ggplot2")

label <- c(
    "Raw Ingestion",
    "Date mutation",
    "Selection",
    "Filtering",
    "Col drop",
    #"Read First", 
    "Time Window", 
    "UA Agent Pre",
    "UA AGENT",      
    "Asset heuristic",
    "Article filtering",  
    "Rate heuristic", 
    "Read time heuristic",
    "ASN Enrichment",
    "ASN filtering 1",   
    "ASN filtering 2",  
    "IP Exclusion",  
    "HONEY POTS",     
    "KPI MEDIAN READTIME"
    #"READTIME STATS"
)

foreach_iter = 10
nb_iter <- 20

global_iter = foreach_iter * nb_iter

source(Sys.getenv("BENCH_PLOTS", ""))

nrows_files <- c()

elapsed_line <- vector("list", length(label))
system_line <- vector("list", length(label))
user_line <- vector("list", length(label))
max_ncells_line <- vector("list", length(label))
max_vcells_line <- vector("list", length(label))
current_ncells_line <- vector("list", length(label))
current_vcells_line <- vector("list", length(label))

tot_lines <- vector("list", nb_iter)

for (i in 1:length(label)) {
  elapsed_line[[i]] <- data.frame("nrows" = numeric(),
                                  "nrows_df" = numeric(),
                                  "val" = numeric()
                                 )

  system_line[[i]] <- data.frame("nrows" = numeric(),
                                 "nrows_df" = numeric(),
                                 "val" = numeric()
                                )
  user_line[[i]] <- data.frame("nrows" = numeric(),
                               "nrows_df" = numeric(),
                               "val" = numeric()
                              )
  max_ncells_line[[i]] <- data.frame("nrows" = numeric(),
                                     "nrows_df" = numeric(),
                                     "val" = numeric()
                                    )
  max_vcells_line[[i]] <- data.frame("nrows" = numeric(),
                                     "nrows_df" = numeric(),
                                     "val" = numeric()
                                    )
  current_ncells_line[[i]] <- data.frame("nrows" = numeric(),
                                         "nrows_df" = numeric(),
                                         "val" = numeric()
                                        )
  current_vcells_line[[i]] <- data.frame("nrows" = numeric(),
                                         "nrows_df" = numeric(),
                                         "val" = numeric()
                                        )
}

count_lines <- function(path) {
  out <- system2("wc", c("-l", shQuote(path)), stdout = TRUE)
  parts <- strsplit(trimws(out), "\\s+")[[1]]
  as.integer(parts[1])
}

for (I in 1:nb_iter) {

  tot_lines[[I]] <- data.frame("nrows_df" = numeric(),
                               "val" = numeric()
                              )

  speed_file <- paste0(speed_folder, "/", I, ".result")
  mem_file <- paste0(mem_folder, "/", I, ".result")

  data <- read.table(speed_file, 
                     sep = ",", 
                     header = FALSE
                    )

  data <- data[data[, ncol(data)] != "Read First", ]


  elapsed <- as.data.frame(matrix(data$V1, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
 
  cur_iter <- nrow(elapsed)

  user <- as.data.frame(matrix(data$V2, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
  
  system <- as.data.frame(matrix(data$V3, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
  
  n_rows <- as.data.frame(matrix(data$V4, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
  
  n_rows$V1 = data.table::shift(n_rows$V1, 
                                type="lag", 
                                fill=count_lines(paste0("logs/out", I, ".log"))
                               )
  
  
  data <- read.table(mem_file, 
                               sep = ",", 
                               header = FALSE
                              )

  data <- data[data[, ncol(data)] != "Read First", ]

  max_ncells <- as.data.frame(matrix(data$V1, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
  
  max_vcells <- as.data.frame(matrix(data$V2, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
  current_ncells <- as.data.frame(matrix(data$V3, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
  current_vcells <- as.data.frame(matrix(data$V4, 
                              ncol = length(label),
                              byrow=TRUE
                             )
                       )
  
  rows_log <- n_rows[1, 1]

  for (i in 1:length(label)) {
  
      elapsed_line[[i]] <- rbind(elapsed_line[[i]], 
                             data.frame("nrows" = rep(n_rows[1, i], cur_iter),
                                        "nrows_df" = rep(rows_log, cur_iter),
                                        "val" = elapsed[, i]
                                       )
                            )
 
      user_line[[i]] <- rbind(user_line[[i]], 
                             data.frame("nrows" = rep(n_rows[1, i], cur_iter),
                                        "nrows_df" = rep(rows_log, cur_iter),
                                        "val" = user[, i]
                                       )
                         )

      system_line[[i]] <- rbind(system_line[[i]], 
                             data.frame("nrows" = rep(n_rows[1, i], cur_iter),
                                        "nrows_df" = rep(rows_log, cur_iter),
                                        "val" = system[, i]
                                       )
                         )

      max_ncells_line[[i]] <- rbind(max_ncells_line[[i]], 
                             data.frame("nrows" = rep(n_rows[1, i], cur_iter),
                                        "nrows_df" = rep(rows_log, cur_iter),
                                        "val" = max_ncells[, i]
                                       )
                              )

      max_vcells_line[[i]] <- rbind(max_vcells_line[[i]], 
                             data.frame("nrows" = rep(n_rows[1, i], cur_iter),
                                        "nrows_df" = rep(rows_log, cur_iter),
                                        "val" = max_vcells[, i]
                                       )
                              )

      current_ncells_line[[i]] <- rbind(current_ncells_line[[i]], 
                             data.frame("nrows" = rep(n_rows[1, i], cur_iter),
                                        "nrows_df" = rep(rows_log, cur_iter),
                                        "val" = current_ncells[, i]
                                       )
                                  )

      current_vcells_line[[i]] <- rbind(current_vcells_line[[i]], 
                             data.frame("nrows" = rep(n_rows[1, i], cur_iter),
                                        "nrows_df" = rep(rows_log, cur_iter),
                                        "val" = current_vcells[, i]
                                       )
                                  )

      cat(paste("\n", label[i], "\n"))
  
  }

  tot_vals <-rowSums(elapsed[, 1:length(label)])
  
  tot_lines[[I]] <- rbind(tot_lines[[I]], 
                           data.frame("nrows_df" = rep(rows_log, cur_iter),
                                      "val" = tot_vals
                                     )
                         )
 
  cat(paste("\n\n", rows_log, "\n\n"))


}

names(elapsed_line) <- label
names(system_line) <- label
names(user_line) <- label
names(max_ncells_line) <- label
names(max_vcells_line) <- label
names(current_ncells_line) <- label
names(current_vcells_line) <- label

overlay_time_df <- vector("list", 3)
names(overlay_time_df) <- c("user", 
                            "system", 
                            "elapsed")

overlay_memory_df <- vector("list", 4)
names(overlay_memory_df) <- c("current_ncells", 
                              "current_vcells", 
                              "max_ncells",
                              "max_vcells")

for (i in 1:length(overlay_time_df)) {

    overlay_time_df[[i]] <- data.frame("nrows" = numeric(),
                                       "nrows_df" = numeric(),
                                       "val" = numeric(),
                                       "operation" = character()
                                      )

}

for (i in 1:length(overlay_memory_df)) {

    overlay_memory_df[[i]] <- data.frame("nrows" = numeric(),
                                         "nrows_df" = numeric(),
                                         "val" = numeric(),
                                         "operation" = character()
                                        )

}


for (i in 1:length(label)) {
    
    cur_op <- label[i]

    overlay_time_df$user <- rbind(overlay_time_df$user,
                                  cbind(user_line[[i]], operation = rep(cur_op, global_iter))
                                 )

    overlay_time_df$system <- rbind(overlay_time_df$system,
                                  cbind(system_line[[i]], operation = rep(cur_op, global_iter))
                                 )

    overlay_time_df$elapsed <- rbind(overlay_time_df$elapsed,
                                  cbind(elapsed_line[[i]], operation = rep(cur_op, global_iter))
                                 )

    overlay_memory_df$current_ncells <- rbind(overlay_memory_df$current_ncells,
                                              cbind(current_ncells_line[[i]], operation = rep(cur_op, global_iter))
                                         )

    overlay_memory_df$current_vcells <- rbind(overlay_memory_df$current_vcells,
                                              cbind(current_vcells_line[[i]], operation = rep(cur_op, global_iter))
                                        )

    overlay_memory_df$max_ncells <- rbind(overlay_memory_df$max_ncells,
                                          cbind(max_ncells_line[[i]], operation = rep(cur_op, global_iter))
                                    )

    overlay_memory_df$max_vcells <- rbind(overlay_memory_df$max_vcells,
                                          cbind(max_vcells_line[[i]], operation = rep(cur_op, global_iter))
                                    )


}

out_final_speed <- file.path("results_final", speed_folder)
dir.create(out_final_speed, showWarnings = FALSE, recursive = TRUE)

out_final_mem <- file.path("results_final", mem_folder)
dir.create(out_final_mem, showWarnings = FALSE, recursive = TRUE)

for (cur_metric in names(overlay_time_df)) {

  write.table(
    x = overlay_time_df[[cur_metric]],
    file = file.path(out_final_speed, paste0("overlay_time_", cur_metric, ".csv")),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )

}

for (cur_metric in names(overlay_memory_df)) {

  write.table(
    x = overlay_memory_df[[cur_metric]],
    file = file.path(out_final_mem, paste0("overlay_mem_", cur_metric, ".csv")),
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )

}


dir.create(out_plot, showWarnings = FALSE, recursive = TRUE)
dir.create(out_plot_mem, showWarnings = FALSE, recursive = TRUE)

safe_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+$", "", x)
  x
}

draw_boxplot <- function(df, title, ylab, outfile) {

  if (is.null(df) || nrow(df) == 0) {
    message("Skipping empty plot: ", title)
    return(invisible(NULL))
  }

  p <- ggplot(df, aes(x = nrows_df, y = val, color = nrows)) +
    geom_boxplot(aes(group = nrows_df)) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      level = 0.95
    ) +
    labs(
      title = title,
      x = "Initial dataframe rows",
      y = ylab
    ) +
    scale_x_continuous(
      labels = function(x) format(x, big.mark = " ", scientific = FALSE)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    filename = outfile,
    plot = p,
    width = 11,
    height = 7,
    dpi = 150
  )
}

for (i in seq_along(label)) {
  step_name <- label[i]
  file_name <- safe_name(step_name)

  draw_boxplot(
    elapsed_line[[i]],
    paste("Elapsed time -", step_name),
    "Elapsed time (seconds)",
    file.path(out_plot, paste0("elapsed_", file_name, ".png"))
  )

  draw_boxplot(
    user_line[[i]],
    paste("User CPU time -", step_name),
    "User CPU time (seconds)",
    file.path(out_plot, paste0("user_", file_name, ".png"))
  )

  draw_boxplot(
    system_line[[i]],
    paste("System CPU time -", step_name),
    "System CPU time (seconds)",
    file.path(out_plot, paste0("system_", file_name, ".png"))
  )
}

for (i in seq_along(label)) {
  step_name <- label[i]
  file_name <- safe_name(step_name)

  draw_boxplot(
    max_ncells_line[[i]],
    paste("Max Ncells -", step_name),
    "Max Ncells bytes",
    file.path(out_plot_mem, paste0("max_ncells_", file_name, ".png"))
  )

  draw_boxplot(
    max_vcells_line[[i]],
    paste("Max Vcells -", step_name),
    "Max Vcells bytes",
    file.path(out_plot_mem, paste0("max_vcells_", file_name, ".png"))
  )

  draw_boxplot(
    current_ncells_line[[i]],
    paste("Current Ncells -", step_name),
    "Current Ncells bytes",
    file.path(out_plot_mem, paste0("current_ncells_", file_name, ".png"))
  )

  draw_boxplot(
    current_vcells_line[[i]],
    paste("Current Vcells -", step_name),
    "Current Vcells bytes",
    file.path(out_plot_mem, paste0("current_vcells_", file_name, ".png"))
  )
}

 
draw_total_boxplot <- function(df, title, ylab, outfile) {

  p <- ggplot(df, aes(x = nrows_df, y = val)) +
    geom_boxplot(aes(group = nrows_df)) +
    geom_smooth(
       method="lm",
       se = TRUE,
       level = 0.85
    ) + 
    labs(
      title = title,
      x = "Initial dataframe rows",
      y = ylab
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    filename = outfile,
    plot = p,
    width = 11,
    height = 7,
    dpi = 150
  )
}

tot_df <- do.call(rbind, tot_lines)

draw_total_boxplot(
  tot_df,
  "Total pipeline elapsed time",
  "Total elapsed time (seconds)",
  file.path(out_plot, "total_pipeline_elapsed.png")
)

draw_boxplot_operations <- function(df, title, ylab, outfile) {

  df$operation <- factor(df$operation, levels = label)
  df$nrows_df <- factor(df$nrows_df, levels = sort(unique(df$nrows_df)))

  p <- ggplot(df, aes(x = operation, y = val, color = nrows)) +
    geom_boxplot(aes(group = interaction(operation, nrows_df)),
                 position = position_dodge(width = 0.8),
                 outlier.size = 0.2
                ) +
    labs(
      title = title,
      x = "Initial dataframe rows",
      y = ylab
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    filename = outfile,
    plot = p,
    width = 11,
    height = 7,
    dpi = 150
  )

}

for (i in 1:length(overlay_time_df)) {

  cur_metric <- names(overlay_time_df)[i]

  draw_boxplot_operations(overlay_time_df[[i]],
                          paste(cur_metric, "by operation by nrows_df"),
                          cur_metric,
                          file.path(out_plot, paste0(cur_metric, "_metric.png"))
                         )

}

for (i in 1:length(overlay_memory_df)) {

  cur_metric <- names(overlay_memory_df)[i]

  draw_boxplot_operations(overlay_memory_df[[i]],
                          paste(cur_metric, "by operation by nrows_df"),
                          cur_metric,
                          file.path(out_plot, paste0(cur_metric, "_metric.png"))
                         )

}











