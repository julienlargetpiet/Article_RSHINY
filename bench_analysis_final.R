library("ggplot2")
library("data.table")

speed_metrics <- c(
  "elapsed",
  "user",
  "system"
)

mem_metrics <- c(
  "current_ncells",
  "current_vcells",
  "max_ncells",
  "max_vcells"
)

final_results <- "results_final/results"

config_map <- c(
  "dplyr_vroom"     = "dplyr_vroom_results",
  "dplyr_readr"     = "dplyr_readr_results",
  "dplyr_fread"     = "dplyr_fread_results",
  "datatable_vroom" = "datatable_vroom_results",
  "datatable_readr" = "datatable_readr_results",
  "datatable_fread" = "datatable_fread_results"
)

config_map_mem <- c(
  "dplyr_vroom"     = "dplyr_mem_vroom_results",
  "dplyr_readr"     = "dplyr_mem_readr_results",
  "dplyr_fread"     = "dplyr_mem_fread_results",
  "datatable_vroom" = "datatable_mem_vroom_results",
  "datatable_readr" = "datatable_mem_readr_results",
  "datatable_fread" = "datatable_mem_fread_results"
)

dir_out <- "final_plots"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

dir_out_speed <- file.path(dir_out, "speed")
dir.create(dir_out_speed, showWarnings = FALSE, recursive = TRUE)

dir_out_mem <- file.path(dir_out, "mem")
dir.create(dir_out_mem, showWarnings = FALSE, recursive = TRUE)

draw_boxplot_operations <- function(df, title, ylab, outfile) {
  
  df$operation <- factor(df$operation, levels = unique(df$operation))
  
  df$nrows_df <- factor(
    df$nrows_df,
    levels = sort(unique(df$nrows_df))
  )
  
  df$configuration <- factor(
    df$configuration,
    levels = unique(df$configuration)
  )
  
  p <- ggplot(df, aes(x = operation, y = val, fill = configuration, color = configuration)) +
    geom_boxplot(
      aes(group = interaction(operation, nrows_df, configuration)),
      position = position_dodge2(width = 0.9, preserve = "single"),
      outlier.size = 0.2
    ) +
    labs(
      title = title,
      x = "Operation",
      y = ylab,
      fill = "Configuration"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(
    filename = outfile,
    plot = p,
    width = 12,
    height = 7,
    dpi = 150
  )
}

read_overlay_metric <- function(config_name, folder_name, prefix, metric) {
  
  file_path <- file.path(
    final_results,
    folder_name,
    paste0(prefix, "_", metric, ".csv")
  )
  
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  
  df <- data.table::fread(file_path)
  
  df[, configuration := config_name]
  
  df
}

config_names <- names(config_map)

pairs <- combn(config_names, 2, simplify = FALSE)

for (metr in speed_metrics) {
 
  for (pair in pairs) {
    
    cf1 <- pair[1]
    cf2 <- pair[2]
    
    df1 <- read_overlay_metric(
      config_name = cf1,
      folder_name = config_map[[cf1]],
      prefix = "overlay_time",
      metric = metr
    )
    
    df2 <- read_overlay_metric(
      config_name = cf2,
      folder_name = config_map[[cf2]],
      prefix = "overlay_time",
      metric = metr
    )
    
    df <- data.table::rbindlist(
      list(df1, df2),
      use.names = TRUE,
      fill = TRUE
    )
    
    outfile <- file.path(
      dir_out_speed,
      paste0(cf1, "_vs_", cf2, "_", metr, ".png")
    )

    print(outfile)

    draw_boxplot_operations(
      df = df,
      title = paste(cf1, "vs", cf2, "-", metr),
      ylab = metr,
      outfile = outfile
    )
  }
}

for (metr in mem_metrics) {

  for (pair in pairs) {
    
    cf1 <- pair[1]
    cf2 <- pair[2]
    
    df1 <- read_overlay_metric(
      config_name = cf1,
      folder_name = config_map_mem[[cf1]],
      prefix = "overlay_mem",
      metric = metr
    )
    
    df2 <- read_overlay_metric(
      config_name = cf2,
      folder_name = config_map_mem[[cf2]],
      prefix = "overlay_mem",
      metric = metr
    )
    
    df <- data.table::rbindlist(
      list(df1, df2),
      use.names = TRUE,
      fill = TRUE
    )
    
    outfile <- file.path(
      dir_out_mem,
      paste0(cf1, "_vs_", cf2, "_", metr, ".png")
    )

    print(outfile)

    draw_boxplot_operations(
      df = df,
      title = paste(cf1, "vs", cf2, "-", metr),
      ylab = metr,
      outfile = outfile
    )
  }
}



