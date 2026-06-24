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

config_labels <- c(
  "dplyr_vroom"     = "vroom::vroom() + dplyr",
  "dplyr_readr"     = "readr::read_tsv() + dplyr",
  "dplyr_fread"     = "fread() + dplyr",
  "datatable_vroom" = "vroom::vroom() + data.table",
  "datatable_readr" = "readr::read_tsv() + data.table",
  "datatable_fread" = "fread() + data.table"
)

config_order <- c(
  "dplyr_readr",
  "dplyr_vroom",
  "dplyr_fread",
  "datatable_readr",
  "datatable_vroom",
  "datatable_fread"
)

speed_diag_folders <- c(
  "dplyr_readr"     = "plots/dplyr_readr_plots",
  "dplyr_vroom"     = "plots/dplyr_vroom_plots",
  "dplyr_fread"     = "plots/dplyr_fread_plots",
  "datatable_readr" = "plots/datatable_readr_plots",
  "datatable_vroom" = "plots/datatable_vroom_plots",
  "datatable_fread" = "plots/datatable_fread_plots"
)

mem_diag_folders <- c(
  "dplyr_readr"     = "plots/dplyr_readr_plots",
  "dplyr_vroom"     = "plots/dplyr_vroom_plots",
  "dplyr_fread"     = "plots/dplyr_fread_plots",
  "datatable_readr" = "plots/datatable_readr_plots",
  "datatable_vroom" = "plots/datatable_vroom_plots",
  "datatable_fread" = "plots/datatable_fread_plots"
)

PUBLIC_PREFIX <- "/assets/common_files/shiny_bench2"

out_dir <- "html_matrices"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

public_path <- function(path) {
  paste0(PUBLIC_PREFIX, "/", path)
}

pair_plot_path <- function(cf1, cf2, metric, kind) {
  
  dir <- if (kind == "speed") "final_plots/speed" else "final_plots/mem"

  p1 <- file.path(dir, paste0(cf1, "_vs_", cf2, "_", metric, ".png"))
  p2 <- file.path(dir, paste0(cf2, "_vs_", cf1, "_", metric, ".png"))

  if (file.exists(p1)) {
    p1
  } else if (file.exists(p2)) {
    p2
  } else {
    warning("No pair plot found for: ", cf1, " vs ", cf2, " metric=", metric, " kind=", kind)
    p1
  }
}

diag_plot_path <- function(cf, metric, kind) {
  if (kind == "speed") {
    folder <- speed_diag_folders[[cf]]
  } else {
    folder <- mem_diag_folders[[cf]]
  }

  file.path(folder, paste0(metric, "_metric.png"))
}

make_matrix_html <- function(metric, kind) {
  title <- paste0(kind, " / ", metric)

  lines <- character()

  lines <- c(lines,
    paste0('<h3>', html_escape(title), '</h3>'),
    '<div class="matrix-tabs" data-matrix-tabs>',
    '',
    '  <div class="matrix-tabs-header">',
    '  <div class="matrix-tabs-scroll">',
    paste0(
      '    <div class="matrix-tabs-grid" style="--matrix-cols: ',
      length(config_order),
      '; --matrix-row-header-width: 12em;">'
    )
  )

  lines <- c(lines, '      <div class="matrix-tabs-corner"></div>')

  for (cf in config_order) {
    lines <- c(
      lines,
      paste0('      <div class="matrix-tab-col">', html_escape(config_labels[[cf]]), '</div>')
    )
  }

  for (i in seq_along(config_order)) {
    row_cf <- config_order[i]

    lines <- c(
      lines,
      paste0('      <div class="matrix-tab-row">', html_escape(config_labels[[row_cf]]), '</div>')
    )

    for (j in seq_along(config_order)) {

      col_cf <- config_order[j]

      if (j < i) {

        lines <- c(
          lines,
          paste0(
            '      <button class="matrix-cell matrix-cell-disabled" disabled ',
            'data-row="', row_cf, '" data-col="', col_cf, '">×</button>'
          )
        )
      } else {
        active_class <- if (i == 1 && j == 1) " active" else ""

        label <- if (i == j) {
          config_labels[[row_cf]]
        } else {
          paste0(config_labels[[row_cf]], " × ", config_labels[[col_cf]])
        }

        lines <- c(
          lines,
          paste0(
            '      <button class="matrix-cell', active_class, '" ',
            'data-row="', row_cf, '" data-col="', col_cf, '">',
            html_escape(label),
            '</button>'
          )
        )
      }
    }
  }

  lines <- c(lines,
    '    </div>',
    '  </div>',
    '  </div>',
    ''
  )

  for (i in seq_along(config_order)) {
    row_cf <- config_order[i]

    for (j in seq_along(config_order)) {
      col_cf <- config_order[j]

      if (j < i) {
        next
      }

      active_class <- if (i == 1 && j == 1) " active" else ""

      img_path <- if (i == j) {
        diag_plot_path(row_cf, metric, kind)
      } else {
        pair_plot_path(row_cf, col_cf, metric, kind)
      }

      alt <- if (i == j) {
        paste0(metric, " - ", config_labels[[row_cf]])
      } else {
        paste0(metric, " - ", config_labels[[row_cf]], " vs ", config_labels[[col_cf]])
      }

      lines <- c(
        lines,
        paste0(
          '  <div class="matrix-tab-panel', active_class, '" ',
          'data-row="', row_cf, '" data-col="', col_cf, '">'
        ),
        '    <div class="matrix-loader" aria-hidden="true"></div>',
        paste0(
          '    <img class="matrix-img" src="',
          public_path(img_path),
          '" alt="',
          html_escape(alt),
          '">'
        ),
        '  </div>',
        ''
      )
    }
  }

  lines <- c(lines, '</div>', '')

  paste(lines, collapse = "\n")
}

all_html <- character()

for (metric in speed_metrics) {
  html <- make_matrix_html(metric, kind = "speed")

  out_file <- file.path(out_dir, paste0("matrix_speed_", metric, ".html"))
  writeLines(html, out_file)

  all_html <- c(all_html, html)
}

for (metric in mem_metrics) {
  html <- make_matrix_html(metric, kind = "mem")

  out_file <- file.path(out_dir, paste0("matrix_mem_", metric, ".html"))
  writeLines(html, out_file)

  all_html <- c(all_html, html)
}

writeLines(
  paste(all_html, collapse = "\n\n"),
  file.path(out_dir, "all_matrices.html")
)

cat("Generated HTML matrices in:", out_dir, "\n")



