function(input, output, session) {

  observeEvent(input$time_unit, ignoreInit = TRUE, {
    updateSelectInput(session, "time_unit", selected = input$time_unit)
  })

  observeEvent(input$last_n, ignoreInit = TRUE, {
    updateNumericInput(session, "last_n", value = input$last_n)
  })

  mmdb_bump <- reactiveVal(0)

  observeEvent(input$upload_asn_mmdb, {
    req(input$upload_asn_mmdb)
  
    src <- input$upload_asn_mmdb$datapath
    dst <- asn_db_path
  
    ok <- file.copy(src, dst, overwrite = TRUE)
    if (!ok) {
      showNotification(paste("Failed to write:", dst), type = "error")
      return()
    }

    clear_ip_caches()
    geo_cache_reactive(NULL)
    last_ips(character())

    mmdb_bump(mmdb_bump() + 1)
    
    showNotification("ASN DB uploaded and installed.", type = "message")

  })
  
  observeEvent(input$upload_city_mmdb, {
    req(input$upload_city_mmdb)
  
    src <- input$upload_city_mmdb$datapath
    dst <- geo_db_path
  
    ok <- file.copy(src, dst, overwrite = TRUE)
    if (!ok) {
      showNotification(paste("Failed to write:", dst), type = "error")
      return()
    }
  
    clear_ip_caches()
    geo_cache_reactive(NULL)
    last_ips(character())

    mmdb_bump(mmdb_bump() + 1)

    showNotification("City DB uploaded and installed.", type = "message")

  })

  output$mmdb_status <- renderUI({

    mmdb_bump()
    asn_ok  <- file.exists(asn_db_path)
    city_ok <- file.exists(geo_db_path)
  
    tags$div(
      tags$small(
        HTML(paste0(
          "<b>ASN DB:</b> ", if (asn_ok) "✅ present" else "❌ missing",
          "<br><b>City DB:</b> ", if (city_ok) "✅ present" else "❌ missing"
        ))
      )
    )
  })

  observe({
    session$sendCustomMessage("getTimezone", list())
  })

  #log_step("Read First", {
  #  raw_data <- load_raw_data(file_path)
  #})

  raw_data <- load_raw_data(file_path)

  filtered_data <- reactive({

    mmdb_bump()

    req(input$time_unit, input$last_n)

    df <- raw_data
    req(nrow(df) > 0)

    # -----------------------------
    # TIME WINDOW FILTER
    # -----------------------------

    log_step("Time Window", {
      last <- input$last_n * mult_map[[input$time_unit]] * bench_index
      cutoff <- max(df$date) - last
  
      df <- df[date >= cutoff]
    })
    
    log_step("UA Agent Pre", {
      ua_unique <- unique(df$ua)
      
      ua_is_bot <- setNames(
        grepl(
          bot_regex,
          ua_unique,
          ignore.case = TRUE,
          perl = TRUE
        ),
        ua_unique
      )

      keep <- !ua_is_bot[df$ua]

    })

    log_step("UA Agent", {
      df <- df[keep]
    })

    # Asset heuristic

    #css_clients <- df[endsWith(tolower(target), ".css"), ip]
    #css_clients <- unique(css_clients)

    #css_clients <- df[endsWith(tolower(target), ".css")]
    #css_clients <- unique(css_clients, by="ip")
    #css_clients <- css_clients$ip

    #df <- df[
    #  ,
    #  if (any(endsWith(tolower(target), ".css"))) .SD else NULL,
    #  by = ip
    #]
    #df <- df[
    #  ,
    #  if (any(endsWith(tolower(target), ".css"))) .SD,
    #  by = ip
    #]

    log_step("Asset heuristic", {
      keep <- endsWith(tolower(df$target), ".css")
      css_clients <- df[keep, 
                        unique(ip)
                        ]
      keep <- df$ip %in% css_clients
      df <- df[keep]
    })

    if (nrow(df) == 0) return(df)

    log_step("Article filtering", {
      keep <- grepl("^/articles/.*\\.html$", df$target, ignore.case=TRUE)
      df <- df[keep]
    })

    if (nrow(df) == 0) return(df)

    # Rate heuristic

    #df[, sec := lubridate::floor_date(date, unit="second")]
    #df[, req_per_sec := .N, by = .(ip, sec)]
    #df <- df[req_per_sec < 10] 
    #df[, c("sec", "req_per_sec") := NULL]

    log_step("Rate heuristic", {
      df[, sec := lubridate::floor_date(date, unit="second")]
      df <- df[df[, .I[.N < 10], by = .(ip, sec)]$V1]
      df[, sec := NULL]
    })

    if (nrow(df) == 0) return(df)

    # Reading-time heuristic
    #cur_cmp <- function(x) {
    #  x == -1 | (x > 5 & x < 3600)
    #}    
    #data.table::setorder(df, ip, date)
    #df[, next_date := shift(date, type = "lead"), by = ip]
    #keep <- cur_cmp(data.table::fcoalesce(
    #  as.numeric(difftime(df$next_date, df$date, units = "secs")),
    #  -1
    #))
    #df <- df[keep] 
    #df[, next_date := NULL]

    log_step("Read time heuristic", {
      data.table::setorder(df, ip, date)
      df[, next_date := shift(date, type="lead"), by = ip]
      df[, time_on_page := data.table::fcoalesce(
                                          as.numeric(difftime(next_date, date, units = "secs")), 
                                          -1
                                       )
      ]
      keep <- df$time_on_page == -1 | (df$time_on_page > 5 & df$time_on_page < 3600)
      df <- df[keep]
      df[, next_date := NULL]
    })

    if (nrow(df) == 0) return(df)

    #--- ASN enrichment (minimal)

    log_step("ASN Enrichment", {
      ips <- sort(unique(df$ip))
  
      asn_data <- lookup_asns(ips, 
                              db_path = asn_db_path
      )

      df <- asn_data[df, on = "ip"] # left join
    })

    # cloud ASN repeated range burst

    log_step("ASN Filtering 1", {
      data.table::setorder(df, date) # sorts by ref
      df[, is_cloud_asn := grepl(cloud_asn_regex, asn_org, ignore.case = TRUE)]
      df[, asn_org_clean := data.table::fcoalesce(asn_org, "UNKNOWN_ASN")]
      df[, ip_16 := sub("\\.[0-9]+\\.[0-9]+$", "", ip)]
      df[, asn_changed := asn_org_clean != shift(asn_org_clean, 
                                                 type = "lag",
                                                 fill = first(asn_org_clean)
                                                )
      ]
      df[, asn_bucket := cumsum(asn_changed)]
      
      #df <- df[, ip_16_occ := .N, by = .(asn_bucket, ip_16)]
      #df <- df[ip_16_occ == 1 | !is_cloud_asn]

      #df <- df[, if (!first(is_cloud_asn) || .N == 1L) .SD, 
      #     by = .(asn_bucket, ip_16)
      #]

      keep <- df[, if (!first(is_cloud_asn) || .N == 1L) .I, 
                    by = .(asn_bucket, ip_16)
                 ]$V1

      df <- df[keep]

      df[, c("asn_org_clean",
             "ip_16",
             "asn_changed",
             "asn_bucket") := NULL
      ]
    })

    log_step("ASN Filtering 2", {
      df[, ip_24 := sub("\\.[0-9]+$", "", ip)]
      df[, half_hour_bucket := lubridate::floor_date(date, unit = "30 minutes")]
      
      #df[, ip_24_occ := .N, by = .(half_hour_bucket, ip_24)]
      #df <- df[ip_24_occ == 1 | !is_cloud_asn]

      df <- df[
               df[, if (.N == 1L) .I else .I[!is_cloud_asn], 
                  by = .(half_hour_bucket, ip_24)
                 ]$V1
      ]

      # Because scalar and vector logical operations can be combined
      # > FALSE | c(TRUE, FALSE)
      # [1]  TRUE FALSE
      # so we can do

      #df <- df[
      #         df[, .I[.N == 1L | !is_cloud_asn], 
      #            by = .(half_hour_bucket, ip_24)
      #           ]$V1
      #]

      df[, c("ip_24",
             "is_cloud_asn",
             "half_hour_bucket") := NULL
      ]

    })

    log_step("IP Exclusion", {
      keep <- !grepl(ip_exclude, df$ip)
      df <- df[keep]
    })

    log_step("HONEY POTS", {
      keep <- df$target %in% honey_pots
      bad_ip <- df[keep, unique(ip)]
      keep <- !(df$ip %in% bad_ip)
      df <- df[keep]
    })

    df

  })  

  geo_cache_reactive <- reactiveVal(NULL)
  last_ips <- reactiveVal(character())
 
  geo_enriched_data <- reactive({
  
    log_step("GEO Enrichment", {
      ips <- sort(unique(df$ip))
  
      if (!identical(ips, last_ips())) {
        geo_data <- lookup_ips(
          ips,
          db_path = geo_db_path
        )
  
        geo_cache_reactive(geo_data)
        last_ips(ips)
      }
  
      geo <- geo_cache_reactive()
  
      if (!is.null(geo)) {
        df <- geo[df, on = "ip"]
      }
    })

    df

  })

  output$kpi_hits <- renderText({
    df <- filtered_data()
    format(nrow(df), big.mark = " ") # big.mark -> spaces as thousands -> each 3 characters
  })

  output$kpi_ips <- renderText({
    df <- filtered_data()
    format(dplyr::n_distinct(df$ip), big.mark = " ")
  })

  output$kpi_pages <- renderText({
    df <- filtered_data()
    format(dplyr::n_distinct(df$target), big.mark = " ")
  })

  article_readtime_stats <- reactive({
 
    df <- filtered_data()

    log_step("READTIME STATS", {
      req(nrow(df) > 0)
 
      keep <- df$time_on_page > 3 & df$time_on_page < 3600
      df <- df[keep] 
      df <- df[, .(
             median_readtime = median(time_on_page),
             valid_reads = .N
            ), 
         by = target
      ]
      data.table::setorder(df, -median_readtime)
    })

    #write_benchs()
    bench_data <<- data.frame("seconds" = numeric(),
                              "nrows" = numeric(),
                              "name" = character()
                             )

    print(df)

    df

  })

  output$kpi_med_readtime <- renderText({
  
    df <- filtered_data()
 
    log_step("KPI MEDIAN READTIME", {
      req(nrow(df) > 0)

      keep <- !is.na(df$time_on_page) & 
              df$time_on_page > 0 & 
              df$time_on_page < 3600

      median_time <- df[keep, median(time_on_page)]

      if (is.na(median_time)) return("—")
  
      mins <- floor(median_time / 60)
      secs <- round(median_time %% 60)
    })

    result <- sprintf("%02d:%02d", mins, secs)

    session$onFlushed(function() {
       session$sendCustomMessage("reload_app", list(
         max = 9,
         delay = 500
       ))
    }, once = TRUE)

    result

  })

  observeEvent(input$bench_finished, {
     write_benchs()
     shiny::stopApp()
   })

  # Pie chart
  output$pie_chart <- renderPlotly({

    input$dark_mode

    df <- filtered_data()
    req(nrow(df) > 0)

    #df <- df[, .(hits = .N), by = target][order(-hits)] # .() as j = summary value
    # and not
    #df <- df[, hits := .N, by = target] 

    df <- df[, .(hits = .N), by = target]
    data.table::setorder(df, -hits)

    n <- nrow(df)
    topn <- 5L
    top <- df[seq_len(min(n, topn))]

    if (n > topn) {
        other_hits <- df[(topn + 1):n, sum(hits)]
        top <- data.table::rbindlist(
                    list(
                        top,
                        data.table::data.table(target="Other", hits = other_hits)
                    ),
                    use.names = TRUE, # bind columns by col name and not by position
                    fill = TRUE # NAs the cells of the missing cols
               )
    }

    plot_ly(
      data = top,
      labels = ~target,
      values = ~hits,
      type = "pie",
      textinfo = "none",
      insidetextorientation = "radial"
    ) %>%
      layout(
        template = "plotly_white",
        title = list(
          text = "Most visited targets (Top 5 + Other)"
        ),
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        showlegend = TRUE
      )
  })

  output$graph <- renderPlotly({

    df <- filtered_data()
    req(nrow(df) > 0)

    interval <- interval_map[[input$time_unit]]

    target_group <- df[, .(hits = .N), by = target]
    data.table::setorder(target_group, -hits)
    target_group <- target_group[min(5L, .N), target]

    df <- df[target %in% target_group]
    df[, date_bucket := lubridate::floor_date(date, unit = interval)]
    df <- df[, .(hits = .N), by = .(target, date_bucket)] # sumarizaton does not mutate in place

    plot_ly(
      data = df,
      x = ~date_bucket,
      y = ~hits,
      color = ~target,
      type = "scatter",
      mode = "lines+markers"
    ) 
  
  })

  output$mytable <- renderDT({
    df <- geo_enriched_data()
    req(input$client_tz)
  
    df[, date := format(
                        lubridate::with_tz(date, tzone = input$client_tz), 
                        "%Y-%m-%d %H:%M:%S"
                       )
    ]

    data.table::setorder(df, -date)
    df[, target := paste0(
          '<a href=\"https://julienlargetpiet.tech', 
          target,
          '\" target=\"_blank\">',
          target,
          "</a>"
        )
    ]
    datatable(
        df[, .(country,
               asn_org,
               ip,
               date,
               target,
               time_on_page
              )
        ],
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        ordering = TRUE
      ),
      rownames = FALSE,
      escape = FALSE
    )

  })

  output$read_time <- renderDT({
  
    df <- article_readtime_stats()
    req(nrow(df) > 0)
  
    df[, 
       median_readtime := sprintf("%02d:%02d",
                                  floor(median_readtime / 60),
                                  round(median_readtime %% 60)
                                 )
    ]

    datatable(
        df[, .(target, median_readtime, valid_reads)],
        options = list(
          pageLength = 20
        ),
        rownames = FALSE
    )

  })

  output$map <- renderLeaflet({
  
    df <- geo_enriched_data()
    req(nrow(df) > 0)
  
    df <- df[!is.na(lat) & !is.na(lon) & !is.na(country)]

    req(nrow(df) > 0)

    cat("\n MISSING COUNTRIES \n")

    missing_countries <- country_coords[
                                        unique(df[, .(country)]), 
                                        on = "country"
                         ][is.na(country_lat) | is.na(country_lon), country]

    cat(paste(missing_countries, collapse = "\n"), "\n")

    df <- df[, .(
                 hits = .N, 
                 unique_ips = data.table::uniqueN(ip)
               ), 
             by = country
    ]
    df <- country_coords[df, on = "country"]
    df <- df[!is.na(country_lat) & !is.na(country_lon)]

    leaflet(df) %>%
      addProviderTiles(
          providers$CartoDB.Positron
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        lng = ~country_lon,
        lat = ~country_lat,
        radius = ~pmin(25, pmax(5, sqrt(hits) * 3)),
        stroke = FALSE,
        fillOpacity = 0.75,
        popup = ~paste0(
          "<b>Country:</b> ", country, "<br>",
          "<b>Total hits:</b> ", hits, "<br>",
          "<b>Unique IPs:</b> <span style='color:red;'>", unique_ips, "</span>"
        ),
        clusterOptions = NULL
      )
  
  })

}



