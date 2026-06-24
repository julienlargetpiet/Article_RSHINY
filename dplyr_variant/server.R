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

    t <- Sys.time()

    # -----------------------------
    # TIME WINDOW FILTER
    # -----------------------------

    log_step("Time Window", {
      last <- input$last_n * mult_map[[input$time_unit]] * bench_index
      cutoff <- max(df$date) - last
  
      df <- df %>% filter(date >= cutoff)
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
      df <- df %>%
        filter(keep)
    })

    # Asset heuristic

    #css_clients <- df %>% 
    #        filter(endsWith(tolower(target), ".css")) %>%
    #        distinct(ip) %>%
    #        pull(ip)

    #df <- df %>% filter(ip %in% css_clients)

    log_step("Asset heuristic", {
      keep <- endsWith(tolower(df$target), ".css")
      css_clients <- df %>% 
              filter(keep) %>%
              distinct(ip) %>%
              pull(ip)

      keep <- df$ip %in% css_clients
      df <- df %>% filter(keep)
    })

    if (nrow(df) == 0) return(df)

    log_step("Article filtering", {
      keep <- grepl("^/articles/.*\\.html$", df$target, ignore.case=TRUE)
      df <- df %>%
        filter(keep)
    })

    if (nrow(df) == 0) return(df)

    # Rate heuristic
    #df <- df %>%
    #  group_by(ip, sec = floor_date(date, "second")) %>%
    #  mutate(req_per_sec = n()) %>%
    #  filter(req_per_sec < 10) %>%
    #  ungroup() %>%
    #  select(-sec, -req_per_sec)

    # Rate heuristic
    #df <- df %>%
    #  group_by(ip, sec = floor_date(date, "second")) %>%
    #  mutate(req_per_sec = n()) %>%
    #  ungroup() %>%
    #  filter(req_per_sec < 10) %>%
    #  select(-sec, -req_per_sec)

    #df <- df %>%
    #  mutate(sec = floor_date(date, "second")) %>%
    #  add_count(ip, sec, name = "req_per_sec") %>%
    #  filter(req_per_sec < 10) %>%
    #  select(-sec, -req_per_sec)

    log_step("Rate Heurstic", {
      df <- df %>%
        group_by(ip, sec = floor_date(date, "second")) %>%
        mutate(req_per_sec = n()) %>%
        ungroup()

      keep <- df$req_per_sec < 10

      df <- df %>%
        filter(keep) %>%
        select(-sec, -req_per_sec)
    })

    if (nrow(df) == 0) return(df)

    # Reading-time heuristic
    #df <- df %>%
    #  arrange(ip, date) %>%
    #  group_by(ip) %>%
    #  mutate(
    #    next_date = lead(date),
    #    time_on_page = as.numeric(difftime(next_date, date, units = "secs")),
    #    time_on_page = coalesce(time_on_page, -1)
    #  ) %>%
    #  ungroup() %>%
    #  filter(time_on_page == -1 | time_on_page > 5 & time_on_page < 3600) %>%
    #  select(-next_date)

    log_step("Read time heuristic", {
      df <- df %>%
        arrange(ip, date) %>%
        group_by(ip) %>%
        mutate(
          next_date = lead(date),
          time_on_page = as.numeric(difftime(next_date, date, units = "secs")),
          time_on_page = coalesce(time_on_page, -1)
        ) %>%
        ungroup()

      keep <- df$time_on_page == -1 | 
              (df$time_on_page > 5 & df$time_on_page < 3600)

      df <- df %>%
        filter(keep) %>%
        select(-next_date)

    })

    if (nrow(df) == 0) return(df)

    #--- ASN enrichment (minimal)

    log_step("ASN Enrichment", {
      ips <- sort(unique(df$ip))
  
      asn_data <- lookup_asns(ips, 
                              db_path = asn_db_path
      )

      df <- df %>% left_join(asn_data, by = "ip")
    })

    # cloud ASN repeated range burst

    #df <- df %>%
    #  arrange(date) %>%
    #  mutate(
    #    is_cloud_asn = grepl(cloud_asn_regex, asn_org, ignore.case = TRUE),
    #    asn_org_clean = coalesce(asn_org, "UNKNOWN_ASN"),
    #    ip_16 = sub("\\.[0-9]+\\.[0-9]+$", "", ip),
    #    asn_changed = asn_org_clean != lag(asn_org_clean, default = first(asn_org_clean)),
    #    asn_bucket = cumsum(asn_changed) + 1
    #  ) %>%
    #  group_by(asn_bucket, ip_16) %>%
    #  mutate(ip_16_occ = n()) %>%
    #  ungroup() %>%
    #  filter(ip_16_occ == 1 | !is_cloud_asn) %>%
    #  select(-asn_org_clean, 
    #         -ip_16, 
    #         -asn_changed, 
    #         -asn_bucket, 
    #         -ip_16_occ
    #  )

    log_step("ASN filtering 1", {
      df <- df %>%
        arrange(date) %>%
        mutate(
          is_cloud_asn = grepl(cloud_asn_regex, asn_org, ignore.case = TRUE),
          asn_org_clean = coalesce(asn_org, "UNKNOWN_ASN"),
          ip_16 = sub("\\.[0-9]+\\.[0-9]+$", "", ip),
          asn_changed = asn_org_clean != lag(asn_org_clean, default = first(asn_org_clean)),
          asn_bucket = cumsum(asn_changed) + 1
        ) %>%
        group_by(asn_bucket, ip_16) %>%
        mutate(ip_16_occ = n()) %>%
        ungroup()

      keep <- df$ip_16_occ == 1 | !df$is_cloud_asn

      df <- df %>%
        filter(keep) %>%
        select(-asn_org_clean, 
               -ip_16, 
               -asn_changed, 
               -asn_bucket, 
               -ip_16_occ
        )

    })

    #df <- df %>%
    #  arrange(date) %>%
    #  mutate(
    #    ip_24 = sub("\\.[0-9]+$", "", ip),
    #    half_hour_bucket = floor_date(date, unit="30 minutes") # ful date + hour
    #  ) %>%
    #  group_by(half_hour_bucket, ip_24) %>%
    #  mutate(ip_24_occ = n()) %>%
    #  ungroup() %>%
    #  filter(ip_24_occ == 1 | !is_cloud_asn) %>%
    #  select(-ip_24, 
    #         -ip_24_occ,
    #         -is_cloud_asn,
    #         -half_hour_bucket
    #  )

    log_step("'ASN filtering 2", {
      df <- df %>%
        arrange(date) %>%
        mutate(
          ip_24 = sub("\\.[0-9]+$", "", ip),
          half_hour_bucket = floor_date(date, unit="30 minutes") # ful date + hour
        ) %>%
        group_by(half_hour_bucket, ip_24) %>%
        mutate(ip_24_occ = n()) %>%
        ungroup()

      keep <- df$ip_24_occ == 1 | !df$is_cloud_asn

      df <- df %>%
        filter(keep) %>%
        select(-ip_24, 
               -ip_24_occ,
               -is_cloud_asn,
               -half_hour_bucket
        )

    })

    #df <- df %>% filter(!grepl(ip_exclude, ip))

    log_step("IP Exclusion", {
      keep <- !grepl(ip_exclude, df$ip)
      df <- df %>% filter(keep)
    })

    log_step("HONEY POTS", {
      keep <- df$target %in% honey_pots
      bad_ip <- df %>%
        filter(keep) %>%
        distinct(ip) %>%
        pull(ip)
   
      keep <- !(df$ip %in% bad_ip)
      df <- df %>%
        filter(keep)
    })

    df

  })  

  geo_cache_reactive <- reactiveVal(NULL)
  last_ips <- reactiveVal(character())
 
  geo_enriched_data <- reactive({
  
    df <- filtered_data()
    req(nrow(df) > 0)
  
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
        df <- df %>% left_join(geo, by = "ip")
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

    req(nrow(df) > 0)

    log_step("READTIME STATS", {
      keep <- df$time_on_page > 3 & df$time_on_page < 3600

      df <- df %>%
               filter(keep) %>%
               group_by(target) %>%
               summarise(median_readtime = median(time_on_page),
                         valid_reads = n(),
                         .groups = "drop") %>%
               arrange(desc(median_readtime))

    })

    #write_benchs()
    #bench_data <<- data.frame("seconds" = numeric(),
    #                          "nrows" = numeric(),
    #                          "name" = character()
    #                         )

    print(df)

    df

  })

  output$kpi_med_readtime <- renderText({
  
    df <- filtered_data()
 
    req(nrow(df) > 0)
 
    log_step("KPI MEDIAN READTIME", {
      keep <- !is.na(df$time_on_page) &
              df$time_on_page > 0 &
              df$time_on_page < 3600

      median_time <- df %>%
        filter(keep) %>%
        summarise(med = median(time_on_page)) %>%
        pull(med)
  
      if (is.na(median_time)) return("—")
  
      mins <- floor(median_time / 60)
      secs <- round(median_time %% 60)
    })

    session$onFlushed(function() {
        session$sendCustomMessage("reload_app", list(
          max = 9,
          delay = 500
        ))
     }, once = TRUE)

    sprintf("%02d:%02d", mins, secs)
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

    #agg <- df %>%
    #  count(target, name = "hits") %>%
    #  arrange(desc(hits))

    # OR
    agg <- df %>%
        group_by(target) %>%
        summarize(hits=n(), .groups="drop") %>%
        arrange(desc(hits))

    topn <- 5
    top <- head(agg, topn)

    if (nrow(agg) > topn) {
      other_hits <- sum(agg$hits[(topn + 1):nrow(agg)])
      top <- bind_rows(top,
                       tibble(target = "Other",
                              hits = other_hits))
    }

    plot_ly(
      data = top,
      labels = ~target,
      values = ~hits,
      type = "pie",
      textinfo = "label+percent",
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

    target_group <- df %>%
                    count(target, name="hits", sort = TRUE) %>%
                    head(n = 5) %>%
                    pull(target)

    df <- df %>%
      filter(target %in% target_group) %>%
      mutate(date_bucket = floor_date(date, unit = interval)) %>%
      count(target, date_bucket, name = "hits")
   
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
  
    df <- df %>%
      mutate(
        date = lubridate::with_tz(date, tzone = input$client_tz),
        date = format(date, "%Y-%m-%d %H:%M:%S")
      )
  
    datatable(
      df %>% 
        arrange(desc(date)) %>% 
        mutate(target = paste0(
          '<a href=\"https://julienlargetpiet.tech', 
          target,
          '\" target=\"_blank\">',
          target,
          "</a>"
        )) %>%
        select(country, asn_org, ip, date, target, time_on_page),
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
  
    stats <- article_readtime_stats()
    req(nrow(stats) > 0)
  
    stats <- stats %>%
      mutate(
        median_seconds = median_readtime,
        median_readtime = sprintf(
          "%02d:%02d",
          floor(median_readtime / 60),
          round(median_readtime %% 60)
        )
      )
  
    datatable(
      stats %>%
        select(target, median_readtime, valid_reads),
      options = list(
        pageLength = 20
      ),
      rownames = FALSE
    )
  })

  output$map <- renderLeaflet({
  
    df <- geo_enriched_data()
    req(nrow(df) > 0)
  
    df <- df %>%
      filter(!is.na(lat), !is.na(lon), !is.na(country))
  
    req(nrow(df) > 0)

    cat("\n MISSING COUNTRIES \n")

    missing_countries <- df %>%
      distinct(country) %>%
      left_join(country_coords, by = "country") %>%
      filter(is.na(country_lat) | is.na(country_lon)) %>%
      pull(country)
    
    cat(paste(missing_countries, collapse = "\n"), "\n")

    agg <- df %>%
      group_by(country) %>%
      summarise(
        hits = n(),
        unique_ips = n_distinct(ip),
        .groups = "drop"
      ) %>%
      left_join(country_coords, by="country") %>%
      filter(!is.na(country_lat), !is.na(country_lon))
  
    leaflet(agg) %>%
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



