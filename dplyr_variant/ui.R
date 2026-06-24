ui <- fluidPage(

  theme = bs_theme(
     version = 5,
     bootswatch = "litera",  
     base_font = font_google("Nunito"),
     code_font = font_google("Nunito")
  ),

  useShinyjs(),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('getTimezone', function(message) {
      var tz = Intl.DateTimeFormat().resolvedOptions().timeZone;
      Shiny.setInputValue('client_tz', tz, {priority: 'event'});
    });
  ")),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('reload_app', function(message) {
      const key = 'shiny_bench_reload_count';
  
      const maxReloads = message.max || 10;
      const delay = message.delay || 500;
  
      let count = parseInt(localStorage.getItem(key) || '0', 10);
  
      if (count < maxReloads) {
        localStorage.setItem(key, count + 1);
  
        setTimeout(function() {
          window.location.reload();
        }, delay);
      } else {
        localStorage.removeItem(key);
        console.log('Benchmark reload loop finished');

        Shiny.setInputValue('bench_finished', Math.random(), {priority: 'event'});

      }
    });
  ")),

  navset_tab(
    nav_panel(
      title = "Most Visited Pages",
      page_sidebar(
        title = "Main Dashboard",
        sidebar = tagList(
          selectInput(
            inputId = "time_unit",
            label = "Time Unit",
            choices = c("h", "d", "w", "m", "y"),
            selected = "h"
          ),
          numericInput(
            inputId = "last_n",
            label = "Last n units",
            value = 72,
            min = 1,
            step = 1
          ),
          fileInput(
            inputId = "upload_asn_mmdb",
            label = "Upload GeoLite2-ASN.mmdb",
            accept = c(".mmdb"),
            multiple = FALSE
          ),
          fileInput(
            inputId = "upload_city_mmdb",
            label = "Upload GeoLite2-City.mmdb",
            accept = c(".mmdb"),
            multiple = FALSE
          ),
          uiOutput("mmdb_status")
        ),

        # KPI row + pie
        layout_column_wrap(
          width = 1/3,
          value_box(title = "Total requests", value = textOutput("kpi_hits")),
          value_box(title = "Unique IPs", value = textOutput("kpi_ips")),
          value_box(title = "Unique pages", value = textOutput("kpi_pages")),
          value_box(title = "Median Read Time", value = textOutput("kpi_med_readtime"))
        ),

        value_box(
          title = NULL,
          value = withSpinner(plotlyOutput("pie_chart"), type = 5, size = 1.3)
        )
      )
    ),

    nav_panel(
      title = "WebPages Accross time",
      value_box(
        title = NULL,
        value = withSpinner(plotlyOutput("graph"), type = 5, size = 1.3)
      )
    ),

    nav_panel(
      title = "Data Page",
      card(
        withSpinner(DTOutput("mytable"), type = 5, size = 1.0)
      )
    ),

    nav_panel(
      title = "ReadTime Page",
      card(
        withSpinner(DTOutput("read_time"), type = 5, size = 1.0)
      )
    ),

    nav_panel(
      title = "Geo Map",
      card(
        withSpinner(
          leafletOutput("map", height = 650),
          type = 5,
          size = 1.2
        )
      )
    )

  )
)




