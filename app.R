# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(leaflet)
library(bslib)
library(plotly)
library(shinyWidgets) 
library(tidyr)
library(purrr)

# --- Load and clean data ---
df <- readr::read_csv("data/water_quality_data.csv", show_col_types = FALSE)

# --- Clean and transform data ---
df <- df %>%
  rename(
    Depth_raw     = `Depth (cm)`,
    Temperature   = `Temperature (C)`,
    Salinity      = `Salinity (ppt)`,
    DO            = `Dissolved Oxygen (mg/L)`,
    Conductivity  = `Conductivity-specific (mS/cm)`,
    DO_percent    = `Dissolved Oxygen (%)`
  ) %>%
  mutate(
    Date = suppressWarnings(mdy(Date)),  # ✅ Parse Date here only
    DepthLayer = case_when(
      Site == "PIER" ~ Depth_raw,
      Depth_raw %in% c("Surface", "surface") ~ "Surface",
      Depth_raw %in% c("Bottom", "bottom") ~ "Bottom",
      suppressWarnings(as.numeric(Depth_raw)) <= 20 ~ "Surface",
      suppressWarnings(as.numeric(Depth_raw)) > 20 ~ "Bottom",
      TRUE ~ NA_character_
    ),
    Depth = suppressWarnings(as.numeric(Depth_raw)),
    Temperature = as.numeric(Temperature),
    Salinity = as.numeric(Salinity),
    DO = as.numeric(DO),
    Conductivity = as.numeric(Conductivity),
    DO_percent = as.numeric(DO_percent),
    Month = month(Date),
    Year = year(Date)
  ) %>%
  filter(
    Site %in% c("MO1", "CUL1", "VBR1", "PIER"),
    !is.na(Date),
    !is.na(Site)
  )

# --- Rename sites to friendly names ---
df <- df %>%
  mutate(Site = recode(Site,
                       "MO1"  = "Mouth",
                       "VBR1" = "Bridge",
                       "CUL1" = "Culvert",
                       "PIER" = "Pier"))

# Parameter choices for UI
param_choices <- c(
  "Temperature (C)"               = "Temperature",
  "Dissolved Oxygen (mg/L)"       = "DO",
  "Dissolved Oxygen (%)"          = "DO_percent",
  "Salinity (ppt)"                = "Salinity",
  "Conductivity-specific (mS/cm)" = "Conductivity"
)


# Site coordinates
site_locations <- tibble::tibble(
  Site = c("Mouth", "Culvert", "Bridge", "Pier"),
  Latitude = c(34.410235, 34.413928, 34.417485, 34.411906),
  Longitude = c(-119.878968, -119.873961, -119.874138, -119.877158)
)

# --- UCSB brand theme ---
ucsb_theme <- bs_theme(
  version = 5,
  primary   = "#003660",  # UCSB Navy
  secondary = "#FEBC11",  # UCSB Gold
  base_font    = font_google("Nunito Sans"),
  heading_font = font_google("Nunito Sans")
)

# UCSB CSS (+ colorful overview cards)
ucsb_css <- HTML("
  /* --- General font and color --- */
  body {
    font-family: 'Nunito Sans', sans-serif;
    color: #003660;
    background-color: #ffffff;
  }

  h1, h2, h3 {
    font-weight: 700;
    color: #003660;
  }

  h1, h2 {
    border-bottom: 2px solid #FEBC11;
    padding-bottom: 4px;
    margin-bottom: 15px;
  }

  /* --- Clean Top Banner --- */
  .top-banner {
    background-color: #003660;
    color: white;
    padding: 18px 30px;
    text-align: center;
    border-bottom: 3px solid #FEBC11;
  }

  .top-banner h1 {
    margin: 0;
    font-size: 2.2em;
    font-weight: 800;
    color: white;
  }

  /* --- Tabs --- */
  .nav-tabs {
    background-color: #F9F9F9;
    border: none;
    border-bottom: 2px solid #FEBC11;
  }

  .nav-tabs > li > a {
    color: #003660 !important;
    background-color: transparent !important;
    font-weight: 600;
    border: none !important;
    padding: 10px 16px;
  }

  .nav-tabs > li.active > a {
    background-color: #003660 !important;
    color: #ffffff !important;
    border-radius: 6px 6px 0 0;
  }

  .nav-tabs > li > a:hover {
    background-color: #FEBC11 !important;
    color: #003660 !important;
  }

  /* --- Sidebar --- */
  .ucsb-sidebar {
    background: #F9FBFD;
    border-left: 4px solid #FEBC11;
    border-radius: 4px;
    padding: 14px 16px;
    margin-bottom: 10px;
  }

  .ucsb-sidebar .ucsb-sidebar-header {
    font-weight: 800;
    color: #003660;
    text-transform: uppercase;
    margin-bottom: 10px;
    font-size: 0.9em;
  }

  .ucsb-sidebar label {
    color: #003660;
    font-weight: 600;
  }

  .ucsb-sidebar .btn-primary {
    background-color: #003660;
    border-color: #003660;
  }

  .ucsb-sidebar .irs-bar,
  .ucsb-sidebar .irs-single,
  .ucsb-sidebar .irs-handle > i:first-child {
    background: #003660;
    border-color: #003660;
  }

  .ucsb-sidebar .irs-line {
    background: #DCE7F3;
  }

  /* --- Hero Section --- */
  .hero {
    background: #003660;
    color: #ffffff;
    padding: 20px;
    border-radius: 8px;
    margin-bottom: 20px;
  }

  .hero h3 {
    color: white;
    margin: 0 0 6px 0;
    font-size: 1.4em;
  }

  /* --- Info Cards --- */
  .info-card {
    border-radius: 6px;
    padding: 14px;
    margin-bottom: 10px;
    border-left: 5px solid #DCE7F3;
    background: #F5F9FC;
  }

  .info-card.gold {
    border-left: 5px solid #FEBC11;
    background: #FFF9E3;
  }

  .info-card.blue {
    border-left: 5px solid #003660;
    background: #EDF4FA;
  }

  /* --- Badges --- */
  .badge {
    display: inline-block;
    padding: 6px 10px;
    margin: 4px 6px 0 0;
    font-weight: 700;
    border-radius: 999px;
    background: #003660;
    color: #fff;
  }

  .badge.badge-gold {
    background: #FEBC11;
    color: #003660;
  }

  /* --- Data notes --- */
  .data-notes {
    background: #F5F9FC;
    border-left: 4px solid #FEBC11;
    padding: 10px 15px;
    margin-top: 20px;
    border-radius: 6px;
    font-size: 0.95em;
  }
  
")


# UI
ui <- tagList(
  fluidPage(
    theme = ucsb_theme,
    tags$head(tags$style(ucsb_css)),
    div(class = "top-banner",
        h1("Devereux Slough Water Quality Explorer")
    ),
    
    tabsetPanel(
      # --- Overview (COPR / Devereux Slough) ---
      tabPanel("Overview",
               br(),
               div(class = "hero",
                   h3("Coal Oil Point Reserve — Devereux Slough Water Quality Monitoring"),
                   div("An interactive dashboard to explore long-term monitoring data and seasonal patterns across multiple sites and depths.")
               ),
               fluidRow(
                 column(
                   7,
                   div(class = "info-card gold",
                       h4("What you can do"),
                       tags$ul(
                         tags$li("Analyze parameters with units: Temperature (C), Salinity (ppt), Dissolved Oxygen (mg/L, %), Conductivity-specific (mS/cm)."),
                         tags$li("Filter by site, depth layer (or fixed pier depth), years, and months."),
                         tags$li("Switch the map between Street and Satellite views; inspect popups for coordinates.")
                       )
                   )
                 ),
                 column(
                   5,
                   div(class = "info-card blue",
                       h4("At a glance"),
                       div(class = "badge", "Sites: Mouth (MO1)"),
                       div(class = "badge", "Culvert (CUL1)"),
                       div(class = "badge", "Bridge (VBR1)"),
                       div(class = "badge", "Pier (PIER)"),
                       br(),
                       div(class = "badge badge-gold", "Surface & Bottom (non-PIER)"),
                       div(class = "badge badge-gold", "Fixed depths at PIER")
                   )
                 )
               ),
               br(),
               div(
                 class = "info-card blue",  
                 style = "margin: 0 auto; max-width: 900px; text-align: left; padding: 20px;",
                 h4("How to use this app", style = "text-align: center; margin-bottom: 20px;"),
                 tags$ol(
                   tags$li(style = "margin-bottom: 10px;", strong("Trends → Time Series:"), " select a parameter to view changes through time at a site and depth."),
                   tags$li(style = "margin-bottom: 10px;", strong("Trends → Seasonal Patterns:"), " compare monthly distributions across years."),
                   tags$li(style = "margin-bottom: 10px;", strong("Map:"), " locate monitoring sites and toggle basemaps.")
                 )
               ),
               br(),
               h4("Data Notes"),
               div(
                 class = "data-notes",
                 paste0(
                   "Dataset covers ",
                   format(min(df$Date, na.rm = TRUE), "%B %Y"), " to ",
                   format(max(df$Date, na.rm = TRUE), "%B %Y"),
                   " (", nrow(df), " observations across ",
                   length(unique(df$Site)), " sites). "
                 ),
                 tags$i(
                   paste0("Last updated: ",
                          format(max(df$Date, na.rm = TRUE), "%B %d, %Y"))
                 )
               ),
               br(),
               div(
                 class = "info-card blue",
                 style = "text-align: center; padding: 20px; margin-top: 20px;",
                 
                 h4("Download Citation", style = "margin-bottom: 20px; color: #003660; font-weight: 700;"),
                 
                 tags$div(
                   style = "display: flex; justify-content: center; flex-wrap: wrap; gap: 15px;",
                   
                   downloadButton("download_citation_txt", "Download Citation (.txt)", class = "btn btn-primary"),
                   downloadButton("download_citation_bib", "Download Citation (.bib)", class = "btn btn-primary"),
                   downloadButton("download_citation_ris", "Download Citation (.ris)", class = "btn btn-primary")
                 )
               ), 
               
               # --- Give page snippet ---
               div(
                 class = "info-card blue",
                 style = "background-color: #EDF4FA; padding: 20px; text-align: center; border-radius: 6px; margin: 20px 0;",
                 
                 # First paragraph
                 p("COPR interns have conducted weekly sampling to monitor key water-quality indicators. Updated monthly, this app allows users to toggle between years and months to visualize seasonal and long-term trends in the slough’s dynamic ecosystem."),
                 
                 # Second paragraph with bolded Give page reference
                 p(strong("If you would like to support ongoing monitoring and student research at the reserve, please consider visiting our Give page.")),
                 
                 # Give page button
                 tags$a(
                   href = "https://give.ucsb.edu/campaigns/58565/donations/new",
                   target = "_blank",
                   class = "btn btn-primary",
                   style = "margin-top: 10px;",  # Adds spacing above button
                   "Donate"
                 )
               )
      ),
      
      tabPanel("Trends",
               sidebarLayout(
                 sidebarPanel(
                   div(class = "ucsb-sidebar",
                       div(class = "ucsb-sidebar-header", "Filters"),
                       selectInput("site", "Monitoring Site:", choices = unique(df$Site)),
                       uiOutput("depthSelector"),
                       selectInput("parameter", "Parameter:", choices = param_choices),
                       sliderTextInput(
                         inputId = "yearRange",
                         label = "Year Range:",
                         choices = sort(unique(df$Year)),
                         selected = range(df$Year),
                         grid = TRUE
                       ),
                       sliderTextInput(
                         inputId = "monthRange",
                         label = "Months:",
                         choices = month.name,
                         selected = month.name[c(1, 12)],
                         grid = TRUE,
                         dragRange = TRUE
                       ),
                       
                       
                       ## ✅ Add this checkbox for the dotted line feature
                       checkboxInput("showGaps", "Show dotted lines for >1 month gaps", value = TRUE),
                       checkboxInput("logTransform", "Log-transform y-axis", value = FALSE),
                       
                       br(),
                       downloadButton("download_csv", "Download filtered data (CSV)", class = "btn-primary")
                   )
                 ),
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Time Series", plotlyOutput("timePlot")),
                     tabPanel("Seasonal Patterns", plotlyOutput("seasonPlot"))
                   ),
                   
                   ## ✅ Add this conditional note about the dotted lines
                   conditionalPanel(
                     condition = "input.showGaps == true",
                     div(
                       style = "font-size: 0.9em; color: #003660; font-style: italic; margin-top: 10px;",
                       "Note: Dotted lines in the time series indicate a data gap of more than 1 month between observations."
                     )
                   ),
                   ## ✅ Add this conditional note about log-transform
                   conditionalPanel(
                     condition = "input.logTransform == true",
                     div(
                       style = "font-size: 0.9em; color: #003660; font-style: italic; margin-top: 5px;",
                       "Note: Log-transform is applied to the y-axis. Only values greater than 0 are shown; zeros and negatives are excluded from the plot."
                     )
                   )
                   
                 )
               ) 
      ),
      
      # --- Map Tab ---
      tabPanel("Map",
               br(),
               h3("Monitoring Site Locations",
                  style = "color:#003660; font-family:'Nunito Sans'; font-weight:700; margin-bottom: 20px;"),
               checkboxInput("showLegend", "Show Legend", value = TRUE),
               leafletOutput("map", height = "600px")
               
               
      ), 
      
      # --- Methods & FAQ ---
      tabPanel("Methods & FAQ",
               br(),
               h3("Methods & Frequently Asked Questions"),
               tags$details(
                 tags$summary("Sampling Design"),
                 p("Sites include Mouth (MO1), Culvert(CUL1), Bridge (VBR1), and Pier (PIER) within the Devereux Slough system. Non-pier sites are summarized by ",
                   em("Surface (≤20 cm)"),
                   " and ",
                   em("Bottom (>20 cm)"),
                   " depth layers. PIER measurements use fixed depths (e.g., 10–250 cm).")
               ),
               tags$details(
                 tags$summary("Parameters & Units"),
                 tags$ul(
                   tags$li("Temperature (C) — water temperature in degrees Celsius."),
                   tags$li("Dissolved Oxygen (mg/L) — concentration by mass."),
                   tags$li("Dissolved Oxygen (%) — percent saturation."),
                   tags$li("Salinity (ppt) — practical salinity in parts per thousand."),
                   tags$li("Conductivity-specific (mS/cm) — temperature-corrected conductivity.")
                 )
               ),
               tags$details(
                 tags$summary("Seasonality vs. Interannual trends"),
                 p("Use the Time Series view to explore long-term changes at a site/depth, and Seasonal Patterns to compare distributions by month across years.")
               ),
               tags$details(
                 tags$summary("Depth & Water Level"),
                 p(" Measurements are taken with a handheld YSI Pro2030 at fixed sites along the slough. Water depth changes seasonally, so during dry periods, some sites may be too shallow to sample. When the slough is low or dry, readings may reflect air conditions rather than water conditions.")
               ),
               tags$details(
                 tags$summary("Data Quality & Caveats"),
                 p("Values are plotted as provided after basic type-cleaning. Consider calibration records, instrument changes, and field conditions when interpreting extremes."),
                 p("Filters (years/months) apply to both plots and may change distributions.")
               ),
               tags$details(
                 tags$summary("Contact & Attribution"),
                 p(
                   "Developed for the Coal Oil Point Reserve (UCSB). For questions or to report issues, please contact the COPR team via",
                   tags$a(
                     href = "https://copr.nrs.ucsb.edu/contact/",
                     tags$strong("email"),
                     target = "_blank"
                   ),
                   "."
                 )
               )
      ), 
    ),
    
    div(
      style = "
    background-color: #FEBC11;
    text-align: center;              
    padding: 15px 0;                
    border-top: 2px solid #003660;   
    display: flex;                   
    justify-content: center;         
    align-items: center;             
    gap: 20px;                       
    flex-wrap: wrap;
    margin-top: 40px;
  ",
      
      # Clickable logos
      tags$a(href = "https://www.nrs.ucsb.edu/", target = "_blank",
             tags$img(src = "nrs_logo.png", height = "60px")),
      
      tags$a(href = "https://copr.nrs.ucsb.edu/", target = "_blank",
             tags$img(src = "COPR_logo.png", height = "60px")),
      
      tags$a(href = "https://www.ucsb.edu/", target = "_blank",
             tags$img(src = "ucsb_logo.png", height = "60px"))
    ),
    div(
      "Developed by Samuel A. Cervantes and Michelle Moreno",
      style = "text-align: center; font-size: 0.8em; color: #003660; margin-top: 5px;"
    )
    
  )
  
)
# Server
server <- function(input, output, session) {
  # Label helper for plots
  param_label <- reactive({
    names(param_choices)[match(input$parameter, param_choices)]
  })
  
  # Dynamic depth selector
  output$depthSelector <- renderUI({
    if (input$site == "Pier") {
      available_depths <- sort(unique(df %>% filter(Site == "Pier") %>% pull(Depth)))
      selectInput("depth", "Select Pier Depth (cm):",
                  choices = c(10, 50, 100, 150, 200, 250), selected = 50)
    } else {
      selectInput("depth", "Select Depth Layer:", choices = c("Surface", "Bottom"))
    }
  })
  
  
  # Define fixed y-axis limits for parameters (safe version)
  param_axis_limits <- reactive({
    limits <- list(
      "Temperature" = c(0, 35),
      "DO" = c(0, 25),
      "DO_percent" = c(0, 250),
      "Salinity" = c(0, 150),
      "Conductivity" = c(0, 200)
    )
    
    param <- input$parameter
    
    if (!is.null(param) && param %in% names(limits)) {
      return(limits[[param]])
    } else {
      return(c(NA, NA))  # or some reasonable default
    }
  })
  
  # Reactive filtered data
  filteredData <- reactive({
    req(input$site, input$parameter, input$yearRange, input$monthRange)
    
    selected_site   <- input$site
    selected_years  <- as.numeric(input$yearRange)
    selected_months <- match(input$monthRange, month.name)
    
    if (length(selected_months) < 2) {
      selected_months <- rep(selected_months[1], 2)
    }
    
    selected_depth <- if (selected_site == "Pier") {
      as.numeric(input$depth)
    } else {
      input$depth
    }
    
    # --- Filter the main data ---
    data <- df %>%
      filter(
        Site == selected_site,
        Year  >= selected_years[1], Year  <= selected_years[2],
        Month >= selected_months[1], Month <= selected_months[2]
      )
    
    # --- Handle depth selection ---
    if (selected_site == "Pier") {
      data <- data %>% filter(Depth == selected_depth)
    } else {
      data <- data %>% filter(DepthLayer == selected_depth)
    }
    
    # --- Filter by parameter safely ---
    param <- input$parameter
    if (!is.null(param) && param %in% names(data)) {
      data <- data %>%
        filter(!is.na(.data[[param]]), !is.na(Date))
    } else {
      data <- tibble()  # Return empty tibble if parameter missing
    }
    
    # --- Ensure we always return a data.frame ---
    validate(
      need(nrow(data) > 0, "No data available for the selected filters.")
    )
    
    return(data)
  })
  
  
  # CSV download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("devereux_filtered_",
             input$site, "_",
             input$parameter, "_",
             input$yearRange[1], "-", input$yearRange[2], ".csv")
    },
    content = function(file) {
      data <- filteredData()
      req(nrow(data) > 0, is.data.frame(data))
      readr::write_csv(data, file, na = "")
    }
  )
  # Data citation download
  output$download_citation_txt <- downloadHandler(
    filename = function() {
      "devereux_water_quality_citation.txt"
    },
    content = function(file) {
      citation_text <- paste(
        "Devereux Slough Water Quality Monitoring Dataset.",
        "University of California, Santa Barbara, Coal Oil Point Reserve.",
        "Accessed via interactive dashboard at https://copr.nrs.ucsb.edu/resources.",
        "DOI: 10.12345/devereux.2025.001",  # Placeholder DOI
        sep = "\n"
      )
      writeLines(citation_text, con = file)
    }
  )
  
  output$download_citation_bib <- downloadHandler(
    filename = function() {
      "devereux_water_quality_citation.bib"
    },
    content = function(file) {
      latest_year <- format(max(df$Date, na.rm = TRUE), "%Y")
      bibtex_entry <- paste(
        "@dataset{devereux2025,",
        "  author    = {Coal Oil Point Reserve},",
        "  title     = {Devereux Slough Water Quality Monitoring Dataset},",
        paste0("  year      = {", latest_year, "},"),
        "  publisher = {University of California, Santa Barbara},",
        "  url       = {https://copr.nrs.ucsb.edu/resources.},",
        "  doi       = {10.12345/devereux.2025.001}",
        "}", sep = "\n"
      )
      writeLines(bibtex_entry, con = file)
    }
  )
 
  output$download_citation_ris <- downloadHandler(
    filename = function() {
      "devereux_water_quality_citation.ris"
    },
    content = function(file) {
      latest_year <- format(max(df$Date, na.rm = TRUE), "%Y")
      ris_entry <- paste(
        "TY  - DATA",
        "T1  - Devereux Slough Water Quality Monitoring Dataset",
        "AU  - Coal Oil Point Reserve",
        paste0("PY  - ", latest_year),
        "PB  - University of California, Santa Barbara",
        "UR  - https://copr.nrs.ucsb.edu/resources.",
        "DO  - 10.12345/devereux.2025.001",  # Placeholder DOI
        "ER  -",
        sep = "\n"
      )
      writeLines(ris_entry, con = file)
    }
  )
  
  
  # --- Time series plot ---
  output$timePlot <- renderPlotly({
    df <- filteredData()
    req(nrow(df) > 1, cancelOutput = TRUE)
    
    df <- df %>% arrange(Date)
    df <- df %>%
      mutate(Parameter = as.numeric(.data[[input$parameter]])) %>%
      filter(!is.na(Parameter))
    
    # Apply log10 transform if enabled
    is_log <- isTRUE(input$logTransform)
    
    if (is_log) {
      df <- df %>% filter(Parameter > 0)
      if (nrow(df) < 2) return(NULL)
      df <- df %>% mutate(Parameter = log10(Parameter))
    }
    
    # Create solid line segments for gaps < 31 days
    df <- df %>%
      mutate(
        time_diff = as.numeric(difftime(Date, lag(Date), units = "days")),
        segment_id = cumsum(if_else(is.na(time_diff) | time_diff > 31, 1, 0))
      )
    
    solid_lines <- df %>%
      group_by(segment_id) %>%
      filter(n() > 1)
    
    # Optional dotted lines for > 31 day gaps
    dotted_lines <- NULL
    if (isTRUE(input$showGaps)) {
      dotted_lines <- df %>%
        mutate(
          next_Date = lead(Date),
          next_Val = lead(Parameter),
          time_diff = as.numeric(difftime(lead(Date), Date, units = "days"))
        ) %>%
        filter(!is.na(time_diff) & time_diff > 31) %>%
        transmute(
          x = map2(Date, next_Date, ~ c(.x, .y)),
          y = map2(Parameter, next_Val, ~ c(.x, .y))
        ) %>%
        unnest(c(x, y)) %>%
        group_by(group = rep(1:(n() / 2), each = 2))
    }
    
    # Plot
    p <- ggplot() +
      geom_line(data = solid_lines,
                aes(x = Date, y = Parameter, group = segment_id),
                color = "#003660", size = 0.8) +
      geom_point(data = df,
                 aes(x = Date, y = Parameter,
                     text = paste("Date:", Date,
                                  "<br>", param_label(), ":", round(Parameter, 2))),
                 size = 1.5, alpha = 0.7, color = "#FEBC11") +
      labs(
        title = paste(param_label(), "at", input$site),
        x = "Date",
        y = if (is_log) paste0("log10(", param_label(), ")") else param_label()
      ) +
      scale_y_continuous(labels = scales::label_number()) +
      theme_minimal(base_family = "Nunito Sans") +
      theme(
        plot.title = element_text(size = 18, face = "bold", color = "#003660"),
        axis.title = element_text(size = 14, face = "bold", color = "#003660"),
        axis.text = element_text(size = 12, color = "#003660"),
        axis.line = element_line(color = "#003660", size = 0.8),
        axis.ticks = element_line(color = "#003660"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.3, color = "gray80")
      )
    
    # Add dotted lines if they exist
    if (!is.null(dotted_lines) && nrow(dotted_lines) > 1) {
      p <- p + geom_line(data = dotted_lines,
                         aes(x = x, y = y, group = group),
                         linetype = "dotted", size = 0.8, color = "#003660")
    }
    
    # Apply y-axis limits only if not log-transformed
    if (!is_log) {
      y_limits <- param_axis_limits()
      if (!is.null(y_limits)) {
        p <- p + coord_cartesian(ylim = y_limits)
      }
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(font = list(family = "Nunito Sans")))
  })
  
  # --- Seasonal patterns plot ---
  output$seasonPlot <- renderPlotly({
    df <- filteredData()
    req(nrow(df) > 1, cancelOutput = TRUE)
    
    df <- df %>%
      mutate(Parameter = as.numeric(.data[[input$parameter]])) %>%
      filter(!is.na(Parameter))
    
    # Apply log10 transform if requested
    if (isTRUE(input$logTransform)) {
      df <- df %>% filter(Parameter > 0)
      if (nrow(df) < 2) return(NULL)
      df <- df %>% mutate(Parameter = log10(Parameter))
    }
    
    p <- ggplot(df,
                aes(x = month(Date, label = TRUE), y = Parameter)) +
      geom_boxplot(
        aes(text = paste("Month:", month(Date, label = TRUE),
                         "<br>", param_label(), ":", round(Parameter, 2))),
        fill = "#FEBC11", color = "#003660", outlier.color = "#003660"
      ) +
      labs(
        title = paste("Seasonal Patterns of", param_label(), "at", input$site),
        x = "Month",
        y = if (isTRUE(input$logTransform)) paste0("log10(", param_label(), ")") else param_label()
      ) +
      scale_y_continuous(labels = scales::label_number()) +
      theme_minimal(base_family = "Nunito Sans") +
      theme(
        plot.title = element_text(size = 18, face = "bold", color = "#003660"),
        axis.title = element_text(size = 14, face = "bold", color = "#003660"),
        axis.text = element_text(size = 12, color = "#003660"),
        axis.line = element_line(color = "#003660", size = 0.8),
        axis.ticks = element_line(color = "#003660"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.3, color = "gray80")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(font = list(family = "Nunito Sans")))
  })
  
  
  # --- Leaflet map ---
  output$map <- renderLeaflet({
    icon_list <- list(
      "Mouth" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "orange"),
      "Culvert" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "blue"),
      "Bridge" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "green"),
      "Pier" = awesomeIcons(icon = "tint", iconColor = "white", library = "fa", markerColor = "red")
    )
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("Street Map", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE)) %>%
      addMiniMap(toggleDisplay = TRUE, position = "bottomright")
    
    for (site in names(icon_list)) {
      site_data <- dplyr::filter(site_locations, Site == site)
      map <- map %>%
        addAwesomeMarkers(
          data = site_data,
          lng = ~Longitude, lat = ~Latitude,
          icon = icon_list[[site]],
          label = ~Site,
          popup = ~paste0(
            "<strong>Site:</strong> ", Site, "<br>",
            "<strong>Coordinates:</strong> ",
            "<a href='https://www.google.com/maps?q=", Latitude, ",", Longitude,
            "' target='_blank'>", round(Latitude, 6), ", ", round(Longitude, 6), "</a>"
          )
        )
    }
    
if (input$showLegend) {
  map <- map %>%
    addLegend(
      position = "bottomright",
      colors = c("orange", "blue", "green", "red"),
      labels = c("Mouth", "Culvert", "Bridge", "Pier"),
      title = "Monitoring Sites",
      opacity = 1,
    )
}

    
    map
  })
}

# Run app
shinyApp(ui = ui, server = server)