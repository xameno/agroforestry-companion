# Load required libraries
library(dplyr)
library(igraph)
library(visNetwork)
library(shiny)
library(leaflet)
library(shinyjs)

# Load the data
df <- read.csv("input/cleaned.csv")
siteInfo = read.csv("input/siteInfo.csv")

# Merge coordinates into main dataframe ONCE at the start
df_info <- df %>%
  left_join(siteInfo %>% select(siteID, lat, lon, Description), by = "siteID")

# Define color mapping for categories
category_colors <- c(
  "Forbs" = "#FFD700",  
  "Shrubs" = "#aab44b",
  "Trees" = "#47814d",  
  "Animals" = "#001ee9"   
)

# NEW FUNCTION: Get species list for a site by category
get_species_by_category <- function(site_id, country_data) {
  site_species <- country_data %>%
    filter(siteID == site_id) %>%
    distinct(commonName, category) %>%
    arrange(category, commonName)
  
  if (nrow(site_species) == 0) {
    return("No species data available")
  }
  
  # Group by category and create color-coded lists
  species_by_category <- site_species %>%
    group_by(category) %>%
    summarise(
      species_list = paste(commonName, collapse = ", "),
      .groups = 'drop'
    )
  
  # Create HTML with color-coded categories
  html_parts <- apply(species_by_category, 1, function(row) {
    category <- row[["category"]]
    species_list <- row[["species_list"]]
    color <- category_colors[category]
    
    paste0("<span style='color:", color, "; font-weight: bold;'>", 
           category, ":</span> ", species_list)
  })
  
  paste(html_parts, collapse = "<br>")
}

# NEW FUNCTION: Get species list for a site by category (for cases tab - simpler version)
get_species_by_category_simple <- function(site_id, country_data) {
  site_species <- country_data %>%
    filter(siteID == site_id) %>%
    distinct(commonName, category) %>%
    arrange(category, commonName)
  
  if (nrow(site_species) == 0) {
    return("No species data available")
  }
  
  # Group by category
  species_by_category <- site_species %>%
    group_by(category) %>%
    summarise(
      species_list = paste(commonName, collapse = ", "),
      .groups = 'drop'
    )
  
  # Create simple text with category headers
  text_parts <- apply(species_by_category, 1, function(row) {
    category <- row[["category"]]
    species_list <- row[["species_list"]]
    
    paste0(toupper(category), ": ", species_list)
  })
  
  paste(text_parts, collapse = "\n")
}

# NEW FUNCTION: Create focused network for a single species - FIXED CO-OCCURRENCE CALCULATION
create_focused_network_data <- function(data, selected_country, selected_species) {
  # Filter by country
  country_data <- data %>% filter(country == selected_country)
  
  if (nrow(country_data) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  
  # Get all sites where the selected species appears
  selected_species_sites <- country_data %>%
    filter(commonName == selected_species) %>%
    distinct(siteID) %>%
    pull(siteID)
  
  if (length(selected_species_sites) == 0) {
    return(list(nodes = data.frame(), edges = data.frame()))
  }
  
  # FIXED: Get co-occurrence counts by counting distinct sites where both species appear
  # Get all species that appear in the same sites as the selected species
  cooccurring_data <- country_data %>%
    filter(siteID %in% selected_species_sites) %>%
    filter(commonName != selected_species) %>%
    group_by(commonName) %>%
    summarise(
      cooccur_sites = n_distinct(siteID),  # Count distinct sites of co-occurrence
      .groups = 'drop'
    )
  
  # Create edges with correct co-occurrence counts
  edges <- data.frame(
    from = selected_species,
    to = cooccurring_data$commonName,
    value = cooccurring_data$cooccur_sites,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      id = paste0("edge_", row_number()),
      title = ""
    )
  
  # Get species information for nodes (including the selected species)
  all_species_in_network <- unique(c(selected_species, cooccurring_data$commonName))
  
  species_info <- country_data %>%
    filter(commonName %in% all_species_in_network) %>%
    group_by(commonName, category, species) %>%
    summarise(
      current_country_sites = n_distinct(siteID),  # Total sites for this species in country
      .groups = 'drop'
    )
  
  # Get global species information for "also found in"
  species_global_info <- data %>%
    group_by(commonName) %>%
    summarise(
      all_countries = paste(sort(unique(country)), collapse = ", "),
      .groups = 'drop'
    )
  
  # Combine current and global info
  species_info <- species_info %>%
    left_join(species_global_info, by = "commonName") %>%
    mutate(
      latin_info = ifelse(species != commonName, 
                          paste0(" <i>(", species, ")</i>"), 
                          "")
    )
  
  # Create nodes WITHOUT tooltips (we'll use click popups instead)
  nodes <- species_info
  nodes$title <- ""  # Empty title to disable hover tooltips
  
  # Store the full info for click popups
  nodes$full_info <- apply(nodes, 1, function(row) {
    common_name <- row[["commonName"]]
    category_val <- row[["category"]]
    latin_info <- row[["latin_info"]]
    current_sites <- row[["current_country_sites"]]
    all_countries <- row[["all_countries"]]
    
    # Remove current country from "also found in"
    other_countries <- gsub(paste0("(", selected_country, ", |, ", selected_country, "|", selected_country, ")"), "", all_countries)
    other_countries <- trimws(gsub(",,", ",", other_countries))
    other_countries <- gsub("^,|,$", "", other_countries)
    
    if (other_countries == "") {
      other_countries_text <- paste0("Only in ", selected_country)
    } else {
      other_countries_text <- paste0("Also found in: ", other_countries)
    }
    
    paste0("<b>", common_name, latin_info, "</b><br>",
           "Category: ", category_val, "<br>",
           "Sites in ", selected_country, ": ", current_sites, " sites<br>",
           other_countries_text)
  })
  
  # Add remaining node properties
  nodes <- nodes %>%
    mutate(
      id = commonName,
      label = commonName,
      group = category,
      color = category_colors[category],
      value = ifelse(commonName == selected_species, 5, 1)  # Make selected species larger
    ) %>%
    distinct(id, .keep_all = TRUE)
  
  return(list(nodes = nodes, edges = edges))
}

# Create the Shiny app UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Agroforestry species companion finder"),
  
  # Custom CSS for better styling
  tags$style(HTML("
    .network-info {
      font-family: 'Courier New', monospace;
      background-color: #f8f9fa;
      padding: 12px;
      border-radius: 5px;
      border: 1px solid #dee2e6;
      margin-bottom: 15px;
    }
    .stats-section {
      margin: 8px 0;
    }
    .category-stats {
      margin-left: 15px;
    }
    .divider {
      border-top: 1px solid #ccc;
      margin: 10px 0;
    }
    .floating-popup {
      position: fixed;
      top: 120px;
      right: 20px;
      width: 300px;
      max-height: 400px;
      overflow-y: auto;
      background-color: #fff3cd;
      padding: 15px;
      border-radius: 5px;
      border: 1px solid #ffeaa7;
      font-family: 'Courier New', monospace;
      z-index: 1000;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
    }
    .close-btn {
      float: right;
      background: none;
      border: none;
      font-size: 16px;
      cursor: pointer;
      color: #666;
    }
    .close-btn:hover {
      color: #000;
    }
    .map-btn {
      background-color: #47814d;
      color: white;
      border: none;
      padding: 6px 12px;
      border-radius: 4px;
      cursor: pointer;
      font-size: 12px;
      margin-top: 5px;
      width: 100%;
    }
    .map-btn:hover {
      background-color: #366635;
    }
    .map-btn:disabled {
      background-color: #cccccc;
      cursor: not-allowed;
    }
    .case-btn {
      background-color: #6c757d;
      color: white;
      border: none;
      padding: 6px 12px;
      border-radius: 4px;
      cursor: pointer;
      font-size: 12px;
      margin-top: 5px;
      width: 100%;
    }
    .case-btn:hover {
      background-color: #545b62;
    }
    .case-btn:disabled {
      background-color: #cccccc;
      cursor: not-allowed;
    }
    .site-count-info {
      font-size: 11px;
      color: #e8f5e8;
      margin-top: 3px;
      text-align: center;
    }
    .case-count-info {
      font-size: 11px;
      color: #f8f9fa;
      margin-top: 3px;
      text-align: center;
    }
    /* Overlay for closing popup */
    .popup-overlay {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: transparent;
      z-index: 999;
      display: none;
    }
    .popup-overlay.active {
      display: block;
    }
    .case-description {
      background-color: #f8f9fa;
      padding: 15px;
      border-radius: 5px;
      border-left: 4px solid #6c757d;
      margin: 15px 0;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .species-category {
      margin: 8px 0;
      padding: 5px;
      border-radius: 3px;
    }
    .species-list {
      margin-left: 10px;
      font-size: 14px;
    }
  ")),
  
  # JavaScript for closing popup
  tags$script(HTML("
    // Close popup when clicking on overlay
    $(document).on('click', '#popup_overlay', function() {
      Shiny.setInputValue('close_popup_click', Math.random());
    });
    
    // Close popup when clicking on dropdowns or other UI elements
    $(document).on('change', '#country_select, #species_select', function() {
      Shiny.setInputValue('close_popup_click', Math.random());
    });
  ")),
  
  # Overlay for closing popup when clicking anywhere
  div(id = "popup_overlay", class = "popup-overlay"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Species selection dropdown with nested categories
      tags$h4("Step 1: Select species"),
      uiOutput("species_select_ui"),
      
      # Country selection dropdown
      tags$h4("Step 2: Select country"),
      uiOutput("country_select_ui"),
      
      # Network information in monospace frame
      tags$h4("Companion network info:"),
      uiOutput("network_info"),
      
      # Reset button
      actionButton("reset", "Reset", class = "btn-primary"),
      
      # Instructions
      tags$hr(),
      tags$h5("How to use this tool:"),
      tags$ol(
        tags$li("Select a species"),
        tags$li("Select a country where it's found"),
        tags$li("Click a node or connection for info"),
        tags$li("Click 'View Cases on Map' to see locations")
      )
    ),
    
    mainPanel(
      width = 9,
      # FLOATING POPUP
      uiOutput("floating_popup"),
      
      # TABS INSTEAD OF MODAL
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        # Network Tab
        tabPanel(
          "Companion Network",
          value = "network_tab",
          # Message when no species selected
          conditionalPanel(
            condition = "input.species_select == ''",
            tags$div(
              style = "height: 800px; display: flex; flex-direction: column; justify-content: center; align-items: center; background-color: #f8f9fa; border-radius: 10px;",
              tags$h3("🌿 Start by selecting a species"),
              tags$p("Choose a species from the dropdown to see available countries"),
              tags$br(),
              tags$p(style = "color: #666;", "Then select a country to explore its companion network")
            )
          ),
          # Species info when species selected but no country
          conditionalPanel(
            condition = "input.species_select != '' && input.country_select == ''",
            tags$div(
              style = "height: 800px; display: flex; flex-direction: column; justify-content: center; align-items: center; background-color: #f8f9fa; border-radius: 10px;",
              tags$h3("🌍 Now select a country"),
              tags$p("Choose a country from the dropdown to see the companion network"),
              tags$br(),
              uiOutput("species_message")
            )
          ),
          # Network visualization (only when both species and country are selected)
          conditionalPanel(
            condition = "input.species_select != '' && input.country_select != ''",
            visNetworkOutput("network", height = "800px")
          )
        ),
        
        # Map Tab
        tabPanel(
          "Case Map",
          value = "map_tab",
          # Show message when no cases are selected
          conditionalPanel(
            condition = "output.map_has_data == false",
            tags$div(
              style = "height: 800px; display: flex; flex-direction: column; justify-content: center; align-items: center; background-color: #f8f9fa; border-radius: 10px;",
              tags$h3("🗺️ Select an element to view cases"),
              tags$p("Click on a species node or connection in the companion network"),
              tags$p("Then click 'View Cases on Map' to see the locations"),
              tags$br(),
              tags$p(style = "color: #666;", "The map will update automatically when you select different elements")
            )
          ),
          # Show map only when we have data
          conditionalPanel(
            condition = "output.map_has_data == true",
            leafletOutput("caseMap", height = "800px")
          )
        ),
        
        # NEW: Cases Tab for non-geolocated entries
        tabPanel(
          "Case Descriptions",
          value = "cases_tab",
          # Show message when no cases are selected
          conditionalPanel(
            condition = "output.cases_has_data == false",
            tags$div(
              style = "height: 800px; display: flex; flex-direction: column; justify-content: center; align-items: center; background-color: #f8f9fa; border-radius: 10px;",
              tags$h3("📋 Select an element to view case descriptions"),
              tags$p("Click on a species node or connection in the companion network"),
              tags$p("Then click 'View Case Descriptions' to see detailed information"),
              tags$br(),
              tags$p(style = "color: #666;", "Case descriptions provide additional context without specific coordinates")
            )
          ),
          # Show cases list when we have data
          conditionalPanel(
            condition = "output.cases_has_data == true",
            uiOutput("cases_list")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Track clicked element for floating popup
  clicked_element <- reactiveVal(NULL)
  popup_title <- reactiveVal("Element Details")
  show_popup <- reactiveVal(FALSE)
  
  # Track map data - initialize as NULL
  map_cases_data <- reactiveVal(NULL)
  map_title <- reactiveVal("Case Locations")
  cases_with_coords_count <- reactiveVal(0)
  total_cases_count <- reactiveVal(0)
  
  # NEW: Track cases data for non-geolocated entries
  cases_data <- reactiveVal(NULL)
  cases_count <- reactiveVal(0)
  
  # Track which element triggered the map view
  current_map_element <- reactiveVal(NULL)
  current_cases_element <- reactiveVal(NULL)
  
  # Output to check if map has data
  output$map_has_data <- reactive({
    !is.null(map_cases_data()) && nrow(map_cases_data()) > 0
  })
  outputOptions(output, "map_has_data", suspendWhenHidden = FALSE)
  
  # NEW: Output to check if cases has data
  output$cases_has_data <- reactive({
    !is.null(cases_data()) && nrow(cases_data()) > 0
  })
  outputOptions(output, "cases_has_data", suspendWhenHidden = FALSE)
  
  # Clear data when species or country changes
  observeEvent(input$species_select, {
    map_cases_data(NULL)
    cases_data(NULL)
    show_popup(FALSE)
    clicked_element(NULL)
    current_map_element(NULL)
    current_cases_element(NULL)
    cases_with_coords_count(0)
    total_cases_count(0)
    cases_count(0)
  })
  
  observeEvent(input$country_select, {
    map_cases_data(NULL)
    cases_data(NULL)
    show_popup(FALSE)
    clicked_element(NULL)
    current_map_element(NULL)
    current_cases_element(NULL)
    cases_with_coords_count(0)
    total_cases_count(0)
    cases_count(0)
  })
  
  # Floating popup UI - WITH CASE COUNT IN BUTTONS
  output$floating_popup <- renderUI({
    if (show_popup() && !is.null(clicked_element())) {
      has_coords <- cases_with_coords_count() > 0
      has_cases <- cases_count() > 0
      
      map_button_text <- if (has_coords) {
        paste0("View Cases on Map (", cases_with_coords_count(), "/", total_cases_count(), " with coordinates)")
      } else {
        "No cases with coordinates available"
      }
      
      cases_button_text <- if (has_cases) {
        paste0("View Case Descriptions (", cases_count(), " available)")
      } else {
        "No case descriptions available"
      }
      
      tagList(
        # Show overlay when popup is active
        tags$script(HTML("$('#popup_overlay').addClass('active');")),
        tags$div(
          class = "floating-popup",
          tags$button(class = "close-btn", "×", id = "close_popup_btn"),
          tags$h5(popup_title()),
          clicked_element(),
          # Add buttons for both species and companionship popups
          if (popup_title() %in% c("🌿 Species info", "🔗 Companionship info")) {
            tags$div(
              style = "margin-top: 10px; text-align: center;",
              if (has_coords) {
                actionButton("view_cases_map", map_button_text, class = "map-btn")
              } else {
                tags$button(class = "map-btn", disabled = TRUE, map_button_text)
              },
              if (has_cases) {
                actionButton("view_cases_list", cases_button_text, class = "case-btn")
              } else {
                tags$button(class = "case-btn", disabled = TRUE, cases_button_text)
              }
            )
          },
          tags$hr(),
          tags$p(style = "font-size: 12px; color: #666; margin: 0;", 
                 "Click anywhere to close")
        )
      )
    } else {
      # Hide overlay when popup is closed
      tags$script(HTML("$('#popup_overlay').removeClass('active');"))
    }
  })
  
  # Species message for display
  output$species_message <- renderUI({
    req(input$species_select)
    tags$p(style = "color: #666;", 
           "The dropdown shows countries where", 
           tags$strong(input$species_select), "is found")
  })
  
  # Observe close button click
  observeEvent(input$close_popup_click, {
    show_popup(FALSE)
    clicked_element(NULL)
    current_map_element(NULL)
    current_cases_element(NULL)
  })
  
  # Handle the "View Cases on Map" button click
  observeEvent(input$view_cases_map, {
    req(input$view_cases_map > 0)
    
    # Close the popup
    show_popup(FALSE)
    clicked_element(NULL)
    
    # Switch to map tab
    updateTabsetPanel(session, "main_tabs", selected = "map_tab")
  })
  
  # NEW: Handle the "View Case Descriptions" button click
  observeEvent(input$view_cases_list, {
    req(input$view_cases_list > 0)
    
    # Close the popup
    show_popup(FALSE)
    clicked_element(NULL)
    
    # Switch to cases tab
    updateTabsetPanel(session, "main_tabs", selected = "cases_tab")
  })
  
  # Create the species select input with nested categories
  output$species_select_ui <- renderUI({
    # Create nested list by category for ALL species
    choices_list <- list()
    
    # Add default option
    choices_list[["Choose a species..."]] <- ""
    
    # Add categories with their species from entire dataset
    categories <- c("Forbs", "Shrubs", "Trees", "Animals")
    for (cat in categories) {
      cat_species <- sort(unique(df_info$commonName[df_info$category == cat]))
      if (length(cat_species) > 0) {
        # Create named vector for this category
        cat_choices <- setNames(cat_species, cat_species)
        choices_list[[cat]] <- cat_choices
      }
    }
    
    selectInput("species_select", NULL, choices = choices_list)
  })
  
  # Create the country select input
  output$country_select_ui <- renderUI({
    if (is.null(input$species_select) || input$species_select == "") {
      selectInput("country_select", NULL,
                  choices = c("First choose a species" = ""))
    } else {
      # Get countries where the selected species appears
      species_countries <- df_info %>%
        filter(commonName == input$species_select) %>%
        distinct(country) %>%
        pull(country) %>%
        sort()
      
      selectInput("country_select", NULL,
                  choices = c("Choose a country..." = "", species_countries))
    }
  })
  
  # Reactive for country data
  country_data <- reactive({
    req(input$country_select)
    df_info %>% filter(country == input$country_select)
  })
  
  # Reactive expression for focused network data
  network_data <- reactive({
    req(input$country_select, input$species_select)
    create_focused_network_data(df_info, input$country_select, input$species_select)
  })
  
  # Enhanced Network information display
  output$network_info <- renderUI({
    req(network_data())
    data <- network_data()
    
    if (nrow(data$nodes) == 0) {
      return(
        tags$div(class = "network-info",
                 tags$p("No companion data found for this species")
        )
      )
    }
    
    # Calculate statistics
    selected_species <- input$species_select
    selected_category <- data$nodes$category[data$nodes$id == selected_species]
    total_country_cases <- n_distinct(country_data()$siteID)
    
    # Get country-wide species counts by category
    country_species_counts <- country_data() %>%
      distinct(commonName, category) %>%
      group_by(category) %>%
      summarise(total_in_country = n(), .groups = 'drop')
    
    # Get total species in country (excluding selected species)
    total_species_country <- n_distinct(country_data()$commonName) - 1
    
    # Companion species (excluding the selected one)
    companion_nodes <- data$nodes[data$nodes$id != selected_species, ]
    n_companions <- nrow(companion_nodes)
    
    # Count companion species by category
    companion_counts <- companion_nodes %>%
      group_by(group) %>%
      summarise(count = n(), .groups = 'drop')
    
    trees_count <- ifelse(any(companion_counts$group == "Trees"), 
                          companion_counts$count[companion_counts$group == "Trees"], 0)
    animals_count <- ifelse(any(companion_counts$group == "Animals"), 
                            companion_counts$count[companion_counts$group == "Animals"], 0)
    forbs_count <- ifelse(any(companion_counts$group == "Forbs"), 
                          companion_counts$count[companion_counts$group == "Forbs"], 0)
    shrubs_count <- ifelse(any(companion_counts$group == "Shrubs"), 
                           companion_counts$count[companion_counts$group == "Shrubs"], 0)
    
    # Calculate "out of" values for each category
    trees_total <- ifelse(any(country_species_counts$category == "Trees"), 
                          country_species_counts$total_in_country[country_species_counts$category == "Trees"], 0)
    animals_total <- ifelse(any(country_species_counts$category == "Animals"), 
                            country_species_counts$total_in_country[country_species_counts$category == "Animals"], 0)
    forbs_total <- ifelse(any(country_species_counts$category == "Forbs"), 
                          country_species_counts$total_in_country[country_species_counts$category == "Forbs"], 0)
    shrubs_total <- ifelse(any(country_species_counts$category == "Shrubs"), 
                           country_species_counts$total_in_country[country_species_counts$category == "Shrubs"], 0)
    
    # Adjust totals if selected species is in that category
    if (selected_category == "Trees") trees_total <- trees_total - 1
    if (selected_category == "Animals") animals_total <- animals_total - 1
    if (selected_category == "Forbs") forbs_total <- forbs_total - 1
    if (selected_category == "Shrubs") shrubs_total <- shrubs_total - 1
    
    # Selected species case count
    selected_species_cases <- data$nodes$current_country_sites[data$nodes$id == selected_species]
    
    tags$div(class = "network-info",
             tags$p(style = "margin: 0;", 
                    tags$strong("Selected species: "), selected_species, 
                    tags$span(style = "color: #666;", paste0(" (", selected_category, ")"))),
             tags$div(class = "divider"),
             tags$div(class = "stats-section",
                      tags$p(style = "margin: 5px 0;", 
                             tags$strong("Species reported in: "), 
                             paste0(selected_species_cases, " out of ", total_country_cases, " cases in ", input$country_select)),
                      tags$div(class = "divider"),
                      tags$p(style = "margin: 5px 0;", 
                             tags$strong("Number of companions: "), 
                             paste0(n_companions, " out of ", total_species_country, " other species in ", input$country_select)),
                      tags$p(style = "margin: 5px 0;", tags$strong("Companions by category:")),
                      tags$div(class = "category-stats",    
                               tags$div(class = "category-stats",
                                        tags$p(style = paste0("color:", category_colors["Forbs"], "; margin: 2px 0;"), 
                                               HTML(paste0("<b>• Forbs:</b> ", forbs_count, " (out of ", forbs_total, " in ", input$country_select, ")"))),
                                        tags$p(style = paste0("color:", category_colors["Shrubs"], "; margin: 2px 0;"), 
                                               HTML(paste0("<b>• Shrubs:</b> ", shrubs_count, " (out of ", shrubs_total, " in ", input$country_select, ")"))),
                                        tags$p(style = paste0("color:", category_colors["Trees"], "; margin: 2px 0;"), 
                                               HTML(paste0("<b>• Trees:</b> ", trees_count, " (out of ", trees_total, " in ", input$country_select, ")"))),
                                        tags$p(style = paste0("color:", category_colors["Animals"], "; margin: 2px 0;"), 
                                               HTML(paste0("<b>• Animals:</b> ", animals_count, " (out of ", animals_total, " in ", input$country_select, ")")))
                               )
                      )
             )
    )
  })
  
  # Output the network
  output$network <- renderVisNetwork({
    req(network_data())
    data <- network_data()
    
    if (nrow(data$nodes) == 0) {
      return(visNetwork(data.frame(), data.frame()) %>% 
               visOptions(nodesIdSelection = FALSE))
    }
    
    visNetwork(data$nodes, data$edges) %>%
      visNodes(
        shape = "dot",
        shadow = TRUE,
        borderWidth = 2,
        font = list(size = 16, face = "bold")
      ) %>%
      visEdges(
        shadow = TRUE,
        color = list(color = "lightgray", highlight = "red"),
        smooth = TRUE,
        width = 2
      ) %>%
      visOptions(
        highlightNearest = list(
          enabled = TRUE,
          degree = 1,
          hover = FALSE,
          algorithm = "all"
        ),
        nodesIdSelection = FALSE
      ) %>%
      visInteraction(
        hover = FALSE,
        navigationButtons = TRUE,
        tooltipDelay = 0
      ) %>%
      visEvents(
        click = "function(properties) {
          if(properties.nodes.length > 0) {
            Shiny.setInputValue('clicked_node', properties.nodes[0]);
            Shiny.setInputValue('clicked_edge', null);
          } else if(properties.edges.length > 0) {
            Shiny.setInputValue('clicked_edge', properties.edges[0]);
            Shiny.setInputValue('clicked_node', null);
          } else {
            Shiny.setInputValue('clicked_node', null);
            Shiny.setInputValue('clicked_edge', null);
            Shiny.setInputValue('background_click', Math.random());
          }
        }"
      ) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(
        stabilization = list(iterations = 100),
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -30)
      )
  })
  
  # Handle node clicks
  observeEvent(input$clicked_node, {
    req(input$clicked_node)
    data <- network_data()
    
    # Find the clicked node info
    node_info <- data$nodes[data$nodes$id == input$clicked_node, ]
    
    if (nrow(node_info) > 0) {
      # Get country context for this node
      country_cases_total <- n_distinct(country_data()$siteID)
      country_species_total <- n_distinct(country_data()$commonName) - 1
      
      # Calculate companion stats for this specific node
      node_country_data <- df_info %>% filter(country == input$country_select)
      node_cases <- node_country_data %>%
        filter(commonName == input$clicked_node) %>%
        distinct(siteID) %>%
        nrow()
      
      node_companion_species <- node_country_data %>%
        filter(siteID %in% (node_country_data %>%
                              filter(commonName == input$clicked_node) %>%
                              distinct(siteID) %>%
                              pull(siteID))) %>%
        filter(commonName != input$clicked_node) %>%
        distinct(commonName) %>%
        nrow()
      
      # Prepare map data for this species (only cases with coordinates)
      species_cases_with_coords <- df_info %>%
        filter(country == input$country_select, commonName == input$clicked_node) %>%
        distinct(siteID, .keep_all = TRUE) %>%
        filter(!is.na(lat) & !is.na(lon))
      
      # NEW: Prepare cases data for this species (only cases without coordinates)
      species_cases_without_coords <- df_info %>%
        filter(country == input$country_select, commonName == input$clicked_node) %>%
        distinct(siteID, .keep_all = TRUE) %>%
        filter(is.na(lat) | is.na(lon))
      
      map_cases_data(species_cases_with_coords)
      cases_data(species_cases_without_coords)
      total_cases_count(node_cases)
      cases_with_coords_count(nrow(species_cases_with_coords))
      cases_count(nrow(species_cases_without_coords))
      map_title(paste("Cases for", input$clicked_node, "in", input$country_select))
      current_map_element(input$clicked_node)
      current_cases_element(input$clicked_node)
      
      # Build enhanced display text
      display_text <- HTML(paste0(
        "<b>", node_info$commonName, node_info$latin_info, "</b><br>",
        "Category: ", node_info$category, "<br>",
        "Cases in ", input$country_select, ": ", node_cases, " cases<br>",
        "Companions in ", input$country_select, ": ", node_companion_species, " out of ", country_species_total, " other species reported<br>",
        node_info$full_info %>% 
          strsplit("<br>") %>% 
          unlist() %>% 
          .[grepl("Also found in:|Found only in", .)]
      ))
      
      clicked_element(display_text)
      popup_title("🌿 Species info")
      show_popup(TRUE)
    }
  })
  
  # Handle edge clicks
  observeEvent(input$clicked_edge, {
    req(input$clicked_edge)
    data <- network_data()
    
    # Get edge info by ID
    edge_info <- data$edges[data$edges$id == input$clicked_edge, ]
    
    if (nrow(edge_info) > 0) {
      # Get additional context - USE FULL COUNTRY DATA, not filtered network data
      from_species <- edge_info$from
      to_species <- edge_info$to
      cooccur_cases <- edge_info$value
      
      # Get case counts for both species FROM FULL COUNTRY DATA
      current_country_data <- country_data()
      
      from_cases <- current_country_data %>%
        filter(commonName == from_species) %>%
        distinct(siteID) %>%
        nrow()
      
      to_cases <- current_country_data %>%
        filter(commonName == to_species) %>%
        distinct(siteID) %>%
        nrow()
      
      # Validate the data - cooccur_cases cannot exceed individual case counts
      cooccur_cases <- min(cooccur_cases, from_cases, to_cases)
      
      # Prepare map data for companionship (co-occurrence cases with coordinates)
      companionship_cases_with_coords <- df_info %>%
        filter(country == input$country_select) %>%
        group_by(siteID) %>%
        filter(commonName %in% c(from_species, to_species)) %>%
        filter(n_distinct(commonName) == 2) %>%  # Only cases where both species occur
        distinct(siteID, .keep_all = TRUE) %>%
        filter(!is.na(lat) & !is.na(lon))  # Only cases with coordinates
      
      # NEW: Prepare cases data for companionship (co-occurrence cases without coordinates)
      companionship_cases_without_coords <- df_info %>%
        filter(country == input$country_select) %>%
        group_by(siteID) %>%
        filter(commonName %in% c(from_species, to_species)) %>%
        filter(n_distinct(commonName) == 2) %>%  # Only cases where both species occur
        distinct(siteID, .keep_all = TRUE) %>%
        filter(is.na(lat) | is.na(lon))  # Only cases without coordinates
      
      map_cases_data(companionship_cases_with_coords)
      cases_data(companionship_cases_without_coords)
      total_cases_count(cooccur_cases)
      cases_with_coords_count(nrow(companionship_cases_with_coords))
      cases_count(nrow(companionship_cases_without_coords))
      map_title(paste("Companionship cases for", from_species, "and", to_species, "in", input$country_select))
      current_map_element(paste(from_species, "and", to_species))
      current_cases_element(paste(from_species, "and", to_species))
      
      # Simple, clear display
      display_text <- HTML(paste0(
        "<b>Connection: ", from_species, " ↔ ", to_species, "</b><br>",
        "Companions in ", cooccur_cases, " case(s)<br><br>",
        "<b>Context:</b><br>",
        "• ", from_species, " appears in ", from_cases, " case(s)<br>",
        "• ", to_species, " appears in ", to_cases, " case(s)<br>"
      ))
      
      clicked_element(display_text)
      popup_title("🔗 Companionship info")
      show_popup(TRUE)
    }
  })
  
  # Case map output - UPDATED WITH SPECIES LISTS AND DESCRIPTION
  output$caseMap <- renderLeaflet({
    req(map_cases_data())
    cases_data <- map_cases_data()
    
    # Filter to only cases with coordinates
    cases_with_coords <- cases_data %>% 
      filter(!is.na(lat) & !is.na(lon))
    
    # Create map
    map <- leaflet() %>% 
      addTiles() %>%
      setView(lng = 10, lat = 50, zoom = 4)
    
    # Add markers if we have coordinates
    if (nrow(cases_with_coords) > 0) {
      map <- map %>%
        addMarkers(
          data = cases_with_coords,
          lng = ~lon,
          lat = ~lat,
          popup = ~{
            species_list <- get_species_by_category(siteID, country_data())
            paste0("<b>Case:</b> ", siteID, "<br>",
                   "<b>Lat:</b> ", round(lat, 4), ", <b>Lon:</b> ", round(lon, 4), "<br>",
                   "<b>Description:</b> ", ifelse(is.na(Description), "No description available", Description), "<br>",
                   "<b>Species reported:</b><br>", species_list)
          },
          clusterOptions = markerClusterOptions()
        )
      
      # Set view to show all markers
      if (nrow(cases_with_coords) > 1) {
        map <- map %>% 
          fitBounds(
            lng1 = min(cases_with_coords$lon, na.rm = TRUE),
            lat1 = min(cases_with_coords$lat, na.rm = TRUE),
            lng2 = max(cases_with_coords$lon, na.rm = TRUE),
            lat2 = max(cases_with_coords$lat, na.rm = TRUE)
          )
      } else {
        map <- map %>% 
          setView(
            lng = cases_with_coords$lon[1],
            lat = cases_with_coords$lat[1],
            zoom = 10
          )
      }
    } else {
      map <- map %>%
        addMarkers(
          lng = 10, lat = 50,
          popup = "No case coordinates available for the selected element"
        )
    }
    
    map
  })
  
  # NEW: Cases list output for non-geolocated entries - UPDATED WITH SPECIES LISTS
  output$cases_list <- renderUI({
    req(cases_data())
    cases_list_data <- cases_data()
    
    if (nrow(cases_list_data) == 0) {
      return(
        tags$div(
          style = "padding: 20px; text-align: center;",
          tags$h4("No case descriptions available"),
          tags$p("All cases for this selection have geographic coordinates and are shown on the map.")
        )
      )
    }
    
    tagList(
      tags$h3(paste("Case Descriptions for:", current_cases_element())),
      tags$p(style = "color: #666;", 
             "These cases provide additional context but don't have specific coordinates."),
      tags$hr(),
      lapply(1:nrow(cases_list_data), function(i) {
        case <- cases_list_data[i, ]
        species_list <- get_species_by_category_simple(case$siteID, country_data())
        
        tags$div(
          class = "case-description",
          tags$h4(paste("Case:", case$siteID)),
          tags$p(HTML(paste0("<b>Description:</b> ", 
                             ifelse(is.na(case$Description) | case$Description == "", 
                                    "No description available", 
                                    case$Description)))),
          tags$div(
            style = "margin-top: 10px;",
            tags$p(style = "font-weight: bold; margin-bottom: 5px;", "Species reported:"),
            tags$pre(
              style = "background-color: white; padding: 10px; border-radius: 3px; border: 1px solid #ddd; font-family: inherit;",
              species_list
            )
          )
        )
      })
    )
  })
  
  # Clear popup when clicking background or close button
  observeEvent(input$close_popup_click, {
    show_popup(FALSE)
    clicked_element(NULL)
    current_map_element(NULL)
    current_cases_element(NULL)
  })
  
  # Clear popup when clicking network background - ALSO CLEAR MAP DATA
  observeEvent(input$background_click, {
    show_popup(FALSE)
    clicked_element(NULL)
    map_cases_data(NULL)
    cases_data(NULL)
    current_map_element(NULL)
    current_cases_element(NULL)
    cases_with_coords_count(0)
    total_cases_count(0)
    cases_count(0)
  })
  
  # Reset everything
  observeEvent(input$reset, {
    updateSelectInput(session, "species_select", selected = "")
    show_popup(FALSE)
    clicked_element(NULL)
    map_cases_data(NULL)
    cases_data(NULL)
    current_map_element(NULL)
    current_cases_element(NULL)
    cases_with_coords_count(0)
    total_cases_count(0)
    cases_count(0)
    updateTabsetPanel(session, "main_tabs", selected = "network_tab")
  })
}

# Run the application
shinyApp(ui = ui, server = server)