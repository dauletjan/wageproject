library(shiny)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(tidycensus)
library(tmap)
library(sf)
library(ggiraph)
library(markdown)  # Added library for includeMarkdown

# Deauthorize so we can read public sheets without login
gs4_deauth()

# --- Load and Clean MIT Living Wage Data ---
mitlivingwage <- read_sheet("https://docs.google.com/spreadsheets/d/1DJXu2NXVb4pgCZDiGMzg1QnVGRWkE3Gv2kJzC5Tstt4/edit?usp=sharing", sheet = "Sheet1")

mitlivingwage_clean <- mitlivingwage %>%
  rename(
    living_wage = `...2`,
    state = `Living Wage (2 Adults, Both Working, 2 Children):`,
    min_wage = `...4`
  ) %>%
  mutate(
    state = tolower(state), 
    living_wage = as.numeric(gsub("[^0-9.]", "", living_wage)),
    min_wage = as.numeric(gsub("[^0-9.]", "", min_wage)),
    wage_gap = living_wage - min_wage
  ) %>%
  select(state, living_wage, min_wage, wage_gap)

# --- Load and Clean Housing Cost Burden Data ---
housing_cost_burden <- read_sheet(
  "https://docs.google.com/spreadsheets/d/178TklM1g0He_eQR6vyyZLF5b-qHh0gqjxGsQ_jGYzgA/edit?usp=sharing", 
  sheet = "Sheet1"
)

housing_cost_clean <- housing_cost_burden %>%
  rename(
    state = `State`,
    housing_burden = `Housing cost burden percentage`
  ) %>%
  mutate(
    state = tolower(state),
    housing_burden = as.numeric(gsub("[^0-9.]", "", housing_burden))
  ) %>%
  select(state, housing_burden)

# --- Load ACS Housing Cost Data for Map Geometry ---
us_map <- get_acs(
  geography = "state",
  variables = "B01001A_001", 
  year = 2022,
  geometry = TRUE
) %>%
  mutate(state = tolower(NAME)) %>%
  filter(!state %in% c("hawaii", "alaska", "puerto rico")) %>%
  select(state, geometry)

# --- Merge All Data ---
map_data <- us_map %>%
  left_join(mitlivingwage_clean, by = "state") %>%
  left_join(housing_cost_clean, by = "state") # Include housing cost burden

# --- Define UI ---
ui <- fluidPage(
  titlePanel("Wage Gap and Housing Cost Analysis"),
  navbarPage("",
             tabPanel("User Guide",
                      includeMarkdown("userGuide.md")  # Embedded User Guide
             ),
             tabPanel("Map View",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Visualize Key Metrics"),
                          p("This map shows wage metrics across U.S. states."),
                          selectInput("map_measure", "Select Measure:", 
                                      choices = c("Living Wage", "Minimum Wage", "Wage Gap"))
                        ),
                        mainPanel(
                          tmapOutput("mapPlot"),
                          h5("Data Source: MIT Living Wage Calculator")
                        )
                      )
             ),
             tabPanel("Bar Chart",
                      sidebarLayout(
                        sidebarPanel(
                          p("Compare Living and Minimum Wages by State")
                        ),
                        mainPanel(
                          plotOutput("barPlot")
                        )
                      )
             ),
             tabPanel("Scatterplot",
                      sidebarLayout(
                        sidebarPanel(
                          p("Wage Gap vs. Minimum Wage"),
                          p("Each point represents a state.")
                        ),
                        mainPanel(
                          girafeOutput("scatterPlot")
                        )
                      )
             ),
             tabPanel("Housing Burden Map",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Housing Cost Burden Map"),
                          p("This map visualizes the percentage of income households spend on housing costs with a mortgage."),
                          p("**Definition:** Spending more than 30% of household income on housing costs is considered 'housing cost burdened.'"),
                          p("**Relevance:** This metric highlights where housing affordability is a major concern. High housing burdens, combined with low wages, show where financial challenges are most severe."),
                          p("**Source:** ACS Housing Costs Dataset (ESRI Hub)")
                        ),
                        mainPanel(
                          tmapOutput("housingBurdenMap"),
                          h6("Note: Missing data for some states may reflect unavailable or incomplete ACS data.")
                        )
                      )
             ),
             tabPanel("Housing Burden vs. Wage Gap",
                      sidebarLayout(
                        sidebarPanel(
                          p("Compare Housing Cost Burden with Wage Gap"),
                          p("This scatterplot visualizes the relationship between housing cost burdens and wage gaps by state."),
                          p("**Key Insight:** States with higher housing cost burdens tend to show larger wage gaps, emphasizing where wages fall short relative to housing costs.")
                        ),
                        mainPanel(
                          girafeOutput("housingScatterPlot")
                        )
                      )
             )
  )
)

# --- Define Server ---
server <- function(input, output, session) {
  # Set tmap to interactive mode
  tmap_mode("view")
  
  # Map Visualization
  output$mapPlot <- renderTmap({
    if (input$map_measure == "Living Wage") {
      tm_shape(map_data) +
        tm_polygons("living_wage", palette = "Purples", title = "Living Wage ($/hr)")
    } else if (input$map_measure == "Minimum Wage") {
      tm_shape(map_data) +
        tm_polygons("min_wage", palette = "Blues", title = "Minimum Wage ($/hr)")
    } else {
      tm_shape(map_data) +
        tm_polygons("wage_gap", palette = "Reds", title = "Wage Gap ($)")
    }
  })
  
  # Housing Cost Burden Map
  output$housingBurdenMap <- renderTmap({
    tm_shape(map_data) +
      tm_polygons("housing_burden", palette = "Oranges", title = "Housing Cost Burden (%)")
  })
  
  # Bar Chart Visualization
  output$barPlot <- renderPlot({
    ggplot(mitlivingwage_clean, aes(x = living_wage, y = reorder(state, -living_wage))) +
      geom_errorbarh(aes(xmin = min_wage, xmax = living_wage), height = 0.3) +
      labs(title = "Living Wage vs. Minimum Wage by State", x = "Wage ($/hr)", y = "State") +
      theme_minimal()
  })
  
  # Scatterplot Visualization: Wage Gap vs. Minimum Wage
  output$scatterPlot <- renderGirafe({
    p <- ggplot(mitlivingwage_clean, aes(x = min_wage, y = wage_gap)) +
      geom_point_interactive(aes(tooltip = state), color = "darkblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(
        title = "Wage Gap vs. Minimum Wage",
        x = "Minimum Wage ($/hr)",
        y = "Wage Gap ($)"
      ) +
      theme_minimal()
    
    girafe(ggobj = p)
  })
  
  # Scatterplot: Housing Burden vs. Wage Gap
  output$housingScatterPlot <- renderGirafe({
    p <- ggplot(map_data, aes(x = housing_burden, y = wage_gap)) +
      geom_point_interactive(aes(tooltip = state), color = "orange") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(
        title = "Housing Cost Burden vs. Wage Gap",
        x = "Housing Cost Burden (%)",
        y = "Wage Gap ($)"
      ) +
      theme_minimal()
    girafe(ggobj = p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)