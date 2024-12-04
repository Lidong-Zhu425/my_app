library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(readxl)

#setwd("my_app/My_App")
file_path <- "federalelections2020.xlsx"
sheet_name <- "3. Table 2 Electoral & Pop Vote"
election_data <- read_excel(file_path, sheet = sheet_name, skip = 3)
colnames(election_data)[1] <- "States"
election_data <- election_data %>%
  mutate(States = gsub("\\*", "", States))
election_data <- election_data[1:51, ]
election_data <- election_data %>%
  rename(
    Biden = `Biden (D)...2`,
    Trump = `Trunp (R)...3`,
    Biden_P = `Biden (D)...4`,
    Trump_P = `Trunp (R)...5`,
    AllOthers = `All Others`,
    TotalVote = `Total Vote`
  ) %>%
  mutate(
    Biden = ifelse(is.na(Biden), 0, as.numeric(Biden)),
    Trump = ifelse(is.na(Trump), 0, as.numeric(Trump)),
    AllOthers = ifelse(is.na(AllOthers), 0, as.numeric(AllOthers)),
    TotalVote = ifelse(is.na(TotalVote), 0, as.numeric(TotalVote)),
    Winner = ifelse(Biden > Trump, "blue", "red")
  )

# Get US state border data (sf format)
us_states <- tigris::states(cb = TRUE, resolution = "20m", year = 2020, class = "sf") %>%
  st_transform(crs = 4326) %>%
  mutate(State = tolower(STUSPS))  # Convert the state abbreviation to lowercase to match

# Merge border data and election data
map_data <- us_states %>%
  left_join(election_data, by = c("STUSPS" = "States"))

# UI
ui <- fluidPage(
  titlePanel("Interactive US Election Map"),
  
  # Map above
  fluidRow(
    column(
      width = 12,
      leafletOutput("us_map", height = "600px")  # Map is displayed at the top of the page
    )
  ),
  
  # Electoral and popular vote details are shown in two columns below
  fluidRow(
    column(
      width = 6,
      h4("Electoral Vote Details"),
      uiOutput("electoral_info")
    ),
    column(
      width = 6,
      h4("Popular Vote Details"),
      uiOutput("popular_info")
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_state <- reactiveVal(NULL)
  
  # Render electoral vote information
  output$electoral_info <- renderUI({
    state <- selected_state()
    if (is.null(state)) {
      return("Click on a state to see the electoral vote details.")
    }
    state_data <- map_data %>%
      filter(State == state)
    HTML(paste0(
      "<strong>State:</strong> ", toupper(state_data$State), "<br>",
      "<strong>Winner:</strong> ", ifelse(state_data$Winner == "blue", "Biden (D)", "Trump (R)"), "<br>",
      "<strong>Biden (D):</strong> ", state_data$Biden, " votes<br> ",
      "<strong>Trump (D):</strong> ", state_data$Trump, " votes "
    ))
  })
  
  # Render popular vote information
  output$popular_info <- renderUI({
    state <- selected_state()
    if (is.null(state)) {
      return("Click on a state to see the popular vote details.")
    }
    state_data <- map_data %>%
      filter(State == state)
    HTML(paste0(
      "<strong>Biden (D):</strong> ", state_data$Biden_P, " votes<br> ",
      "<strong>Trump (R):</strong> ", state_data$Trump_P, " votes<br> ",
      "<strong>All Others:</strong> ", state_data$AllOthers, " votes<br>",
      "<strong>Total Votes:</strong> ", state_data$TotalVote
    ))
  })
  
  # Render map
  output$us_map <- renderLeaflet({
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%  
      addPolygons(
        fillColor = ~Winner,  
        weight = 1,
        color = "white",     
        fillOpacity = 0.7,   
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        layerId = ~State,  
        label = ~paste0(NAME),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
  
  # Capture click events to update selected states
  observeEvent(input$us_map_shape_click, {
    clicked_state <- input$us_map_shape_click$id
    selected_state(clicked_state)
  })
}

shinyApp(ui = ui, server = server)