library(shiny)
library(ggplot2)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(tigris)
library(readxl)
library(plotly)
library(highcharter)

# Load the data
vote_data <- read.csv("election_data.csv")

# Remove columns: Electoral votes, AllOthers, TotalVote, and Winner
vote_data <- vote_data[, !(names(vote_data) %in% c("Biden", "Trump", "AllOthers", "TotalVote", "Winner"))]

# Add columns for Biden and Trump vote percentage
vote_data$Biden_Percentage <- round((vote_data$Biden_P / (vote_data$Biden_P + vote_data$Trump_P)) * 100,4)
vote_data$Trump_Percentage <- round((vote_data$Trump_P / (vote_data$Biden_P + vote_data$Trump_P)) * 100,4)

# Calculate percentage difference between Biden and Trump vote share
vote_data$Vote_Share_Difference <- round(vote_data$Biden_Percentage - vote_data$Trump_Percentage,4)

file_path <- "federalelections2020.xlsx"  
# Get the sheet names
sheet_names <- excel_sheets(file_path)

# Load data from excel sheets
sheet1 <- read_excel(file_path, sheet = "2. Table 1 Pres Popular Vote", skip = 3)
sheet2 <- read_excel(file_path, sheet = "3. Table 2 Electoral & Pop Vote")
sheet5 <- read_excel(file_path, sheet = "6. Table 5 P&GVotesCastforCong")
sheet6 <- read_excel(file_path, sheet = "7. Table 6 Senate by Party")

# Data preparation for electoral and popular vote
pres_pop_vote <- sheet1 %>%
  slice(1:38) %>%
  select(1:3) %>%
  rename(Candidate = 1, `Popular Vote Total` = 2, `Percent of Popular Vote` = 3) %>%
  mutate(`Popular Vote Total` = as.numeric(`Popular Vote Total`),
         `Percent of Popular Vote` = as.numeric(`Percent of Popular Vote`))

electoral_pop_vote <- sheet2 %>%
  slice(4:54) %>%
  select(c(1, 4, 5, 7)) %>%
  rename(State = 1, `Biden (D)` = 2, `Trump (R)` = 3, `Total Vote` = 4) %>%
  mutate(`Biden (D)` = as.numeric(`Biden (D)`),
         `Trump (R)` = as.numeric(`Trump (R)`),
         `Total Vote` = as.numeric(`Total Vote`),
         `Biden %` = `Biden (D)` / `Total Vote` * 100,
         `Trump %` = `Trump (R)` / `Total Vote` * 100)

senate_vote <- sheet6 %>%
  slice(4:59) %>%
  select(c(1, 5, 6)) %>%
  rename(State = 1, Democratic = 2, Republican = 3) %>%
  mutate(Democratic = as.numeric(Democratic),
         Republican = as.numeric(Republican))

# Data for leaflet map (from first segment)
election_data <- read_excel(file_path, sheet = "3. Table 2 Electoral & Pop Vote", skip = 3)
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

us_states <- tigris::states(cb = TRUE, resolution = "20m", year = 2020, class = "sf") %>%
  st_transform(crs = 4326) %>%
  mutate(State = tolower(STUSPS))

map_data <- us_states %>%
  left_join(election_data, by = c("STUSPS" = "States"))

# UI
ui <- fluidPage(
  titlePanel("2020 U.S. Election Data Visualization"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("2020 Presidential Election Overview", 
               highchartOutput("popularity_pie_chart", height = "400px"),
               fluidRow(
                 column(6, 
                        selectInput("candidate", "Select Candidate:", 
                                    choices = unique(pres_pop_vote$Candidate))),
                 column(6, 
                        textOutput("candidate_vote_info"))
               )
      ),
      
      tabPanel("Presidential Electoral and Popular Vote by State", 
               sidebarLayout(
                 sidebarPanel(
                   h4("Electoral Vote Details"),
                   uiOutput("electoral_info"),
                   br(),
                   h4("Popular Vote Details"),
                   uiOutput("popular_info")
                 ),
                 mainPanel(
                   leafletOutput("us_map", height = "600px")
                 )
               )),
      
      tabPanel("Percentage Difference in R/D Vote Share", 
               fluidRow(
                 column(6,
                        plotOutput("votePlot", height = "800px")
                 ),
                 column(6,
                        DT::dataTableOutput("voteTable")
                 )
               ))
    )
  )
)

# Server
server <- function(input, output, session) {
  selected_state <- reactiveVal(NULL)
  
  # Popularity Vote Pie Chart
  output$popularity_pie_chart <- renderHighchart({
    pie_data <- pres_pop_vote %>%
      na.omit() %>%
      select(Candidate, `Popular Vote Total`)
    
    total_votes <- sum(pie_data$`Popular Vote Total`)
    pie_data <- pie_data %>%
      mutate(Percent = `Popular Vote Total` / total_votes * 100)
    
    threshold <- 5
    other_votes <- sum(pie_data$Percent[pie_data$Percent < threshold])
    pie_data <- pie_data %>%
      filter(Percent >= threshold) %>%
      bind_rows(data.frame(Candidate = "Other", `Popular Vote Total` = NA, Percent = other_votes))
    
    custom_colors <- c("blue", "red", "gray")
    
    hchart(
      pie_data,
      type = "pie",
      hcaes(name = Candidate, y = Percent),
      name = "Vote Share"
    ) %>%
      hc_colors(colors = custom_colors) %>%
      hc_tooltip(
        pointFormat = "<b>{point.name}</b><br>Percentage: {point.y:.2f}%<br>Popular Vote Total: {point.Popular_Vote_Total}"
      ) %>%
      hc_title(text = "National Presidential Popular Vote Distribution") %>%
      hc_plotOptions(pie = list(
        dataLabels = list(
          enabled = TRUE,
          format = "<b>{point.name}</b>: {point.percentage:.1f} %"
        )
      ))
  })
  
  # Render candidate vote information
  output$candidate_vote_info <- renderText({
    selected_candidate <- input$candidate
    candidate_row <- pres_pop_vote %>% filter(Candidate == selected_candidate)
    if (nrow(candidate_row) > 0) {
      vote_count <- candidate_row$`Popular Vote Total`
      percentage <- candidate_row$`Percent of Popular Vote`
      paste0("Total Votes: ", vote_count, " (", 100*round(percentage, 4), "%)")
    } else {
      "No data available"
    }
  })
  
  # Render Leaflet Map
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
  
  # Capture click event to update selected state
  observeEvent(input$us_map_shape_click, {
    clicked_state <- input$us_map_shape_click$id
    selected_state(clicked_state)
  })
  
  # Render Electoral Vote Information
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
      "<strong>Trump (R):</strong> ", state_data$Trump, " votes "
    ))
  })
  
  # Render Popular Vote Information
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
  
  # Sorted data for vote share difference
  sorted_data <- reactive({
    vote_data[order(vote_data$Vote_Share_Difference), ]
  })
  
  # Render interactive data table for vote share difference
  output$voteTable <- DT::renderDataTable({
    datatable(sorted_data(), options = list(pageLength = 100, order = list(list(4, 'asc'))),
              selection = 'none',
              colnames = c('States', 'Biden_P', 'Trump_P', 'Biden_Percentage', 'Trump_Percentage', 'Vote_Share_Difference')) %>%
      formatStyle(
        columns = c('Vote_Share_Difference', 'Biden_Percentage', 'Trump_Percentage'),
        cursor = 'pointer'
      )
  })
  
  # Render plot for vote share difference
  output$votePlot <- renderPlot({
    data <- sorted_data()
    ggplot(data, aes(x = reorder(States, Vote_Share_Difference), y = Vote_Share_Difference, fill = Vote_Share_Difference > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("red", "blue"), labels = c("Republican Win", "Democratic Win")) +
      labs(title = "Vote Share Difference by State",
           x = "State",
           y = "Vote Share Difference (%)",
           fill = "Result") +
      theme_minimal()
  })
}

# Launch the Shiny App
shinyApp(ui = ui, server = server)


