library(shiny)
library(ggplot2)
library(dplyr)

library(readxl)
setwd("~/my_app")
file_path <- "My_App/federalelections2020.xlsx"  
# Get the sheet names
sheet_names <- excel_sheets(file_path)

sheet1 <- read_excel(file_path, sheet = "2. Table 1 Pres Popular Vote")
sheet2 <- read_excel(file_path, sheet = "3. Table 2 Electoral & Pop Vote")
sheet5 <- read_excel(file_path, sheet = "6. Table 5 P&GVotesCastforCong")
sheet6 <- read_excel(file_path, sheet = "7. Table 6 Senate by Party")

library(dplyr)
pres_pop_vote <- sheet1 %>%
  slice(4:41) %>%
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
# 

ui <- fluidPage(
  titlePanel("2020 U.S. Election Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select Dataset:", choices = c("Presidential Popular Vote", "Electoral and Popular Vote", "Senate Vote")),
      uiOutput("dynamic_selector") 
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Overview", tableOutput("data_view")),
        tabPanel("Popularity Vote Pie Chart", plotOutput("popularity_pie_chart")),
        tabPanel("Vote Percentage by State", 
                 plotOutput("vote_percentage_electoral"),
                 plotOutput("vote_percentage_senate")),
        tabPanel("State Vote Comparison", 
                 plotOutput("electoral_state_vote_comparison"),
                 plotOutput("senate_state_vote_comparison"))
      )
    )
  )
)

# 
server <- function(input, output, session) {
  
  
  output$dynamic_selector <- renderUI({
    if (input$dataset == "Presidential Popular Vote") {
      selectInput("candidate", "Select Candidate:", choices = unique(pres_pop_vote$Candidate))
    } else {
      selectInput("state", "Select State:", choices = unique(electoral_pop_vote$State))
    }
  })
  
  # 
  output$data_view <- renderTable({
    if (input$dataset == "Presidential Popular Vote") {
      if (!is.null(input$candidate)) {
        data <- pres_pop_vote %>% filter(Candidate == input$candidate)
      } else {
        data <- pres_pop_vote
      }
    } else if (input$dataset == "Electoral and Popular Vote") {
      if (!is.null(input$state)) {
        data <- electoral_pop_vote %>% filter(State == input$state)
      } else {
        data <- electoral_pop_vote
      }
    } else {
      if (!is.null(input$state)) {
        data <- senate_vote %>% filter(State == input$state)
      } else {
        data <- senate_vote
      }
    }
    data
  })
  
  # Popularity Vote Pie Chart
  output$popularity_pie_chart <- renderPlot({
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
    
    pie(pie_data$Percent, labels = paste(pie_data$Candidate, round(pie_data$Percent, 1), "%"), main = "2020 Presidential Popular Vote Distribution", col = rainbow(nrow(pie_data)))
    legend("topright", legend = pie_data$Candidate, fill = rainbow(nrow(pie_data)), cex = 0.8)
  })
  
  # Vote Percentage by State Bar Charts
  output$vote_percentage_electoral <- renderPlot({
    ggplot(electoral_pop_vote, aes(x = State)) +
      geom_bar(aes(y = `Biden %`), stat = "identity", fill = "blue", position = position_dodge(width = 0.8), width = 0.4) +
      geom_bar(aes(y = `Trump %`), stat = "identity", fill = "red", position = position_dodge(width = 0.8), width = 0.4) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      labs(title = "Vote Percentage by Candidate in 2020 Presidential Election (Electoral)", y = "Percentage of Votes (%)", x = "State")
  })
  
  output$vote_percentage_senate <- renderPlot({
    ggplot(senate_vote, aes(x = State)) +
      geom_bar(aes(y = `Democratic`), stat = "identity", fill = "blue", position = position_dodge(width = 0.8), width = 0.4) +
      geom_bar(aes(y = `Republican`), stat = "identity", fill = "red", position = position_dodge(width = 0.8), width = 0.4) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      labs(title = "Vote Percentage by Party in Senate Election", y = "Percentage of Votes (%)", x = "State")
  })
  
  # State Vote Comparison Pie Charts with Percentage Labels
  output$electoral_state_vote_comparison <- renderPlot({
    if (!is.null(input$state)) {
      electoral_data <- electoral_pop_vote %>% filter(State == input$state)
      if (nrow(electoral_data) > 0 && !is.na(electoral_data$`Biden (D)`) && !is.na(electoral_data$`Trump (R)`)) {
        electoral_votes <- c(electoral_data$`Biden (D)`, electoral_data$`Trump (R)`)
        electoral_percent <- round(electoral_votes / sum(electoral_votes) * 100, 1)
        pie(electoral_votes, labels = paste(c("Biden", "Trump"), ": ", electoral_percent, "%", sep = ""),
            main = paste("Electoral Vote Distribution in", input$state),
            col = c("blue", "red"))
      } else {
        plot.new()
        text(0.5, 0.5, "Data for this state is missing. Unable to display electoral vote pie chart.", cex = 1.2)
      }
    }
  })
  
  output$senate_state_vote_comparison <- renderPlot({
    if (!is.null(input$state)) {
      senate_data <- senate_vote %>% filter(State == input$state)
      if (nrow(senate_data) > 0 && !is.na(senate_data$Democratic) && !is.na(senate_data$Republican)) {
        senate_votes <- c(senate_data$Democratic, senate_data$Republican)
        senate_percent <- round(senate_votes / sum(senate_votes) * 100, 1)
        pie(senate_votes, labels = paste(c("Democratic", "Republican"), ": ", senate_percent, "%", sep = ""),
            main = paste("Senate Vote Distribution in", input$state),
            col = c("blue", "red"))
      } else {
        plot.new()
        text(0.5, 0.5, "Data for this state is missing. Unable to display senate vote pie chart.", cex = 1.2)
      }
    }
  })
}

# 
shinyApp(ui = ui, server = server)
