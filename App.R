# Shiny App for football outcome predictions

# Load necessary packages
library(shiny)
library(dplyr)
library(readr)
library(lubridate)

source("functions.R")

#theta <- readRDS("theta_final.rds")

ui <- fluidPage(
  
  titlePanel("Data Overview"),
  
  actionButton(
    "load_data",
    "Load and Clean Data"
  ),
  
  br(), br(),
  
  h3("Dataset Summary"),
  verbatimTextOutput("summary"),
  
  h3("Data Preview"),
  tableOutput("preview")
  
)

server <- function(input, output, session) {
  
  data_clean <- eventReactive(input$load_data, {
    
    # 2025/26 season
    data25_26_raw <- read_csv(
      "https://www.football-data.co.uk/mmz4281/2526/E0.csv",
      show_col_types = FALSE
    ) |>
      select(FTHG, FTAG, FTR, Date, HomeTeam, AwayTeam)
    
    # 2026/27 season
    data26_27_raw <- read_csv(
      "https://www.football-data.co.uk/mmz4281/2627/E0.csv",
      show_col_types = FALSE
    ) |>
      select(FTHG, FTAG, FTR, Date, HomeTeam, AwayTeam)
    
    # Clean data
    bind_rows(data25_26_raw, data26_27_raw) %>%
      rename(
        H.goals = FTHG,
        A.goals = FTAG,
        Result  = FTR
      ) %>%
      mutate(
        Date = as.Date(Date, format = "%d/%m/%Y"),
        
        Date_num = as.integer(Date - min(Date)),
        
        Season = ifelse(
          month(Date) >= 8,
          paste0(
            year(Date),
            "/",
            substr(year(Date) + 1, 3, 4)
          ),
          paste0(
            year(Date) - 1,
            "/",
            substr(year(Date), 3, 4)
          )
        ),
        
        H.Team = factor(HomeTeam),
        A.Team = factor(AwayTeam),
        
        H.goals = as.integer(H.goals),
        A.goals = as.integer(A.goals)
      ) %>%
      select(
        Season,
        Date,
        Date_num,
        H.Team,
        A.Team,
        H.goals,
        A.goals,
        Result
      ) %>%
      arrange(Date_num)
    
  })
  
  output$summary <- renderPrint({
    
    req(data_clean())
    
    df <- data_clean()
    
    cat(
      "Rows:", nrow(df), "\n",
      "Columns:", ncol(df), "\n",
      "Number of teams:",
      length(unique(c(as.character(df$H.Team),
                      as.character(df$A.Team)))), "\n",
      "Seasons:",
      paste(unique(df$Season), collapse = ", "), "\n",
      "Date range:",
      format(min(df$Date), "%d-%b-%Y"),
      "to",
      format(max(df$Date), "%d-%b-%Y")
    )
    
  })
  
  output$preview <- renderTable({
    
    req(data_clean())
    
    data_clean() %>%
      head(10) %>%
      mutate(
        Date = format(Date, "%d-%b-%Y")
      )
    
  })
  
}

shinyApp(ui = ui, server = server)

