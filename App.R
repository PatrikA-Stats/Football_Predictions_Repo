# Shiny App for football outcome predictions

# Load necessary packages
library(shiny)
library(dplyr)
library(readr)
library(lubridate)
library(DT)

source("functions.R")

#theta <- readRDS("theta_final.rds")

season_urls <- c(
  "2026-27" = "2627",
  "2025-26" = "2526",
  "2024-25" = "2425",
  "2023-24" = "2324",
  "2022-23" = "2223"
)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      table {
        border-collapse: collapse;
      }

      tbody tr:nth-child(even) {
        background-color: #f2f2f2;
      }

      tbody tr:nth-child(odd) {
        background-color: white;
      }
    "))
  ),
  
  titlePanel("Football Data Overview"),
  
  checkboxGroupInput(
    "seasons",
    "Premier League Data",
    choices = names(season_urls),
    selected = c("2026-27", "2025-26")
  ),
  
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
    
    req(input$seasons)
    
    data_list <- lapply(input$seasons, function(season) {
      
      code <- season_urls[[season]]
      
      invisible(capture.output({
        data <- read_csv(
          paste0(
            "https://www.football-data.co.uk/mmz4281/",
            code,
            "/E0.csv"
          ),
          show_col_types = FALSE
        )
      }))
      
      data |> select(
        FTHG,
        FTAG,
        FTR,
        Date,
        HomeTeam,
        AwayTeam
        )
      
    })
    
    bind_rows(data_list) |> 
      rename(
        H.goals = FTHG,
        A.goals = FTAG,
        Result = FTR
      ) |> 
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
      ) |> 
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
  
  output$summary <- renderText({
    
    req(data_clean())
    
    df <- data_clean()
    
    teams <- sort(unique(as.character(df$H.Team)))
    
    team_lines <- split(
      teams,
      ceiling(seq_along(teams) / 8)
    )
    
    teams_text <- paste(
      sapply(team_lines, paste, collapse = ", "),
      collapse = "\n"
    )
    
    paste0(
      "Rows: ", nrow(df), "\n",
      "Columns: ", ncol(df), "\n",
      "Number of teams: ",
      length(unique(c(as.character(df$H.Team),
                      as.character(df$A.Team)))), "\n",
      "Seasons: ",
      paste(unique(df$Season), collapse = ", "), "\n",
      "Date range: ",
      format(min(df$Date), "%d-%b-%Y"),
      " to ",
      format(max(df$Date), "%d-%b-%Y"), "\n\n",
      "Teams:\n",
      teams_text
    )
    
  })
  
  output$preview <- renderDT({
    
    datatable(
      data_clean() %>%
        head(10) %>%
        mutate(
          Date = format(Date, "%d-%b-%Y")
        ),
      options = list(
        pageLength = 10,
        dom = "t"
      ),
      rownames = FALSE,
      class = "stripe hover"
    )
    
  })
  
}

shinyApp(ui = ui, server = server)

