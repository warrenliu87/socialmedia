library(shiny)
library(bslib)
library(tidyverse)
library(pROC)

# 1. Load resources
model <- readRDS("model_ctr.rds")
df <- readRDS("clean_df.rds")

# 2. Statistical averages for extra variables
avg_user <- df[1, ]
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    avg_user[[col]] <- mean(df[[col]], na.rm = TRUE)
  } else {
    avg_user[[col]] <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
  }
}

# 3. UI Definition
ui <- navbarPage(
  title = "",
  theme = bs_theme(bootswatch = "quartz"),
  
  header = tags$head(
    tags$style(HTML("
      .selectize-input { 
        background: rgba(255, 255, 255, 0.1) !important; 
        border: 1px solid rgba(255, 255, 255, 0.2) !important;
        color: white !important;
        box-shadow: none !important;
      }
      .selectize-dropdown { 
        background-color: #484c7a !important; 
        color: white !important;
        z-index: 99999 !important;
        position: absolute !important;
      }
      .selectize-dropdown .active {
        background-color: #6269ab !important;
        color: white !important;
      }
      .wellPanel { 
        margin-bottom: 30px !important; 
        min-height: 280px; 
        overflow: visible !important; 
      }
      #first-row { position: relative; z-index: 100; }
      #second-row { position: relative; z-index: 1; }
      h1 { font-size: 75px; font-weight: bold; margin-top: 30px; }
    "))
  ),
  
  tabPanel("Prediction Model",
           fluidPage(
             # Main Header
             column(12, align = "center", 
                    h1("CTR Predictor"),
                    p("Fill in the details below to estimate the user's click-through rate probability.", style = "font-size: 1.2em;"),
                    hr(style = "margin-bottom: 50px;")
             ),
             
             # Row 1: Demographics & Social
             div(id = "first-row", class = "fluid-row-spacer",
                 fluidRow(
                   column(4, wellPanel(
                     h4("Demographics"),
                     numericInput("age", "Age", value = 25, min = 13, max = 90),
                     selectInput("country", "Country", choices = sort(unique(df$country))),
                     selectInput("gender", "Gender", choices = list("Male" = 1, "Female/Other" = 0))
                   )),
                   column(4, wellPanel(
                     h4("Socio-economics"),
                     selectInput("income", "Income Level", choices = list("Low" = 0, "Mid" = 2, "High" = 4)),
                     selectInput("employment", "Employment", choices = unique(df$employment_status))
                   )),
                   column(4, wellPanel(
                     h4("Social"),
                     radioButtons("children", "Has children?", choices = list("Yes" = 1, "No" = 0), inline = TRUE)
                   ))
                 )
             ),
             
             # Row 2: Habits & lifestyle
             div(id = "second-row", class = "fluid-row-spacer",
                 fluidRow(
                   column(6, wellPanel(
                     h4("Instagram habits"),
                     sliderInput("ig_min", "Daily Minutes on IG", 0, 600, 60),
                     sliderInput("engagement", "Engagement Score (1-10)", 0, 10, 5),
                     numericInput("followers", "Followers Count", 500)
                   )),
                   column(6, wellPanel(
                     h4("Lifestyle metrics"),
                     sliderInput("sleep", "Sleep Hours", 0, 12, 7, step = 0.5),
                     sliderInput("stress", "Stress Level (1-10)", 1, 10, 5)
                   ))
                 )
             ),
             
             # Centered Action Button
             fluidRow(
               column(12, align = "center",
                      actionButton("go", "Analyze and predict", class = "btn-lg btn-primary", 
                                   style = "width: 40%; margin-top: 20px; margin-bottom: 60px; font-weight: bold;")
               )
             )
           )
  ),
  
  tabPanel("Visualizations",
           fluidPage(h3("Analysis"), p("TBC"))
  )
)

# 4. Server Logic
server <- function(input, output, session) {
  
  observeEvent(input$go, {
    user_data <- avg_user
    user_data$age <- input$age
    user_data$gender_bin <- as.numeric(input$gender)
    user_data$country <- input$country
    user_data$income_num <- as.numeric(input$income)
    user_data$employment_status <- input$employment
    user_data$daily_active_minutes_instagram <- input$ig_min
    user_data$user_engagement_score <- input$engagement
    user_data$followers_count <- input$followers
    user_data$sleep_hours_per_night <- input$sleep
    user_data$perceived_stress_score <- input$stress
    
    prob <- predict(model, newdata = user_data, type = "response")
    prob_pct <- round(prob * 100, 1)
    
    showModal(modalDialog(
      title = "Analysis complete",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(style = "text-align: center;",
          h3("Estimated Probability"),
          h1(paste0(prob_pct, "%"), style = "color: #00d1b2; font-weight: bold; font-size: 80px;"),
          p("Based on our AI model, this user has a ", strong(paste0(prob_pct, "%")), " chance of engaging with ads.")
      )
    ))
  })
}

shinyApp(ui, server)