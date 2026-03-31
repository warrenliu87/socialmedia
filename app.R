library(shiny)
library(bslib)
library(tidyverse)
library(pROC)

# 1. Load resources
model <- readRDS("model_ctr.rds")
df <- readRDS("clean_df.rds")

# 2. Setup average user template
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
  title = "Social Media Analytics",
  theme = bs_theme(bootswatch = "quartz"),
  
  header = tags$head(
    tags$style(HTML("
      .selectize-input, .form-control { 
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
        margin-bottom: 20px !important; 
        min-height: 280px; 
        overflow: visible !important; 
      }
      #first-row { 
        position: relative; 
        z-index: 100; 
      }
      #second-row { 
        position: relative; 
        z-index: 1; 
        margin-top: 60px !important; 
      }
      h1 { font-size: 70px; font-weight: bold; margin-top: 30px; }
      h4 { font-weight: bold; margin-bottom: 15px; }
      .btn-primary { background-color: #00d1b2 !important; border: none; }
    "))
  ),
  
  tabPanel("Prediction Model",
           fluidPage(
             column(12, align = "center", 
                    h1("CTR predictor"),
                    p("Estimate a user's Click-Through Rate", 
                      style = "font-size: 1.2em; opacity: 0.8;"),
                    hr(style = "margin-bottom: 40px;")
             ),
             
             div(id = "first-row",
                 fluidRow(
                   column(4, wellPanel(
                     h4("Demographics"),
                     numericInput("age", "Age", value = 25, min = 13, max = 90),
                     selectInput("country", "Country", 
                                 choices = c("Spain", "United States", "United Kingdom", "Germany", 
                                             "Canada", "Brazil", "India", "Japan", "South Korea", "Other")),
                     selectInput("gender", "Gender", choices = list("Male" = 1, "Female/Other" = 0))
                   )),
                   column(4, wellPanel(
                     h4("Socio-economics"),
                     selectInput("income", "Income Level", choices = list("Low" = 0, "Mid" = 2, "High" = 4)),
                     selectInput("employment", "Employment", choices = unique(df$employment_status))
                   )),
                   column(4, wellPanel(
                     h4("Social and family"),
                     radioButtons("children", "Has children?", choices = list("Yes" = 1, "No" = 0), inline = TRUE),
                     selectInput("education", "Education level", choices = unique(df$education_level))
                   ))
                 )
             ),
             
             div(id = "second-row",
                 fluidRow(
                   column(6, wellPanel(
                     h4("Instagram habits"),
                     sliderInput("ig_min", "Daily minutes on IG", 0, 600, 60),
                     sliderInput("engagement", "Engagement score (1-10)", 0, 10, 5),
                     numericInput("followers", "Followers count", 500)
                   )),
                   column(6, wellPanel(
                     h4("Lifestyle"),
                     sliderInput("sleep", "Sleep hours", 0, 12, 7, step = 0.5),
                     sliderInput("stress", "Stress level (1-10)", 1, 10, 5)
                   ))
                 )
             ),
             
             fluidRow(
               column(12, align = "center",
                      actionButton("go", "Analyze", class = "btn-lg btn-primary", 
                                   style = "width: 40%; margin-top: 30px; margin-bottom: 50px; font-weight: bold;")
               )
             )
           )
  ),
  
  tabPanel("Visualizations",
           fluidPage(
             column(12, align = "center",
                    hr()
             )
           )
  )
)

# 4. Server Logic
server <- function(input, output, session) {
  
  observeEvent(input$go, {
    user_data <- avg_user
    
    user_data$age <- input$age
    user_data$gender_bin <- as.numeric(input$gender)
    
    valid_countries <- c("Brazil", "Canada", "Germany", "India", "Japan", "South Korea", "United Kingdom", "United States")
    user_data$country <- if(input$country %in% valid_countries) input$country else "Other"
    
    user_data$income_num <- as.numeric(input$income)
    user_data$employment_status <- input$employment
    user_data$education_level <- input$education
    user_data$daily_active_minutes_instagram <- input$ig_min
    user_data$user_engagement_score <- input$engagement
    user_data$followers_count <- input$followers
    user_data$sleep_hours_per_night <- input$sleep
    user_data$perceived_stress_score <- input$stress
    
    prob <- predict(model, newdata = user_data, type = "response")
    prob_pct <- round(prob * 100, 1)
    
    showModal(modalDialog(
      title = "Prediction Results",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(style = "text-align: center; padding: 20px;",
          h3("Ad Engagement Probability"),
          h1(paste0(prob_pct, "%"), style = "color: #00d1b2; font-weight: bold; font-size: 90px;"),
          p("Likelihood of this user clicking on an Instagram ad.")
      )
    ))
  })
}

shinyApp(ui, server)