library(shiny)
library(bslib)
library(tidyverse)

# Note: Remember to load your model and clean data here before running
# model <- readRDS("model_ctr.rds")

ui <- navbarPage(
  title = "Instagram Ad-Engagement",
  theme = bs_theme(bootswatch = "quartz"),
  
  # Main Prediction Tab
  tabPanel("Prediction Model",
           sidebarLayout(
             sidebarPanel(
               h4("User Profile"),
               numericInput("age", "Age", value = 25, min = 13, max = 100),
               selectInput("gender", "Gender", choices = list("Male" = 1, "Female/Other" = 0)),
               selectInput("income", "Income Level", choices = list("Low" = 0, "Lower-Middle" = 1, "Middle" = 2, "Upper-Middle" = 3, "High" = 4)),
               selectInput("edu", "Education", choices = list("Other" = 0, "Secondary" = 1, "High School" = 2, "Some College" = 3, "Bachelor" = 4, "Master" = 5, "PhD" = 6)),
               radioButtons("children", "Has Children?", choices = list("Yes" = 1, "No" = 0), inline = TRUE),
               
               hr(),
               h4("Usage & Lifestyle"),
               sliderInput("ig_min", "Daily Instagram Minutes", 0, 600, 60),
               sliderInput("engagement", "Engagement Score", 0, 10, 5),
               numericInput("followers", "Followers", 500),
               numericInput("following", "Following", 300),
               sliderInput("sleep", "Sleep Hours", 0, 12, 7, step = 0.5),
               sliderInput("stress", "Stress Score (1-10)", 1, 10, 5),
               selectInput("diet", "Diet Quality", choices = list("Very Poor" = 0, "Poor" = 1, "Average" = 2, "Good" = 3, "Excellent" = 4)),
               radioButtons("smoking", "Smoker?", choices = list("Yes" = 1, "No" = 0), inline = TRUE),
               selectInput("alcohol", "Alcohol Frequency", choices = list("Never" = 0, "Rarely" = 1, "Weekly" = 2, "Several times/week" = 3, "Daily" = 4)),
               
               hr(),
               h4("Security Settings"),
               radioButtons("tfa", "2FA Enabled?", choices = list("Yes" = 1, "No" = 0), inline = TRUE),
               radioButtons("bio", "Biometric Login?", choices = list("Yes" = 1, "No" = 0), inline = TRUE),
               
               actionButton("go", "Predict CTR Probability", class = "btn-lg btn-primary w-100")
             ),
             mainPanel(
               h3("Prediction Results"),
               textOutput("res_text")
             )
           )
  ),
  
  # Visualizations Tab
  tabPanel("Visualizations",
           fluidPage(
             h3("Market Insights"),
             p("Visual analytics placeholder for Jan's section.")
           )
  )
)

server <- function(input, output, session) {
  # Logic to be implemented
}

shinyApp(ui, server)