library(shiny)
library(bslib)
library(tidyverse)
library(pROC)
library(plotly)
library(scales)

# ==========================================
# 1. LOAD RESOURCES & SETUP
# ==========================================
model <- readRDS("model_ctr.rds")
df <- readRDS("clean_df.rds")

# Instagram Branding Colors
instagram_cols <- c(pink="#E1306C", purple="#C13584", orange="#F77737", yellow="#FCAF45", blue="#405DE6")

# Setup average user template (fill numeric with mean, character with mode)
avg_user <- df[1, ]
for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    avg_user[[col]] <- mean(df[[col]], na.rm = TRUE)
  } else {
    avg_user[[col]] <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
  }
}

# ==========================================
# 2. UI DEFINITION
# ==========================================
ui <- navbarPage(
  title = "Social Media Analytics",
  theme = bs_theme(bootswatch = "quartz", primary = "#00d1b2"),

  header = tags$head(
    tags$style(HTML("
      .wellPanel {
        background: rgba(255, 255, 255, 0.05) !important;
        border: 1px solid rgba(255, 255, 255, 0.1) !important;
        padding: 20px;
        border-radius: 8px;
        height: 100%;
      }
      h1 { font-size: 50px; font-weight: bold; }
      .btn-primary { background: linear-gradient(45deg, #405DE6, #E1306C) !important; border: none; color: white; }
      .btn-secondary { background: rgba(255,255,255,0.1); border: 1px solid rgba(255,255,255,0.2); color: white; }
    "))
  ),

  # --- TAB 1: PREDICTION MODEL ---
  tabPanel("Prediction Model",
    fluidPage(
      column(12, align = "center", h1("CTR Predictor"), hr(style = "margin-bottom: 30px;")),

      fluidRow(
        column(4, wellPanel(
          h4("Demographics"),
          numericInput("age", "Age", value = 25, min = 13, max = 90),
          selectInput("country", "Country", choices = sort(unique(df$country))),
          selectInput("gender", "Gender", choices = list("Male" = 1, "Female/Other" = 0))
        )),
        column(4, wellPanel(
          h4("Socio-economics"),
          selectInput("income", "Income Level",
            choices = list("Low" = 0, "Lower-middle" = 1, "Middle" = 2, "Upper-middle" = 3, "High" = 4),
            selected = 2),
          selectInput("employment", "Employment", choices = sort(unique(df$employment_status)))
        )),
        column(4, wellPanel(
          h4("Social and family"),
          radioButtons("children", "Has children?", choices = list("Yes" = 1, "No" = 0), inline = TRUE),
          selectInput("education", "Education level", choices = list(
            "Other" = 0, "Secondary" = 1, "High School" = 2,
            "Some College" = 3, "Bachelor" = 4, "Masters" = 5, "PhD" = 6
          ))
        ))
      ),

      div(style = "height: 40px;"),

      fluidRow(
        column(6, wellPanel(
          h4("Instagram habits"),
          sliderInput("ig_min", "Daily minutes on IG", 0, 600, 120),
          sliderInput("engagement", "Engagement score (1-10)", 0, 10, 5),
          numericInput("followers", "Followers count", 500)
        )),
        column(6, wellPanel(
          h4("Lifestyle"),
          sliderInput("sleep", "Sleep hours", 0, 12, 7, step = 0.5),
          sliderInput("stress", "Stress level (1-10)", 1, 10, 5)
        ))
      ),

      div(style = "height: 40px;"),

      fluidRow(
        column(12, align = "center",
          actionButton("go", "Analyze Predictor", class = "btn-lg btn-primary", style = "width: 40%; margin-bottom: 50px;")
        )
      )
    )
  ),

  # --- TAB 2: VISUALIZATIONS ---
  tabPanel("Visualizations",
    page_sidebar(
      sidebar = sidebar(
        title = "Dashboard Controls",
        actionButton("sync", "Plot Specific User", icon = icon("crosshairs"), class = "btn-primary mb-2"),
        actionButton("reset", "Reset Visualizations", icon = icon("undo"), class = "btn-secondary mb-4"),
        hr(),
        h5("Current View:"),
        uiOutput("view_status"),
        helpText("Use the buttons above to toggle between showing the entire dataset and pinpointing the exact user you built in the Predictor tab.")
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Sleep vs IG Usage"),
          plotlyOutput("sleep_plot")
        ),
        layout_column_wrap(
          width = 1/2,
          card(card_header("Engagement Heatmap"), plotlyOutput("heatmap_plot")),
          card(card_header("Follower Dynamics"), plotlyOutput("growth_plot"))
        )
      )
    )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {

  # --- UI State Tracking ---
  view_state <- reactiveVal("all")

  observeEvent(input$sync, { view_state("specific") })
  observeEvent(input$reset, { view_state("all") })

  output$view_status <- renderUI({
    if (view_state() == "all") {
      HTML("<span style='color: #a0aec0;'>Showing All Demographic Data</span>")
    } else {
      HTML("<span style='color: #00d1b2; font-weight: bold;'>Highlighting Predicted User</span>")
    }
  })

  # --- Data Assembly ---
  predicted_user <- reactive({
    u <- avg_user
    u$age <- input$age
    u$country <- input$country
    u$gender_bin <- as.numeric(input$gender)
    u$income_num <- as.numeric(input$income)
    u$education_num <- as.numeric(input$education)
    u$employment_status <- input$employment
    u$has_children <- as.numeric(input$children)
    u$sleep_hours_per_night <- input$sleep
    u$daily_active_minutes_instagram <- input$ig_min
    u$followers_count <- input$followers
    u$user_engagement_score <- input$engagement
    u$perceived_stress_score <- input$stress
    u
  })

  # --- Prediction Logic ---
  observeEvent(input$go, {
    user_data <- predicted_user()

    prob <- predict(model, newdata = user_data, type = "response")
    prob_pct <- round(prob * 100, 1)

    showModal(modalDialog(
      title = "Prediction Results",
      div(style = "text-align:center",
        h1(paste0(prob_pct, "%"), style = "color:#00d1b2; font-size:80px;"),
        p("Ad Engagement Probability")
      ),
      easyClose = TRUE
    ))
  })

  # --- Plot Renderers ---

  # 1. Scatter: Sleep vs IG
  output$sleep_plot <- renderPlotly({
    d_bg <- df %>% slice_sample(n = 3000)

    p <- ggplot() +
      geom_point(data = d_bg, aes(x = sleep_hours_per_night, y = daily_active_minutes_instagram),
                 color = "lightgrey", alpha = 0.3, size = 1) +
      theme_minimal() +
      labs(x = "Sleep Hours", y = "Daily IG Minutes", color = "Stress")

    if (view_state() == "all") {
      p <- p + geom_point(data = d_bg,
        aes(x = sleep_hours_per_night, y = daily_active_minutes_instagram, color = perceived_stress_score),
        alpha = 0.6) +
        scale_color_gradientn(colors = c(instagram_cols["yellow"], instagram_cols["pink"], instagram_cols["purple"]))
    } else {
      p <- p + geom_point(data = predicted_user(),
        aes(x = sleep_hours_per_night, y = daily_active_minutes_instagram),
        color = instagram_cols["pink"], size = 6)
    }

    ggplotly(p)
  })

  # 2. Heatmap: Demographic Engagement
  output$heatmap_plot <- renderPlotly({
    p_data <- df %>%
      mutate(age_group = cut(age, breaks = c(0, 25, 45, 65, Inf), labels = c("GenZ", "Millennial", "GenX", "Senior"))) %>%
      group_by(age_group, income_num) %>%
      summarise(avg_eng = mean(user_engagement_score, na.rm = TRUE), .groups = "drop")

    p <- ggplot(p_data, aes(x = factor(income_num), y = age_group, fill = avg_eng)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = c(instagram_cols["blue"], instagram_cols["pink"])) +
      theme_minimal() +
      labs(x = "Income Level (0=Low, 4=High)", y = "Age Group", fill = "Avg Eng.")

    if (view_state() == "specific") {
      u_age_group <- cut(predicted_user()$age, breaks = c(0, 25, 45, 65, Inf), labels = c("GenZ", "Millennial", "GenX", "Senior"))
      u_income <- factor(predicted_user()$income_num, levels = 0:4)

      p <- p + geom_tile(data = data.frame(income_num = u_income, age_group = u_age_group),
                         aes(x = income_num, y = age_group),
                         color = "#00d1b2", fill = NA, size = 1.5, inherit.aes = FALSE)
    }

    ggplotly(p)
  })

  # 3. Scatter: Follower Dynamics
  output$growth_plot <- renderPlotly({
    d_bg <- df %>% slice_sample(n = 2000)

    p <- ggplot() +
      geom_point(data = d_bg, aes(x = following_count, y = followers_count),
                 color = "lightgrey", alpha = 0.3, size = 1) +
      theme_minimal() +
      labs(x = "Following", y = "Followers", color = "Engagement")

    if (view_state() == "all") {
      p <- p + geom_point(data = d_bg,
        aes(x = following_count, y = followers_count, color = user_engagement_score),
        alpha = 0.6) +
        scale_color_gradientn(colors = c("#405DE6", "#833AB4", "#E1306C", "#F77737", "#FCAF45"))
    } else {
      p <- p + geom_point(data = predicted_user(),
        aes(x = following_count, y = followers_count),
        color = instagram_cols["blue"], size = 6)
    }

    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
