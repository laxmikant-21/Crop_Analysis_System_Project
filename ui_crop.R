library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Crop Analysis System"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Explore Data", tabName = "explore", icon = icon("chart-bar")),
      menuItem("5-Year Forecast", tabName = "forecast", icon = icon("chart-line")),
      menuItem("Smart Recommendations", tabName = "recommend", icon = icon("lightbulb"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #home {
          background-image: url('https://images.unsplash.com/photo-1501004318641-b39e6451bec6');
          background-size: cover;
          background-position: center;
          min-height: 100vh;
          padding: 50px;
          color: white;
        }
        #home h2 {
          font-size: 40px;
          font-weight: bold;
          text-shadow: 1px 1px 3px black;
          text-align: center;
        }
        #home p {
          font-size: 22px;
          font-weight: 500;
          text-shadow: 1px 1px 2px black;
          text-align: center;
        }
        .btn-3d {
          background-color: #28a745;
          color: white;
          font-size: 16px;
          font-weight: bold;
          padding: 10px 20px;
          border-radius: 8px;
          border: none;
          transition: all 0.3s ease;
        }
        .btn-3d:hover {
          background-color: #218838;
          transform: scale(1.1);
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        }
        .btn-3d:focus {
          outline: none;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "home",
              div(id = "home",
                  h2("Welcome to Crop Analysis System 🌾"),
                  p("This application helps users explore and understand agricultural data with ease.
                    You can upload your own CSV file and analyze soil and climate conditions to get smart crop suggestions.
                    Whether you’re a farmer, student, or agriculture enthusiast, this tool makes crop planning smart, data-driven, and intuitive.")
              )
      ),
      
      tabItem(tabName = "upload",
              h3("Upload Your Dataset"),
              tags$p("📌 Please upload a CSV file in the following format (column names should match exactly):"),
              tags$ul(
                tags$li("Crop"),
                tags$li("State"),
                tags$li("Year"),
                tags$li("Season"),
                tags$li("Area (ha)"),
                tags$li("Production (tonnes)"),
                tags$li("Yield (kg/ha)"),
                tags$li("SoilType"),
                tags$li("Rainfall (mm)"),
                tags$li("Temperature (°C)")
              ),
              fileInput("file", "Upload Dataset", accept = ".csv"),
              DTOutput("preview")
      ),
      
      tabItem(tabName = "explore", 
              h3("Explore Your Dataset"), 
              actionButton("toggle_plot", "Switch to 3D View", class = "btn-3d"),
              plotlyOutput("scatterPlot"),
              helpText("Hover over the points in the plot to explore crop-related data")
      ),
      
      tabItem(tabName = "forecast",
              h3("📈 5-Year Production Forecast"),
              uiOutput("crop_selector"),
              plotlyOutput("forecast_plot")
      ),
      
      tabItem(tabName = "recommend",
              h3("🌱 Smart Crop Recommendations"),
              uiOutput("recommend_crop_selector"),
              verbatimTextOutput("recommend_summary"),
              DTOutput("recommend_table")
      )
    )
  )
)
