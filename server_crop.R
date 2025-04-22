library(shiny)
library(DT)
library(plotly)
library(readr)
library(dplyr)
library(forecast)
library(tidyr)

server <- function(input, output, session) {
  
  uploaded_data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  output$preview <- renderDT({
    req(uploaded_data())
    datatable(head(uploaded_data(), 10), options = list(scrollX = TRUE))
  })
  
  output$scatterPlot <- renderPlotly({
    df <- uploaded_data()
    req(df)
    
    df[is.na(df)] <- "NA"
    
    df$hover_text <- paste0(
      "Crop: ", df$Crop, "<br>",
      "Year: ", df$Year, "<br>",
      "State: ", df$State, "<br>",
      "Season: ", df$Season, "<br>",
      "Temperature: ", df$`Temperature (°C)`, "<br>",
      "Rainfall: ", df$`Rainfall (mm)`, "<br>",
      "Production: ", df$`Production (tonnes)`, "<br>",
      "Yield: ", df$`Yield (kg/ha)`
    )
    
    if (input$toggle_plot %% 2 == 1) {
      plot_ly(
        data = df,
        x = ~Year,
        y = ~`Temperature (°C)`,
        z = ~`Rainfall (mm)`,
        type = 'scatter3d',
        mode = 'markers',
        color = ~Crop,
        text = ~hover_text,
        hoverinfo = 'text',
        marker = list(size = 10, opacity = 0.8)
      ) %>%
        layout(title = "3D Scatter Plot: Full Info on Hover",
               scene = list(
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Temperature (°C)"),
                 zaxis = list(title = "Rainfall (mm)")
               ))
    } else {
      plot_ly(
        data = df,
        x = ~Year,
        y = ~`Temperature (°C)`,
        type = 'scatter',
        mode = 'markers',
        color = ~Crop,
        text = ~hover_text,
        hoverinfo = 'text',
        marker = list(size = 10, opacity = 0.8)
      ) %>%
        layout(title = "2D Scatter Plot: Full Info on Hover",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Temperature (°C)"))
    }
  })
  
  output$crop_selector <- renderUI({
    df <- uploaded_data()
    req(df)
    crops <- unique(df$Crop)
    selectInput("selected_crop", "Select Crop for Forecasting", choices = crops)
  })
  
  output$forecast_plot <- renderPlotly({
    req(input$selected_crop)
    df <- uploaded_data()
    
    df <- df %>%
      filter(!is.na(`Production (tonnes)`), !is.na(Year), !is.na(Crop)) %>%
      group_by(Crop, Year) %>%
      summarise(Total_Production = sum(`Production (tonnes)`), .groups = 'drop')
    
    crop_data <- df %>% filter(Crop == input$selected_crop)
    
    if (nrow(crop_data) < 3) {
      return(plot_ly() %>% layout(title = "Not enough data to forecast this crop"))
    }
    
    ts_data <- ts(crop_data$Total_Production, start = min(crop_data$Year), frequency = 1)
    fit <- auto.arima(ts_data)
    forecasted <- forecast(fit, h = 5)
    
    forecast_df <- data.frame(
      Year = seq(max(crop_data$Year)+1, max(crop_data$Year)+5),
      Forecast = as.numeric(forecasted$mean)
    )
    
    combined <- bind_rows(
      crop_data %>% select(Year, Total_Production) %>% rename(Value = Total_Production),
      forecast_df %>% rename(Value = Forecast)
    )
    
    plot_ly(combined, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines+markers',
            name = paste("Forecast for", input$selected_crop),
            line = list(color = 'green', width = 3)) %>%
      layout(title = paste("5-Year Production Forecast for", input$selected_crop),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Production (tonnes)"))
  })
  
  output$recommend_crop_selector <- renderUI({
    df <- uploaded_data()
    req(df)
    crops <- unique(df$Crop)
    selectInput("recommend_crop", "Select a Crop to Get Recommendations", choices = crops)
  })
  
  output$recommend_summary <- renderPrint({
    req(input$recommend_crop)
    df <- uploaded_data()
    
    crop_df <- df %>% filter(Crop == input$recommend_crop)
    
    avg_temp <- mean(crop_df$`Temperature (°C)`, na.rm = TRUE)
    avg_rain <- mean(crop_df$`Rainfall (mm)`, na.rm = TRUE)
    common_soil <- crop_df %>%
      filter(!is.na(SoilType)) %>%
      count(SoilType, sort = TRUE) %>%
      slice(1) %>%
      pull(SoilType)
    
    cat(paste0("📌 Smart Recommendations for ", input$recommend_crop, ":\n\n"))
    cat(paste0("- ✅ Recommended Avg Temperature: ", round(avg_temp, 2), " °C\n"))
    cat(paste0("- ✅ Recommended Avg Rainfall: ", round(avg_rain, 2), " mm\n"))
    cat(paste0("- ✅ Most Suitable Soil Type: ", common_soil, "\n"))
    cat("\n👉 Try to grow this crop in regions having similar climate and soil type.")
  })
  
  output$recommend_table <- renderDT({
    req(input$recommend_crop)
    df <- uploaded_data()
    crop_df <- df %>% filter(Crop == input$recommend_crop)
    datatable(crop_df, options = list(scrollX = TRUE, pageLength = 5))
  })
}
