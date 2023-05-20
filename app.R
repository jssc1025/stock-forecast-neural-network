# Import necessary libraries
library(reticulate)
library(keras)
library(shiny)
library(zoo)
library(dplyr)
library(lubridate)
library(tensorflow)
library(tidyverse)
library(scales)
library(ggplot2)
library(gridExtra)
library(shinydashboard)
library(DT)

# Load your dataset
df <- read.csv("StockPrices.csv")

names(df)[names(df) == "X"] <- "Date"

# Define the split date as the middle point of your dataset
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

split_date <- df$Date[floor(nrow(df)/2)]

# Create historical and target datasets based on the split_date
historical_data <- df %>% filter(Date < split_date)
target_data <- df %>% filter(Date >= split_date)


scale_data <- function(data) {
  center <- sapply(data, mean)
  scale <- sapply(data, sd)
  if (any(scale == 0)) {
    stop("The standard deviation of one or more columns is zero.")
  }
  scaled_data <- scale(data, center = center, scale = scale)
  return(list(scaled_data = as.data.frame(scaled_data), center = center, scale = scale)) # change to as.data.frame
}

historical_data_scaled_list <- scale_data(historical_data[-1])  # Exclude 'Date' column
target_data_scaled_list <- scale_data(target_data[-1])  # Exclude 'Date' column

historical_data_scaled <- historical_data_scaled_list$scaled_data
target_data_scaled <- target_data_scaled_list$scaled_data

# Function to unscale the data
unscale_data <- function(scaled_data, center, scale) {
  unscaled_data <- (scaled_data * scale) + center
  return(unscaled_data)
}

# Define window size
window_size <- 10

create_sequences <- function(data, window_size) {
  n <- nrow(data)
  if (n <= window_size) {
    stop("window_size is greater or equal to the number of rows in data. Reduce window_size.")
  }
  
  result <- array(0, dim = c(n - window_size + 1, window_size, 1))  # The last dimension is 1
  
  for (start in 1:(n - window_size + 1)) {
    end <- start + window_size - 1
    result[start,,] <- as.matrix(data[start:end, ])
  }
  
  return(result)
}

# Initialize a list to store the models and histories
models <- list()
histories <- list()
predictions <- list()
forecast_days = 30


# Loop over the columns
for (col in colnames(historical_data_scaled)) {
  # Extract the sequences for the current column
  historical_data_seq <- create_sequences(data.frame(historical_data_scaled[[col]]), window_size)
  target_data_seq <- create_sequences(data.frame(target_data_scaled[[col]]), window_size)
  target_data_seq <- target_data_seq[, window_size, ]
  
  # Set the regularization parameter
  lambda <- 0.01
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = 256, return_sequences = TRUE, input_shape = c(window_size, 1), unroll=TRUE, dropout = 0.2, recurrent_dropout = 0.2, kernel_regularizer = regularizer_l2(lambda)) %>%
    layer_lstm(units = 128, dropout = 0.2, recurrent_dropout = 0.2, unroll=TRUE) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 10, activation = 'relu') %>%
    layer_dense(units = 1, name = "output")
  
  
  optimizer <- optimizer_rmsprop(learning_rate = 0.01)
  
  model %>% compile(
    loss = list(output = "mean_squared_error"),
    optimizer = optimizer,
    metrics = list(output = 'mean_absolute_error')
  )
  
  # Train the model
  history <- model %>% fit(
    historical_data_seq,
    target_data_seq,
    epochs = 10,
    batch_size = 64,
    validation_split = 0.2
  )
  
  # Store the history in the list
  histories[[col]] <- history
  
  # Store the model in the list
  models[[col]] <- model
  
  # Convert target_data_scaled to a list
  target_data_scaled <- as.list(target_data_scaled)
  
  # Forecast the next time step and store the result
  for(i in 1:forecast_days) {
    recent_data <- tail(target_data_scaled[[col]], window_size)
    recent_data_seq <- array(0, dim = c(1, window_size, 1))
    recent_data_seq[1,,] <- as.matrix(recent_data)
    pred <- model %>% predict(recent_data_seq, verbose = 0)
    
    # Add the prediction to the target_data_scaled for rolling forecast
    target_data_scaled[[col]] <- c(target_data_scaled[[col]], as.vector(pred))
    
    # Rebuild sequences with new data
    target_data_seq <- create_sequences(data.frame(target_data_scaled[[col]]), window_size)
    target_data_seq <- target_data_seq[, window_size, ]
    
    # Reshape the predictions
    pred <- matrix(pred, nrow = 1, ncol = 1)
    
    # Unscale the predictions
    pred <- unscale_data(pred, target_data_scaled_list$center[[col]], target_data_scaled_list$scale[[col]])
    if (is.null(predictions[[col]])) {
      predictions[[col]] <- pred
    } else {
      predictions[[col]] <- rbind(predictions[[col]], pred)
    }
    
    # Retrain the model with updated data for rolling forecast
    historical_data_seq <- create_sequences(data.frame(historical_data_scaled[[col]]), window_size)
    
    history <- model %>% fit(
      historical_data_seq,
      target_data_seq,
      epochs = 1,
      batch_size = 64,
      validation_split = 0.2
    )
    
    # Store the history in the list
    histories[[col]] <- history
  }
}

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Stock Price Forecasting"),
  dashboardSidebar(
    menuItem("Table", tabName = "table", icon = icon("table")),
    menuItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
    menuItem("Loss", tabName = "loss", icon = icon("chart-bar")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table",
              DT::dataTableOutput("forecast_table")
      ),
      tabItem(tabName = "forecast",
              uiOutput("forecast_plots_ui")
      ),
      tabItem(tabName = "loss",
              uiOutput("loss_plots_ui")
      ),
      tabItem(tabName = "about",
              uiOutput("aboutPage")
      )
    )
  )
)


# Shiny Server
server <- function(input, output) {
  
  
  output$aboutPage <- renderUI({
    includeMarkdown("about.Rmd")  # Replace with the path to your about.Rmd file if it's located in a different folder
  })
  
  output$forecast_table <- DT::renderDataTable({
    # Build your forecast table here from predictions list
    forecast_table <- do.call(cbind, round(as.data.frame(predictions), digits = 2))
    colnames(forecast_table) <- colnames(historical_data_scaled)
    forecast_table
  })
  
  output$forecast_plots_ui <- renderUI({
    plot_output_list <- lapply(1:length(predictions), function(i) {
      plotOutput(outputId = paste("forecast_plot_", i))
    })
    
    do.call(tagList, plot_output_list)
  })
  
  output$loss_plots_ui <- renderUI({
    plot_output_list <- lapply(1:(2 * length(histories)), function(i) {
      plotOutput(outputId = paste("loss_plot_", i))
    })
    
    do.call(tagList, plot_output_list)
  })
  
  for (i in 1:length(predictions)) {
    local({
      my_i <- i
      output[[paste("forecast_plot_", my_i)]] <- renderPlot({
        plot(predictions[[my_i]], type = "l", main = colnames(historical_data_scaled)[my_i], ylab = "Forecast", xlab = "Time")
      })
    })
  }
  
  for (i in 1:length(histories)) {
    local({
      my_i <- i
      output[[paste("loss_plot_", 2*my_i-1)]] <- renderPlot({
        mae_data <- data.frame(
          Epochs = 1:length(histories[[my_i]]$metrics$mean_absolute_error),
          MAE = histories[[my_i]]$metrics$mean_absolute_error,
          Val_MAE = histories[[my_i]]$metrics$val_mean_absolute_error
        )
        
        ggplot(mae_data, aes(x = Epochs)) +
          geom_point(aes(y = MAE), color = "black") +
          geom_point(aes(y = Val_MAE), color = "red") +
          labs(title = paste(colnames(historical_data_scaled)[my_i], "- Mean Absolute Error"), y = "Mean Absolute Error", x = "Epochs") +
          theme_minimal() +
          theme(legend.position = "topright") +
          scale_x_continuous(breaks = seq(1, length(histories[[my_i]]$metrics$mean_absolute_error), by = 1)) +
          scale_y_continuous(limits = c(min(mae_data$MAE, mae_data$Val_MAE), max(mae_data$MAE, mae_data$Val_MAE)))
      })
      
      output[[paste("loss_plot_", 2*my_i)]] <- renderPlot({
        loss_data <- data.frame(
          Epochs = 1:length(histories[[my_i]]$metrics$loss),
          Loss = histories[[my_i]]$metrics$loss,
          Val_Loss = histories[[my_i]]$metrics$val_loss
        )
        
        ggplot(loss_data, aes(x = Epochs)) +
          geom_point(aes(y = Loss), color = "black") +
          geom_point(aes(y = Val_Loss), color = "red") +
          labs(title = paste(colnames(historical_data_scaled)[my_i], "- Loss"), y = "Loss", x = "Epochs") +
          theme_minimal() +
          theme(legend.position = "topright") +
          scale_x_continuous(breaks = seq(1, length(histories[[my_i]]$metrics$loss), by = 1)) +
          scale_y_continuous(limits = c(min(loss_data$Loss, loss_data$Val_Loss), max(loss_data$Loss, loss_data$Val_Loss)))
      })
    })
  }
}
# Run Shiny App
shinyApp(ui = ui, server = server)
