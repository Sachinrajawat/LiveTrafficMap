install.packages("webshot2")
install.packages("googletraffic")
install.packages("raster")
install.packages("ggplot2")
install.packages("shiny")
install.packages("dplyr")
library(webshot2)
# Load necessary libraries
library(shiny)
library(googletraffic)
library(ggplot2)
library(dplyr)
library(raster)
install.packages("fastmap")
library(fastmap)
# Set your Google API key
google_key <- "AIzaSyB41DRUbKWJHPxaFjMAwdrzWzbVKartNGg"

# Define UI for the shiny app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-panel {
        text-align: center;
      }
      .traffic-plot {
        display: flex;
        justify-content: center;
        align-items: center;
      }
    "))
  ),
  
  # Center the title
  titlePanel(
    div("Real-Time Traffic Map", class = "title-panel")
  ),
  
  # Center the plot
  div(plotOutput("trafficPlot"), class = "traffic-plot")
)


# Define server logic
server <- function(input, output, session) {  # Include session in server function
  output$trafficPlot <- renderPlot({
    invalidateLater(60000, session)  # Re-run this function every 5 minutes (300000 ms)
    
    # Fetch traffic data
    r <- gt_make_raster(location   = c(12.936224001737958, 77.60868414530202), 
                        height     = 2000,
                        width      = 2000,
                        zoom       = 20,
                        google_key = google_key)
    
    # Convert raster to data frame for plotting
    r_df <- rasterToPoints(r, spatial = TRUE) %>% as.data.frame()
    names(r_df) <- c("value", "x", "y")
    
    # Create the traffic map plot
    ggplot() +
      geom_raster(data = r_df, aes(x = x, y = y, fill = as.factor(value))) +
      labs(fill = "Traffic\nLevel") +
      scale_fill_manual(values = c("green2", "orange", "red", "#660000")) +
      coord_quickmap() +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", color = "white"))
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)

