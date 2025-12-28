library(shiny)
library(dplyr)
library(ggplot2)

fires_daily <- readRDS("fires_daily.rds")

ui <- fluidPage(
    titlePanel("Top Days by Burned Area"),
    sidebarLayout(
        sidebarPanel(
            # 1. Slider for Burned Area
            sliderInput("dba_range", "Burned Area range (DBA):", 
                        min = 0, max = 1000, value = c(0, 1000), step = 0.01),
            
            # 2. Top N
            textInput("top_n", "Number of top days:", value = "5"),
            
            hr(),
            
            # Selection for DNRF Class
            radioButtons(
                inputId = "dnrf_class_select",
                label = "Select DNRF Class (Fires per day):",
                choices = sort(unique(fires_daily$DNRF_class)),
                selected = sort(unique(fires_daily$DNRF_class))[1]
            ),
            
            # Selection for FWI Class
            selectInput(
                inputId = "fwi_class_select",
                label = "Select FWI Class:",
                choices = c("All", sort(unique(fires_daily$FWI_class))),
                selected = "All"
            ),
            
            # Checkboxes for Distrito
            checkboxGroupInput(
                inputId = "distrito_check",
                label = "Filter by Distrito:",
                choices = sort(unique(fires_daily$Distrito)),
                selected = unique(fires_daily$Distrito)
            )
        ),
        
        mainPanel(
            tableOutput("top_days"),
            plotOutput("barplot")
        )
    )
)

server <- function(input, output) {
    
    top_data <- reactive({
        # 1. Ensure inputs are available before running the rest of the code
        req(input$dba_range, input$dnrf_class_select, input$distrito_check)
        
        # Validate Top N input
        n <- as.numeric(input$top_n)
        if (is.na(n) || n < 1) n <- 1
        
        data <- fires_daily %>%
            filter(
                DBA >= input$dba_range[1],
                DBA <= input$dba_range[2],
                DNRF_class == (input$dnrf_class_select),
                Distrito %in% input$distrito_check
            )
        
        # 2. Fix the "if" error by checking if input$fwi_class_select exists
        if (!is.null(input$fwi_class_select)) {
            if (input$fwi_class_select != "All") {
                data <- data %>% filter(FWI_class == input$fwi_class_select)
            }
        }
        
        data %>%
            arrange(desc(DBA)) %>%
            slice_head(n = n)
    })
    
    output$top_days <- renderTable({
        req(top_data()) # Only render if data exists
        top_data()
    })
    
    output$barplot <- renderPlot({
        # 3. req() ensures the plot doesn't try to draw with 0 rows
        req(top_data(), nrow(top_data()) > 0) 
        
        ggplot(top_data(), aes(x = reorder(as.character(Date), DBA), y = DBA)) +
            geom_col(fill = "firebrick") +
            coord_flip() +
            labs(x = "Date", y = "Burned Area", title = "Top Days") +
            theme_minimal()
    })
}

shinyApp(ui, server)