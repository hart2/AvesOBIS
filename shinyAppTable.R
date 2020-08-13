#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# before removing duplicates, land_buffer, and data without sampling effort

# upload datasetsForOBIS.xlsx from local repository

# set working directory to Shiny folder in github
tbl <- datasetsForOBIS %>% 
    select(hemisphere, name, numberRecords, makeupPerc) %>% 
    arrange(makeupPerc)
colnames(tbl) <- c("Hemisphere","Dataset Name","Number of Records","Makeup of Aves OBIS (%)")

library(shiny)

# Define UI for application that creates table

ui <- fluidPage(
    title = "Table 1",
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                'input.dataset === "Datasets"',
                checkboxGroupInput("show_vars", "Columns to show:",
                                   names(tbl), selected = names(tbl))
            ),
        ),
        mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Datasets", DT::dataTableOutput("mytable1"))
            )
        )
    )
)

server <- function(input, output) {
    
    # choose columns to display
    x = tbl[sample(nrow(tbl), 107), ]
    output$mytable1 <- DT::renderDataTable({
        DT::datatable(x[, input$show_vars, drop = FALSE])
    })
    
}

shinyApp(ui, server)
