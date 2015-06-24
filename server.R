## Sac City Budget Visualization ##
## server.R ##

source("helpers.R")

server <- function(input, output, session) {

  classification_list = reactive({
    switch(input$primary, 
         "Departments" = as.list(c("All",departments)),
         "Fund Types" = as.list(c("All",fund_types)),
         "Object Classes" = as.list(c("All",object_classes)))
  })
  
  observe({
    updateSelectInput(session, "primarySub", choices = classification_list())
  })

  
  output$title1 = renderText({input$primary})
  
  output$chart1 = renderPlot({
    data <- switch(input$primary, 
                   "Departments" = SacBudget$DEPARTMENT,
                   "Fund Types" = SacBudget$FUND.TYPE,
                   "Object Classes" = SacBudget$OBJECT.CLASS)
    budget_graph(data)
  })
  
  output$total1 = renderText({paste0("Total Budget: $", round(grand_total / 1000000, digits = 1), " million")})
}