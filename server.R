## Sac City Budget Visualization ##
## server.R ##

source("helpers.R")

server <- function(input, output, session) {
  data1 <- reactive({
    switch(input$primary, 
                 "Departments" = SacBudget$DEPARTMENT,
                 "Fund Types" = SacBudget$FUND.TYPE,
                 "Object Classes" = SacBudget$OBJECT.CLASS)
  })
  
  classification_list = reactive({
    switch(input$primary, 
         "Departments" = as.list(c("All",departments)),
         "Fund Types" = as.list(c("All",fund_types)),
         "Object Classes" = as.list(c("All",object_classes)))
  })
  
  observe({
    updateSelectInput(session, "primarySub", choices = classification_list())
  })

  output$title1 = renderText({
    paste0(input$primary, ": ", input$primarySub)
  })
  
  output$chart1 = renderPlot({
    data_sub = input$primarySub
    budget_graph(data1(), data_sub)
  })
  
  output$total1 = renderText({
    if (input$primarySub == "All") {
      total_budget = grand_total
    } else {
      total_budget = sum(SacBudget$BUDGET.AMOUNT[which(data1() == input$primarySub &
                                                         SacBudget$EXP.REV == "Expenses" &
                                                         SacBudget$Year == 2016 &
                                                         SacBudget$DEPARTMENT != "Non-Appropriated")])
    }
    paste0("Total Budget: $", round(total_budget / 1000000, digits = 1), " million")
  })
}