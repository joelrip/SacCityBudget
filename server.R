## Sac City Budget Visualization ##
## server.R ##


source("helpers.R")

server <- function(input, output, session) {
  data1 <- reactive({
    switch(input$primary, 
                 "Departments" = "DEPARTMENT",
                 "Fund Types" = "FUND.TYPE",
                 "Object Classes" = "OBJECT.CLASS")
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
  
#  output$chart1 = renderPlot({
#    data_sub = input$primarySub
#    budget_graph(data1(), data_sub)
#  })
  
  output$table1 = renderDataTable({
    first_table = budget_table(data1(), input$primarySub, SacBudget)
#    bar_colors = first_table$ChangeCol
    first_table = first_table[,c(1,2,4)]
    datatable(first_table, rownames = FALSE, class = "compact",
              colnames = c("", "Amount in Millions", "Change"),
              options = list(searching = FALSE,
                             pageLength = 40,
                             dom = "t",
                               columnDefs = list(list(
                                 targets = 0,
                                 render = JS(
                                   "function(data, type, row, meta) {",
                                   "return type === 'display' && data.length > 35 ?",
                                   "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                   "}")
                               ))
                             )) %>%
      formatStyle("Budget1516",
                  background = styleColorBar(c(0, max(first_table$Budget1516) * 1.4), color = "steelblue", angle = -90),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("PctChange",
                  color = styleInterval(c(-.2, -.15, -.1, -.05, 0, .05, .1, .15, .2),
                                        c("#FF0000", "#F02A2A", "#E25454", "#D37E7E", "#C5A8A8",
                                          "#ACB8AC", "#8AAD8A", "#67A167", "#449644", "#228B22")),
                  fontWeight = 'bold'
      ) %>%
      formatStyle(c(1:3), fontSize = "12px") %>%
      formatCurrency("Budget1516", digits = 1) %>%
      formatPercentage("PctChange")
  })
  
  output$total1 = renderText({
    if (input$primarySub == "All") {
      total_budget = grand_total
    } else {
      domain = which(names(SacBudget) == data1())
      total_budget = sum(SacBudget$BUDGET.AMOUNT[which(SacBudget[,domain] == input$primarySub &
                                                         SacBudget$EXP.REV == "Expenses" &
                                                         SacBudget$Year == 2016 &
                                                         SacBudget$DEPARTMENT != "Non-Appropriated")])
    }
    paste0("Total Budget: $", round(total_budget / 1000000, digits = 1), " million")
  })
  
  data2 <- reactive({
    switch(input$secondary, 
           "Departments" = "DEPARTMENT",
           "Divisions" = "DIVISION",
           "Sections" = "SECTION",
           "Fund Types" = "FUND.TYPE",
           "Fund Groups" = "FUND.GROUP",
           "Fund Names" = "FUND.NAME",
           "Object Classes" = "OBJECT.CLASS",
           "Account Categories" = "ACCOUNT.CATEGORY",
           "Account Names" = "ACCOUNT.NAME")
  })
  
  classification_list2 = reactive({
    switch(input$secondary, 
           "Departments" = as.list(c("All",departments)),
           "Divisions" = as.list(c("All",divisions)),
           "Sections" = as.list(c("All", sections)),
           "Fund Types" = as.list(c("All",fund_types)),
           "Fund Groups" = as.list(c("All", fund_groups)),
           "Fund Names" = as.list(c("All", fund_names)),
           "Object Classes" = as.list(c("All",object_classes)),
           "Account Categories" = as.list(c("All", account_categories)),
           "Account Names" = as.list(c("All", account_names)))
  })
  
  observe({
    updateSelectInput(session, "secondarySub", choices = classification_list2())
  })
  
  output$title2 = renderText({
    paste0(input$secondary, ": ", input$secondarySub)
  })
  
#   if (input$primarySub == "All") {
#     output$table2 = renderText({""})
#   } else {
#     
#   }
  
  
  output$table2 = renderDataTable({
    if (input$primarySub == "All" | input$secondary == "") {
      budget_data = data.frame("Category" = c("Nothing Selected", "Nothing Selected"), "BUDGET.AMOUNT" = c(0, 0),
                               "EXP.REV" = c("Expenses", "Expenses"), "Year" = c(2015, 2016),
                               "DEPARTMENT" = c("One", "One"))
      data_selection = "Category"
      data_subselection = "Nothing Selected"
    } else if (input$primarySub != "All") {
      domain = which(names(SacBudget) == data1())
      budget_data = SacBudget[which(SacBudget[,domain] == input$primarySub),]
      data_selection = data2()
      data_subselection = input$secondarySub
    } else if (input$secondarySub != "All") {
      domain = data2()
      budget_data = SacBudget[which(SacBudget$domain == input$secondarySub),]
      data_selection = data1()
      data_subselection = input$primarySub
    }
    first_table = budget_table(data_selection, data_subselection, budget_data)
#    bar_colors = first_table$ChangeCol
    first_table = first_table[,c(1,2,4)]
    datatable(first_table, rownames = FALSE, class = "compact",
              colnames = c("", "Amount in Millions", "Change"),
              options = list(searching = FALSE,
                             pageLength = 40,
                             dom = "t",
                             columnDefs = list(list(
                               targets = 0,
                               render = JS(
                                 "function(data, type, row, meta) {",
                                 "return type === 'display' && data.length > 35 ?",
                                 "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                 "}")
                             ))
                             )) %>%
      formatStyle("Budget1516",
                  background = styleColorBar(c(0, max(first_table$Budget1516) * 1.4), color = "orange", angle = -90),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("PctChange",
                  color = styleInterval(c(-.2, -.15, -.1, -.05, 0, .05, .1, .15, .2),
                                        c("#FF0000", "#F02A2A", "#E25454", "#D37E7E", "#C5A8A8",
                                          "#ACB8AC", "#8AAD8A", "#67A167", "#449644", "#228B22")),
                  fontWeight = 'bold'
      ) %>%
      formatStyle(c(1:3), fontSize = "12px") %>%
      formatCurrency("Budget1516", digits = 2) %>%
      formatPercentage("PctChange") 
  })
  
  output$total2 = renderText({
    if (input$secondarySub == "All") {
      domain = which(names(SacBudget) == data1())
      total_budget = sum(SacBudget$BUDGET.AMOUNT[which(SacBudget[,domain] == input$primarySub &
                                                         SacBudget$EXP.REV == "Expenses" &
                                                         SacBudget$Year == 2016 &
                                                         SacBudget$DEPARTMENT != "Non-Appropriated")])
    } else {
      domain1 = which(names(SacBudget) == data1())
      budget_data1 = SacBudget[which(SacBudget[,domain1] == input$primarySub),]
      domain = which(names(budget_data1) == data2())
      budget_data = budget_data1[which(budget_data1[,domain] == input$secondarySub),]
      total_budget = sum(budget_data$BUDGET.AMOUNT[which(budget_data[,domain] == input$secondarySub &
                                                           budget_data$EXP.REV == "Expenses" &
                                                           budget_data$Year == 2016 &
                                                           budget_data$DEPARTMENT != "Non-Appropriated")])
    }
    paste0("Total Budget: $", round(total_budget / 1000000, digits = 2), " million")
  })
  
  data3 <- reactive({
    switch(input$tertiary, 
           "Departments" = "DEPARTMENT",
           "Divisions" = "DIVISION",
           "Sections" = "SECTION",
           "Fund Types" = "FUND.TYPE",
           "Fund Groups" = "FUND.GROUP",
           "Fund Names" = "FUND.NAME",
           "Object Classes" = "OBJECT.CLASS",
           "Account Categories" = "ACCOUNT.CATEGORY",
           "Account Names" = "ACCOUNT.NAME")
  })
  
  output$title3 = renderText({
    input$tertiary
  })
  
  output$table3 = renderDataTable({
    if (input$secondarySub == "All" | input$tertiary == "" | input$primarySub == "All") {
      budget_data = data.frame("Category" = c("Nothing Selected", "Nothing Selected"), "BUDGET.AMOUNT" = c(0, 0),
                               "EXP.REV" = c("Expenses", "Expenses"), "Year" = c(2015, 2016),
                               "DEPARTMENT" = c("One", "One"))
      data_selection = "Category"
      data_subselection = "Nothing Selected"
    } else if (input$secondarySub != "All" & input$tertiary != "") {
      domain1 = which(names(SacBudget) == data1())
      budget_data1 = SacBudget[which(SacBudget[,domain1] == input$primarySub),]
      domain = which(names(budget_data1) == data2())
      budget_data = budget_data1[which(budget_data1[,domain] == input$secondarySub),]
      data_selection = data3()
      data_subselection = "All"
#     } else if (input$secondarySub != "All") {
#       domain = data2()
#       budget_data = SacBudget[which(SacBudget$domain == input$secondarySub),]
#       data_selection = data1()
#       data_subselection = input$primarySub
    }
    first_table = budget_table(data_selection, data_subselection, budget_data)
    #    bar_colors = first_table$ChangeCol
    first_table = first_table[,c(1,2,4)]
    datatable(first_table, rownames = FALSE, class = "compact",
              colnames = c("", "Amount in Millions", "Change"),
              options = list(searching = FALSE,
                             pageLength = 40,
                             dom = "t",
                             columnDefs = list(list(
                               targets = 0,
                               render = JS(
                                 "function(data, type, row, meta) {",
                                 "return type === 'display' && data.length > 35 ?",
                                 "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                 "}")
                             ))
              )) %>%
      formatStyle("Budget1516",
                  background = styleColorBar(c(0, max(first_table$Budget1516) * 1.4), color = "orangered", angle = -90),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center"
      ) %>%
      formatStyle("PctChange",
                  color = styleInterval(c(-.2, -.15, -.1, -.05, 0, .05, .1, .15, .2),
                                        c("#FF0000", "#F02A2A", "#E25454", "#D37E7E", "#C5A8A8",
                                          "#ACB8AC", "#8AAD8A", "#67A167", "#449644", "#228B22")),
                  fontWeight = 'bold'
      ) %>%
      formatStyle(c(1:3), fontSize = "12px") %>%
      formatCurrency("Budget1516", digits = 2) %>%
      formatPercentage("PctChange") 
  })
  
  output$total3 = renderText({
    if (input$secondarySub != "All" & input$tertiary != "") {
      domain1 = which(names(SacBudget) == data1())
      budget_data1 = SacBudget[which(SacBudget[,domain1] == input$primarySub),]
      domain = which(names(budget_data1) == data2())
      budget_data = budget_data1[which(budget_data1[,domain] == input$secondarySub),]
#       data_selection = data3()
#       domain = which(names(SacBudget) == data1())
      total_budget = sum(budget_data$BUDGET.AMOUNT[which(budget_data[,domain] == input$secondarySub &
                                                         budget_data$EXP.REV == "Expenses" &
                                                         budget_data$Year == 2016 &
                                                         budget_data$DEPARTMENT != "Non-Appropriated")])
    } else {
      total_budget = 0
    }
    paste0("Total Budget: $", round(total_budget / 1000000, digits = 2), " million")
  })
  
}