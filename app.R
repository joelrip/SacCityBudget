### Sac City Budget Visualization ###
### app.R ###

library(shinydashboard)
library(shiny)
library(DT)
library(dplyr)

source("helpers2.R")

ui = dashboardPage(
  dashboardHeader(
    title = "City of Sacramento 2018 Budget Explorer",
    titleWidth = 475
  ),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
                      background-color: #ecf0f5;
                      }
                      "))
      ),
    column(width = 4,
           box(
             width = NULL, status = "primary",
             
             selectInput("primary", label = "What do you want to break the budget down by first?",
                         choices = list("Departments", "Fund Types",
                                        "Account Types"), selected = "Departments"),
             
             selectInput("primarySub", label = "To explore in more detail, select a specific one:",
                         choices = "All")
           ),
           box(
             title = textOutput("title1"), width = NULL, status = "primary",
             #             plotOutput("chart1"),
             dataTableOutput("table1")
           ),
           box(
             width = NULL, background = "light-blue",
             h4(textOutput("total1"), align = "center")
           )
    ),
    
    conditionalPanel(
      condition = "input.primarySub == 'All'",
      column(width = 8,
             HTML("
                  <h1>What is this?</h1>
                  <h4>This site lets you explore the details of the <a href='http://www.cityofsacramento.org/'>
                  City of Sacramento's</a> adopted <a href='http://www.cityofsacramento.org/Finance/Budget/'>
                  2018 budget</a>.</h4>
                  <h3>Select</h3>
                  <h4>
                  In the first column, you can begin breaking down the budget by:
                  <br><br>
                  <ul>
                  <li><b>Departments:</b> These are the major divisions of city government, such as Police, Fire,
                  and Utilities.
                  <li><b>Fund Types:</b> These are the different fund sources that are used to pay for city
                  expenditures, such as the General Fund, Enterprise Funds, and Internal Service Funds.
                  <li><b>Account Types:</b> These are the various categories of expenditure for the city, including
                  Employee Services, Other Services and Supplies, and Debt Service.
                  </ul></h4>
                  <h3>Sort</h3>
                  <h4>
                  Clicking the column headers in the table allows you to sort the table alphabetically, by budget
                  amount, and by change from the previous year.</h4>
                  <h3>Explore</h3>
                  <h4>
                  To explore part of the budget in more detail, just select a specific department, fund type,
                  or account type in the appropriate dropdown menu, and a second column will appear right here, with
                  information on the item you selected.
                  <br><br>
                  <i>Note: The city renamed and reorganized several divisions within its departments between
                  fiscal year 2017 and fiscal year 2018. If a division is new or has a different name in 2018,
                  it will not show a percentage change from the previous year.</i></h4>
                  ")
             )
             ),
    
    column(width = 4,
           conditionalPanel(
             condition = "input.primarySub != 'All'",
             box(
               width = NULL, status = "warning",
               selectInput("secondary", label = "Break down your previous selection further by:",
                           choices = list("", "Departments", "...Divisions", "......Sections", "Fund Types", "...Fund Groups",
                                          "......Fund Names", "Account Types", "...Account Categories", "......Account Names"),
                           selected = "Fund Types"),
               
               selectInput("secondarySub", label = "To explore in more detail, select a specific one:",
                           choices = "All")
             ),
             box(
               title = textOutput("title2"), width = NULL, status = "warning",
               dataTableOutput("table2")
             ),
             box(
               width = NULL, background = "yellow",
               h4(textOutput("total2"), align = "center")
             )
           )
    ),
    
    column(width = 4,
           conditionalPanel(
             condition = "input.primarySub != 'All' & input.secondarySub == 'All'",
             HTML("
                  <h1>Now what?</h1>
                  <h4>Now that you've made a selection, you can break that piece of the budget down into finer
                  levels of detail. Your choices include:
                  <br><br>
                  <ul>
                  <li><b>Department/Division/Section:</b> For example, Police <em>department</em>, Investigations
                  <em>division</em>, Forensic Identification <em>section</em>.
                  <li><b>Fund Type/Fund Group/Fund Name:</b> For example, Other Governmental <em>fund type</em>,
                  Debt Service <em>fund group</em>, Kings-Arco Arena Acquisition <em>fund name</em>.
                  <li><b>Account Type/Account Category/Account Name:</b> For example, Employee Services <em>
                  account type</em>, Employee Benefits <em>account category</em>, PERS Retirement <em>
                  account name</em>.
                  </ul>
                  If you select a specific item in the second column, a third column will appear here, allowing you
                  refine your selection even further. With practice, you can use this tool to make all kinds of interesting
                  comparisons. For example:
                  <br><br>
                  <em>To compare the total amount paid for hours worked in each City Councilmember's office, select
                  Departments: Mayor/Council -> Account Names: Regular Hours -> Sections.
                  <br><br>
                  To see how much the city spends on interest payments on outstanding debt, select Account Types:
                  City Debt Service -> Account Names: Interest Payments -> Fund Names.
                  <br><br>
                  To compare PERS retirement costs among city departments, select Account Types: Employee Services
                  -> Account Names: PERS Retirement -> Departments.</em>
                  </h4>
                  ")
             ),
           conditionalPanel(
             condition = "input.primarySub != 'All' & input.secondarySub != 'All'",
             box(
               width = NULL, status = "success",
               selectInput("tertiary", label = "Break down your previous selection further by:",
                           choices = list("", "Departments", "...Divisions", "......Sections", "Fund Types", "...Fund Groups",
                                          "......Fund Names", "Account Types", "...Account Categories", "......Account Names"),
                           selected = "Account Types")
             ),
             box(
               title = textOutput("title3"), width = NULL, status = "success",
               dataTableOutput("table3")
             ),
             box(
               width = NULL, background = "green",
               h4(textOutput("total3"), align = "center")
             )
           )
        )
    )
)

server = function(input, output, session) {
  data1 <- reactive({
    switch(input$primary, 
           "Departments" = "OPERATING_UNIT_DESCRIPTION",
           "Fund Types" = "FUND_GROUP",
           "Account Types" = "OBJECT_CLASS")
  })
  
  classification_list = reactive({
    switch(input$primary, 
           "Departments" = as.list(c("All",departments)),
           "Fund Types" = as.list(c("All",fund_types)),
           "Account Types" = as.list(c("All",object_classes)))
  })
  
  observe({
    updateSelectInput(session, "primarySub", choices = classification_list())
  })
  
  output$title1 = renderText({
    paste0(input$primary, ": ", input$primarySub)
  })
  
  output$table1 = renderDataTable({
    first_table = budget_table(data1(), input$primarySub, SacBudget)
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
      formatStyle("Budget18",
                  background=color_from_middle(first_table$Budget18 * 1.4,'red','steelblue', abs(min(first_table$Budget18, 0))/(abs(max(first_table$Budget18, 0) - min(first_table$Budget18, 0))) * 100),
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
      formatCurrency("Budget18") %>%
      formatPercentage("PctChange")
  })
  
  output$total1 = renderText({
    if (input$primarySub == "All") {
      total_budget = grand_total
    } else {
      domain = which(names(SacBudget) == data1())
      total_budget = sum(SacBudget$BUDGET_AMOUNT[which(SacBudget[,domain] == input$primarySub &
                                                         SacBudget$EXPREV == "Expenses" &
                                                         SacBudget$YEAR == "FY18" &
                                                         SacBudget$OPERATING_UNIT_DESCRIPTION != "Non-Appropriated")])
    }
    paste0("Total Budget: $", round(total_budget / 1000000, digits = 1), " million")
  })
  
  data2 <- reactive({
    switch(input$secondary, 
           "Departments" = "OPERATING_UNIT_DESCRIPTION",
           "...Divisions" = "DEPARTMENT_DIVISION",
           "......Sections" = "DEPARTMENT_DESCRIPTION",
           "Fund Types" = "FUND_GROUP",
           "...Fund Groups" = "FUND_SUMMARY",
           "......Fund Names" = "FUND_DESCRIPTION",
           "Account Types" = "OBJECT_CLASS",
           "...Account Categories" = "ACCOUNT_CATEGORY",
           "......Account Names" = "ACCOUNT_DESCRIPTION")
  })
  
  observe({
    updateSelectInput(session, "secondarySub", choices = class_list(data2(), data1(), input$primarySub, SacBudget))
  })
  
  output$title2 = renderText({
    paste0(input$secondary, ": ", input$secondarySub)
  })
  
  output$table2 = renderDataTable({
    req(input$primarySub != 'All')
   
    if (input$primarySub == "All" | input$secondary == "") {
      budget_data = data.frame("Category" = c("Nothing Selected", "Nothing Selected"), "BUDGET_AMOUNT" = c(0, 0),
                               "EXPREV" = c("Expenses", "Expenses"), "YEAR" = c("FY17", "FY18"),
                               "OPERATING_UNIT_DESCRIPTION" = c("One", "One"))
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
      formatStyle("Budget18",
                  background=color_from_middle(first_table$Budget18 * 1.4,'red','orange', abs(min(first_table$Budget18, 0))/(abs(max(first_table$Budget18, 0) - min(first_table$Budget18, 0))) * 100),
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
      formatCurrency("Budget18") %>%
      formatPercentage("PctChange") 
  })
  
  output$total2 = renderText({
    if (input$secondarySub == "All") {
      domain = which(names(SacBudget) == data1())
      total_budget = sum(SacBudget$BUDGET_AMOUNT[which(SacBudget[,domain] == input$primarySub &
                                                         SacBudget$EXPREV == "Expenses" &
                                                         SacBudget$YEAR == "FY18" &
                                                         SacBudget$OPERATING_UNIT_DESCRIPTION != "Non-Appropriated")])
    } else {
      domain1 = which(names(SacBudget) == data1())
      budget_data1 = SacBudget[which(SacBudget[,domain1] == input$primarySub),]
      domain = which(names(budget_data1) == data2())
      budget_data = budget_data1[which(budget_data1[,domain] == input$secondarySub),]
      total_budget = sum(budget_data$BUDGET_AMOUNT[which(budget_data[,domain] == input$secondarySub &
                                                           budget_data$EXPREV == "Expenses" &
                                                           budget_data$YEAR == "FY18" &
                                                           budget_data$OPERATING_UNIT_DESCRIPTION != "Non-Appropriated")])
    }
    paste0("Total Budget: $", round(total_budget / 1000000, digits = 2), " million")
  })
  
  data3 <- reactive({
    switch(input$tertiary, 
           "Departments" = "OPERATING_UNIT_DESCRIPTION",
           "...Divisions" = "DEPARTMENT_DIVISION",
           "......Sections" = "DEPARTMENT_DESCRIPTION",
           "Fund Types" = "FUND_GROUP",
           "...Fund Groups" = "FUND_SUMMARY",
           "......Fund Names" = "FUND_DESCRIPTION",
           "Account Types" = "OBJECT_CLASS",
           "...Account Categories" = "ACCOUNT_CATEGORY",
           "......Account Names" = "ACCOUNT_DESCRIPTION")
  })
  
  output$title3 = renderText({
    input$tertiary
  })
  
  output$table3 = renderDataTable({
    req(input$primarySub != 'All' & input$secondarySub != 'All')
    
    if (input$tertiary == "") {
      budget_data = data.frame("Category" = c("Nothing Selected", "Nothing Selected"), "BUDGET_AMOUNT" = c(0, 0),
                               "EXPREV" = c("Expenses", "Expenses"), "Year" = c("FY17", "FY18"),
                               "OPERATING_UNIT_DESCRIPTION" = c("One", "One"))
      data_selection = "Category"
      data_subselection = "Nothing Selected"
    } else {
      domain1 = which(names(SacBudget) == data1())
      budget_data1 = SacBudget[which(SacBudget[,domain1] == input$primarySub),]
      domain = which(names(budget_data1) == data2())
      budget_data = budget_data1[which(budget_data1[,domain] == input$secondarySub),]
      data_selection = data3()
      data_subselection = "All"
    }
    first_table = budget_table(data_selection, data_subselection, budget_data)
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
      formatStyle("Budget18",
                  background=color_from_middle(first_table$Budget18 * 1.4,'red','seagreen', abs(min(first_table$Budget18, 0))/(abs(max(first_table$Budget18, 0) - min(first_table$Budget18, 0))) * 100),
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
      formatCurrency("Budget18") %>%
      formatPercentage("PctChange") 
  })
  
  output$total3 = renderText({
    if (input$secondarySub != "All" & input$tertiary != "") {
      domain1 = which(names(SacBudget) == data1())
      budget_data1 = SacBudget[which(SacBudget[,domain1] == input$primarySub),]
      domain = which(names(budget_data1) == data2())
      budget_data = budget_data1[which(budget_data1[,domain] == input$secondarySub),]
      total_budget = sum(budget_data$BUDGET_AMOUNT[which(budget_data[,domain] == input$secondarySub &
                                                           budget_data$EXPREV == "Expenses" &
                                                           budget_data$YEAR == "FY18" &
                                                           budget_data$OPERATING_UNIT_DESCRIPTION != "Non-Appropriated")])
    } else {
      total_budget = 0
    }
    paste0("Total Budget: $", round(total_budget / 1000000, digits = 2), " million")
  })
  
}

shinyApp(ui = ui, server = server)
