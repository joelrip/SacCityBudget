## Sac City Budget Visualization ##
## ui.R ##
library(shinydashboard)
library(shiny)
library(DT)


dashboardPage(
  dashboardHeader(
    title = "City of Sacramento 2015-16 Budget Explorer",
    titleWidth = 475
  ),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    column(width = 4,
           box(
             width = NULL, status = "primary",
             
             selectInput("primary", label = "What do you want to break the budget down by first?",
                         choices = list("Departments", "Fund Types",
                                        "Object Classes"), selected = "Departments"),
             
             selectInput("primarySub", label = "To explore in detail to the right, select a specific one:",
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
    
    column(width = 4,
           box(
             width = NULL, status = "warning",
             selectInput("secondary", label = "Break down your selection to the left further by:",
                         choices = list("", "Departments", "Divisions", "Sections", "Fund Types", "Fund Groups",
                                        "Fund Names", "Object Classes", "Account Categories", "Account Names"),
                         selected = ""),
             
             selectInput("secondarySub", label = "To explore in more detail to the right, select a specific one:",
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
    ),
    
    column(width = 4,
           box(
             width = NULL, status = "danger",
             selectInput("tertiary", label = "Break down your selection to the left further by:",
                         choices = list("", "Departments", "Divisions", "Sections", "Fund Types", "Fund Groups",
                                        "Fund Names", "Object Classes", "Account Categories", "Account Names"),
                         selected = "")
#              selectInput("tertiarySub", label = "All of them, or a specific one?",
#                          choices = "All")
           ),
          box(
            title = textOutput("title3"), width = NULL, status = "danger",
            dataTableOutput("table3")
          ),
          box(
            width = NULL, background = "red",
            h4(textOutput("total3"), align = "center")
          )
    )
  )
)