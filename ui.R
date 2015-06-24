## Sac City Budget Visualization ##
## ui.R ##
library(shinydashboard)
library(shiny)

dashboardPage(
  dashboardHeader(
    title = "City of Sacramento Budget Visualization",
    titleWidth = 400
  ),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    column(width = 4,
           box(
             width = NULL, status = "primary",
             
             selectInput("primary", label = "First, break down the budget by:",
                         choices = list("Departments", "Fund Types",
                                        "Object Classes"), selected = "Departments"),
             
             selectInput("primarySub", label = "All of them, or a specific one?",
                         choices = "")
#            uiOutput("selection1")         
           ),
           box(
             title = textOutput("title1"), width = NULL, status = "primary",
             plotOutput("chart1")
           ),
           box(
             width = NULL, background = "light-blue",
             h4(textOutput("total1"), align = "center")
           )
    ),
    
    column(width = 4,
           box(
             width = NULL, status = "warning",
             selectInput("secondary", label = "Second, break down the budget further by:",
                         choices = list("", "Departments", "Divisions", "Sections", "Fund Types", "Fund Groups",
                                        "Fund Names", "Object Classes", "Account Categories", "Account Names"),
                         selected = ""),
             
             selectInput("secondarySub", label = "All of them, or a specific one?",
                         choices = list("All" = 1, "???" = 2,
                                        "???" = 3), selected = 1)
           ),
           box(
             title = textOutput("title2"), width = NULL, status = "warning",
             plotOutput("chart2")
           ),
           box(
             width = NULL, background = "yellow",
             h4(textOutput("selection1"), align = "center")
           )
    ),
    
    column(width = 4,
           box(
             title = "Title 2", width = NULL, solidHeader = TRUE,
             "Box content"
           ),
           box(
             title = "Title 6", width = NULL, background = "maroon",
             "A box with a solid maroon background"
           )
    )
  )
)