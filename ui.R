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
                  City of Sacramento's</a> <em>proposed</em>* <a href='http://www.cityofsacramento.org/Finance/Budget/'>
                  2015-16 budget</a>.</h4>
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
               </h4>
               <br><br><br>
               <h6>*Data is taken from the city's <a href='http://data.cityofsacramento.org/home/'>Open Data Portal</a>.
               Once the portal is updated to include the city's <em>adopted</em> 2015-16 budget, this site will be
               updated accordingly.</h6>
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
#                selectInput("tertiarySub", label = "All of them, or a specific one?",
#                            choices = "All")
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