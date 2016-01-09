## Sac City Budget Visualization ##
## helpers.R ## 

#Read in data and calculate one-time variables
SacBudget = read.csv("data/92727-city-of-sacramento-proposed-budget-fiscal-year-201516.csv")
names(SacBudget)[11] = "OBJECT.CLASS"

grand_total = sum(SacBudget$BUDGET.AMOUNT[which(SacBudget$EXP.REV == "Expenses" &
                                                  SacBudget$Year == 2016 &
                                                  SacBudget$DEPARTMENT != "Non-Appropriated")])

departments = as.character(unique(SacBudget$DEPARTMENT))[order(as.character(unique(SacBudget$DEPARTMENT)))]
fund_types = as.character(unique(SacBudget$FUND.TYPE))[order(as.character(unique(SacBudget$FUND.TYPE)))]
object_class1 = SacBudget$OBJECT.CLASS[which(SacBudget$EXP.REV == "Expenses")]
object_classes = as.character(unique(object_class1))[order(as.character(unique(object_class1)))]

class_list = function(second_selection, first_selection, first_subselection, budget_data) {
  col_num = which(names(budget_data) == first_selection)
  temp = budget_data[which(budget_data[,col_num] == first_subselection),]
  col_num2 = which(names(temp) == second_selection)
  returner = as.character(unique(temp[,col_num2]))[order(as.character(unique(temp[,col_num2])))]
  return(as.list(c("All",returner)))
}

budget_table = function(data_selection, data_subselection, budget_data) {

  col_num = which(names(budget_data) == data_selection)
  graph_data = budget_data[ , col_num]
  
  #Calculate current and last year's budget for each selected category
  budget_now = aggregate(budget_data$BUDGET.AMOUNT[which(budget_data$EXP.REV == "Expenses" &
                                                         budget_data$Year == 2016 &
                                                         budget_data$DEPARTMENT != "Non-Appropriated")] ~
                           graph_data[which(budget_data$EXP.REV == "Expenses" &
                                              budget_data$Year == 2016 &
                                              budget_data$DEPARTMENT != "Non-Appropriated")],
                         "sum", data = budget_data)
  names(budget_now)[1] = "graph_data"
  names(budget_now)[2] = "Budget1516"
  budget_last = aggregate(budget_data$BUDGET.AMOUNT[which(budget_data$EXP.REV == "Expenses" &
                                                          budget_data$Year == 2015 &
                                                          budget_data$DEPARTMENT != "Non-Appropriated")] ~
                            graph_data[which(budget_data$EXP.REV == "Expenses" &
                                               budget_data$Year == 2015 &
                                               budget_data$DEPARTMENT != "Non-Appropriated")],
                          "sum", data = budget_data)
  names(budget_last)[1] = "graph_data"
  names(budget_last)[2] = "Budget1415"
  
  #Calculate percent change from year to year
  budget_both = merge(budget_now, budget_last)
  budget_both$PctChange = (budget_both$Budget1516 / budget_both$Budget1415) - 1
  budget_both$PctChange[which(is.nan(budget_both$PctChange))] = 0
  
  #Subset to sub-selection, if necessary
  if (data_subselection != "All") {
    budget_both = budget_both[which(budget_both$graph_data == data_subselection), ]
  }
  
  budget_both$Budget1516 = round(budget_both$Budget1516 / 1000000, digits = 2)
  budget_both = budget_both[order(budget_both$Budget1516, decreasing = TRUE),]
  return(budget_both)
  
}