## Sac City Budget Visualization ##
## helpers.R ##

#Read in data and calculate one-time variables
SacBudget = read.csv("data/92727-city-of-sacramento-proposed-budget-fiscal-year-201516.csv")
names(SacBudget)[11] = "OBJECT.CLASS"

grand_total = sum(SacBudget$BUDGET.AMOUNT[which(SacBudget$EXP.REV == "Expenses" &
                                                  SacBudget$Year == 2016 &
                                                  SacBudget$DEPARTMENT != "Non-Appropriated")])
departments = as.character(unique(SacBudget$DEPARTMENT))[order(as.character(unique(SacBudget$DEPARTMENT)))]
divisions = unique(SacBudget$DIVISION)
sections = unique(SacBudget$SECTION)
fund_types = as.character(unique(SacBudget$FUND.TYPE))[order(as.character(unique(SacBudget$FUND.TYPE)))]
fund_groups = unique(SacBudget$FUND.GROUP)
fund_names = unique(SacBudget$FUND.NAME)
object_classes = as.character(unique(SacBudget$OBJECT.CLASS))[order(as.character(unique(SacBudget$OBJECT.CLASS)))]
account_categories = unique(SacBudget$ACCOUNT.CATEGORY)
account_names = unique(SacBudget$ACCOUNT.NAME)

#Graph primary chart
budget_graph = function(data_selection) {
  #Calculate current and last year's budget for each selected category
  budget_now = aggregate(SacBudget$BUDGET.AMOUNT[which(SacBudget$EXP.REV == "Expenses" &
                                                         SacBudget$Year == 2016 &
                                                         SacBudget$DEPARTMENT != "Non-Appropriated")] ~
                           data_selection[which(SacBudget$EXP.REV == "Expenses" &
                                                  SacBudget$Year == 2016 &
                                                  SacBudget$DEPARTMENT != "Non-Appropriated")],
                         "sum", data = SacBudget)
  names(budget_now)[1] = "data_selection"
  names(budget_now)[2] = "Budget1516"
  budget_last = aggregate(SacBudget$BUDGET.AMOUNT[which(SacBudget$EXP.REV == "Expenses" &
                                                         SacBudget$Year == 2015 &
                                                         SacBudget$DEPARTMENT != "Non-Appropriated")] ~
                           data_selection[which(SacBudget$EXP.REV == "Expenses" &
                                                  SacBudget$Year == 2015 &
                                                  SacBudget$DEPARTMENT != "Non-Appropriated")],
                         "sum", data = SacBudget)
  names(budget_last)[1] = "data_selection"
  names(budget_last)[2] = "Budget1415"
  
  #Calculate percent change from year to year
  budget_both = merge(budget_now, budget_last)
  budget_both$PctChange = (budget_both$Budget1516 / budget_both$Budget1415) - 1
  budget_both$PctChange[which(is.nan(budget_both$PctChange))] = 0
  
  #Color bars by percent change from previous year
  colfunc <- colorRampPalette(c("red", "white", "green4"))
  budget_both$ChangeCol = NA
  for (item in 1:nrow(budget_both)) {
    if (budget_both$PctChange[item] > -0.5 & budget_both$PctChange[item] < 0.5) {
      budget_both$ChangeCol[item] = colfunc(100)[round((budget_both$PctChange[item] * 100) + 50)]
    } else if (budget_both$PctChange[item] <= -0.5) {
      budget_both$ChangeCol[item] = colfunc(100)[1]
    } else {
      budget_both$ChangeCol[item] = colfunc(100)[100]
    }
  }
  
  #Construct plot
  par(mar = c(2,9,0,1))
  barplot(budget_both$Budget1516[order(budget_both$Budget1516)],
          col = budget_both$ChangeCol[order(budget_both$Budget1516)],
          space = 1, horiz = TRUE, xaxt = "n")
#  title("FY2015/16 Sacramento City Budget by Department", adj = 1)
  end_point = 0.5 + nrow(budget_both) + nrow(budget_both) - 1
  axis(2, at = seq(1.5, end_point, by = 2), labels = budget_both$data_selection[order(budget_both$Budget1516)],
       tick = FALSE, las = 1, cex.axis = 0.65)
  max_val = max(budget_both$Budget1516)
  label_vector = seq(0, floor(max_val / 20000000) * 20, 20)
  axis(1, at = seq(0, max_val, 20000000),
       labels = paste0("$",label_vector,"M"), cex.axis = 0.85)
  
  #Calculate total
  
}