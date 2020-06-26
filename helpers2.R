## Sac City Budget Visualization ##
## helpers2.R ## 

#Read in data and calculate one-time variables
SacBudget = read.csv("data/Approved_Budget_FY_2015__FY_2018.csv", stringsAsFactors = F)
names(SacBudget)[22] = "OBJECT_CLASS"

grand_total = sum(SacBudget$BUDGET_AMOUNT[which(SacBudget$EXPREV == "Expenses" &
                                                  SacBudget$YEAR == "FY18" &
                                                  SacBudget$OPERATING_UNIT_DESCRIPTION != "Non-Appropriated")])

departments = as.character(unique(SacBudget$OPERATING_UNIT_DESCRIPTION))[order(as.character(unique(SacBudget$OPERATING_UNIT_DESCRIPTION)))]
fund_types = as.character(unique(SacBudget$FUND_GROUP))[order(as.character(unique(SacBudget$FUND_GROUP)))]
object_class1 = SacBudget$OBJECT_CLASS[which(SacBudget$EXPREV == "Expenses")]
object_classes = as.character(unique(object_class1))[order(as.character(unique(object_class1)))]

class_list = function(second_selection, first_selection, first_subselection, budget_data) {
  col_num = which(names(budget_data) == first_selection)
  temp = budget_data[which(budget_data[,col_num] == first_subselection & budget_data$YEAR == "FY18"),]
  col_num2 = which(names(temp) == second_selection)
  returner = as.character(unique(temp[,col_num2]))[order(as.character(unique(temp[,col_num2])))]
  return(as.list(c("All",returner)))
}

budget_table = function(data_selection, data_subselection, budget_data) {

  budget_now = budget_data %>%
    filter(EXPREV == "Expenses",
           YEAR == "FY18",
           OPERATING_UNIT_DESCRIPTION != "Non-Appropriated") %>%
    select(graph_data = all_of(data_selection),
           BUDGET_AMOUNT) %>%
    group_by(graph_data) %>%
    summarise(Budget18 = sum(BUDGET_AMOUNT), .groups = "drop")
    
  if (length(which(budget_data$YEAR == "FY17")) > 0) {
    budget_last = budget_data %>%
      filter(EXPREV == "Expenses",
             YEAR == "FY17",
             OPERATING_UNIT_DESCRIPTION != "Non-Appropriated") %>%
      select(graph_data = all_of(data_selection),
             BUDGET_AMOUNT) %>%
      group_by(graph_data) %>%
      summarise(Budget17 = sum(BUDGET_AMOUNT), .groups = "drop")
  } else {
    budget_last = data.frame("graph_data" = "None", "Budget17" = 0)
  }
  
  #Calculate percent change from year to year
  budget_both = merge(budget_now, budget_last, all.x = TRUE)
  budget_both$PctChange = (budget_both$Budget18 / budget_both$Budget17) - 1
  budget_both$PctChange[which(is.nan(budget_both$PctChange))] = 0
  
  #Subset to sub-selection, if necessary
  if (data_subselection != "All") {
    budget_both = budget_both[which(budget_both$graph_data == data_subselection), ]
  }
  
  budget_both$Budget18 = round(budget_both$Budget18 / 1000000, digits = 2)
  budget_both = budget_both[order(budget_both$Budget18, decreasing = TRUE),]
  return(budget_both)
  
}

color_from_middle <- function (data, color1, color2, cutpt) 
{
  max_val=max(abs(data))
  JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (%f + value/%s * (Math.abs(%f - 50) + 50)) + '%%, %s ' + (%f + value/%s * (Math.abs(%f - 50) + 50)) + '%%,%s  %f%%,transparent %f%%)': 'linear-gradient(90deg, transparent, transparent %f%%, %s %f%%, %s ' + (%f + value/%s * (Math.abs(%f - 50) + 50)) + '%%, transparent ' + (%f + value/%s * (Math.abs(%f - 50) + 50)) + '%%)'",
             cutpt,max_val,cutpt,color1,cutpt,max_val,cutpt,color1,cutpt,cutpt,cutpt,color2,cutpt,color2,cutpt,max_val,cutpt,cutpt,max_val,cutpt))
} 