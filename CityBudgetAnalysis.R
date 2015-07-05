###__________________
###Initial exploration of Sac City budget data
###__________________

SacBudget = read.csv("data/92727-city-of-sacramento-proposed-budget-fiscal-year-201516.csv")
names(SacBudget)[11] = "OBJECT.CLASS"
attach(SacBudget)

table(Year)
table(Fiscal.Year)
table(DEPARTMENT)

test_variable = SacBudget$DEPARTMENT[which(SacBudget$DEPARTMENT == "Police")]
test_variable = SacBudget$DEPARTMENT

#Plot budget size for each Department
DeptSize16 = aggregate(BUDGET.AMOUNT[which(EXP.REV == "Expenses" & Year == 2016 & DEPARTMENT != "Non-Appropriated")] ~
                       test_variable[which(EXP.REV == "Expenses" & Year == 2016 & DEPARTMENT != "Non-Appropriated")],
                     "sum", data = SacBudget)
names(DeptSize16)[1] = "DEPARTMENT"
names(DeptSize16)[2] = "Budget1516"
DeptSize15 = aggregate(BUDGET.AMOUNT[which(EXP.REV == "Expenses" & Year == 2015 & DEPARTMENT != "Non-Appropriated")] ~
                         test_variable[which(EXP.REV == "Expenses" & Year == 2015 & DEPARTMENT != "Non-Appropriated")],
                       "sum", data = SacBudget)
names(DeptSize15)[1] = "DEPARTMENT"
names(DeptSize15)[2] = "Budget1415"

#Calculate percent change from year to year
DeptSize = merge(DeptSize16, DeptSize15)
DeptSize$PctChange = (DeptSize$Budget1516 / DeptSize$Budget1415) - 1
DeptSize$PctChange[which(is.nan(DeptSize$PctChange))] = 0
DeptSize$DEPARTMENT = as.character(DeptSize$DEPARTMENT)

DeptSize = DeptSize[which(DeptSize$DEPARTMENT == "Police"),]

#Color bars by percent change from previous year
colfunc <- colorRampPalette(c("red", "white", "green4"))
plot(rep(1,100),col=colfunc(100),pch=19) #just shows you the color ramp
DeptSize$ChangeCol = NA
for (item in 1:nrow(DeptSize)) {
  if (DeptSize$PctChange[item] > -0.5 & DeptSize$PctChange[item] < 0.5) {
    DeptSize$ChangeCol[item] = colfunc(100)[round((DeptSize$PctChange[item] * 100) + 50)]
  } else if (DeptSize$PctChange[item] <= -0.5) {
    DeptSize$ChangeCol[item] = colfunc(100)[1]
  } else {
    DeptSize$ChangeCol[item] = colfunc(100)[100]
  }
}
#DeptSize$ChangeCol = colfunc(100)[round((DeptSize$PctChange * 100) + 50)]
#DeptSize$ChangeCol[which(DeptSize$PctChange > -0.5 & DeptSize$PctChange < 0.5)] =
#  colfunc(100)[round((DeptSize$PctChange * 100) + 50)]
#DeptSize$ChangeCol[which(DeptSize$PctChange <= -0.5)] = 1
#DeptSize$ChangeCol[which(DeptSize$PctChange >= 0.5)] = 100

par(mar = c(4,9,4,2))
barplot(DeptSize$Budget1516[order(DeptSize$Budget1516)], col = DeptSize$ChangeCol[order(DeptSize$Budget1516)],
        space = 1, horiz = TRUE, xaxt = "n")
title("FY2015/16 Sacramento City Budget by Department", adj = 1)
end_point = 0.5 + nrow(DeptSize) + nrow(DeptSize) - 1
axis(2, at = seq(1.5, end_point, by = 2), labels = DeptSize$DEPARTMENT[order(DeptSize$Budget1516)],
     tick = FALSE, las = 1, cex.axis = 0.65)
text(x = 0, y = seq(1.5,end_point,by=2), pos = 2, xpd = TRUE,
     labels = DeptSize$DEPARTMENT[order(DeptSize$Budget1516)], cex = 0.65)
axis(1, at = c(0,20000000,40000000,60000000,80000000,100000000,120000000),
     labels = c("","$20M","$40M","$60M","$80M","$100M","$120M"), cex.axis = 0.85)

bar_color = DeptSize$ChangeCol
DeptSize = DeptSize[,c(1,2,4)]
datatable(DeptSize, rownames = FALSE, class = "compact",
          colnames = c("", "Amount in Millions", "Change"),
          options = list(searching = FALSE,
                         pageLength = 20,
                         dom = "t")) %>%
  formatStyle("Budget1516",
              background = styleColorBar(DeptSize$Budget1516, bar_color, angle = -90),
              backgroundSize = "100% 90%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
  ) %>%
  formatStyle(c(1:3), fontSize = "12px") %>%
  formatCurrency("Budget1516", digits = 1) %>%
  formatPercentage("PctChange")

str(aggregate(Id ~ DEPARTMENT, "length", data = SacBudget))

length(table(DIVISION))
length(table(SECTION))
table(FUND.TYPE)
table(FUND.GROUP)
length(table(FUND.NAME))
table(EXP.REV)
table(OBJECT.CLASS)
table(ACCOUNT.CATEGORY)
length(table(ACCOUNT.NAME))
summary(BUDGET.AMOUNT)
SacBudget[which(BUDGET.AMOUNT == 94901568),]
sum(BUDGET.AMOUNT[which(Year == 2015)]) # $1.6 billion
sum(BUDGET.AMOUNT[which(Year == 2016)]) # $1.7 billion
