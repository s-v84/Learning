#############################################################################
## (1) Install the 'nutshell' package and load up the mort06.smpl dataset.
##
install.packages("nutshell")
library(nutshell)
data("mort06.smpl")

mean(mort06.smpl$age, na.rm = TRUE)
median(mort06.smpl$age, na.rm = TRUE)
quantile(mort06.smpl$age, na.rm = TRUE, c(0.25,0.75))
mean(mort06.smpl$age, na.rm = TRUE, trim = 0.15)

sd(mort06.smpl$age, na.rm = TRUE)
var(mort06.smpl$age, na.rm = TRUE)
IQR(mort06.smpl$age, na.rm = TRUE)
range(mort06.smpl$age, na.rm = TRUE)

prop.table(addmargins(table(mort06.smpl$Sex, mort06.smpl$Cause)))

table(mort06.smpl$Cause)

#Table both variables of interest
sexCauseReltnTable <- table(mort06.smpl$Sex, mort06.smpl$Cause)
print(t(sexCauseReltnTable))

#Compute row-wise proportions (cause of deaths among women and men 
#as proportions of total deaths in each gender)
# Round to two decimal places and transpose for easier readability
t(round(prop.table(x = sexCauseReltnTable, 1), 2))

# Compute column-wise proportions (the proportion of deaths for each gender for 
# each individual cause)
# Round to two decimal places and transpose for easier readability
t(round(prop.table(x = sexCauseReltnTable, 2), 2))

