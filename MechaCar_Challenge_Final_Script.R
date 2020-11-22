# Deliverable 1
# import MechCar dataset
# mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
#install.packages("dplyr")
library("dplyr")
mpg_table = read.csv(file.choose(),check.names=F,stringsAsFactors = F)
View(mpg_table)
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data=mpg_table) #create linear model
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,mpg_table)) #summarize linear model
#file='MechaCar_mpg.csv'
#Deliverable 2
#install.packages("dplyr")

# Download the Suspension_Coil.csv and place in active directory
Suspension_Coil_table = read.csv(file.choose(), check.names=F,stringsAsFactors = F)
View(Suspension_Coil_table)
#import Suspension_Coil dataset
head(Suspension_Coil_table)
#write an RScript that creates a total_summary dataframe using the sumarize() function to get M,M,V, and SE
summary_Suspension_Coil_table <- Suspension_Coil_table %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI),SD_PSI=sd(PSI))
str(summary_Suspension_Coil_table)
Suspension_Coil_table$Manufacturing_Lot = as.factor(Suspension_Coil_table$Manufacturing_Lot)
#create a lot summary
lot_demo <- Suspension_Coil_table %>% group_by(Manufacturing_Lot) %>% summarise(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))
lot_demo
1
# Deliverable 3
#perform t-test to determine if the PSI across 
# Peform t-test across all Lots
t.test(Suspension_Coil_table$PSI,mu = 1500)
# Peform t-test on Lot 1
t.test(subset(Suspension_Coil_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# Peform t-test on Lot 2
t.test(subset(Suspension_Coil_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# Peform t-test on Lot 3
t.test(subset(Suspension_Coil_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
