# Deliverable 1
# import MechCar dataset
# mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
install.packages("dplyr")
mpg_table = read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
View(mpg_table)
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data=mpg_table) #create linear model
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,mpg_table)) #summarize linear model

#Deliverable 2
install.packages("dplyr")
# Download the Suspension_Coil.csv and place in active directory
Suspension_Coil_table = read.csv(file='Suspension_Coil.csv', check.names=F,stringsAsFactors = F)
View(Suspension_Coil_table)
#import Suspension_Coil dataset
head(Suspension_Coil_table)
#write an RScript that creates a total_summary dataframe using the sumarize() function to get M,M,V, and SE
summary_Suspension_Coil_table <- Suspension_Coil_table %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI),SD_PSI=sd(PSI))
#create a lot summary
lot_demo <- Suspension_Coil_table %>% group_by(Manufacturing_Lot) % > % summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))

# Deliverable 3
#perform t-test to determine if the PSI across 
# Peform t-test across all Lots
t.test(Suspension$PSI,mu = 1500)
# Peform t-test on Lot 1
t.test(subset(Suspension,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# Peform t-test on Lot 2
t.test(subset(Suspension,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# Peform t-test on Lot 3
t.test(subset(Suspension,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
