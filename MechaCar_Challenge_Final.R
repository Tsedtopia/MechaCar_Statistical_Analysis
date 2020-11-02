# import MechCar dataset
mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
install.packages("dplyr")
View(mpg_table)
lm(qsec ~ hp,mtcars) #create linear model
summary(lm(qsec~hp,mtcars)) #summarize linear model
mpg_table <- lm(qsec ~ hp,mpg) #create linear model
> yvals <- mpg_table$coefficients['hp']*mpg$hp +
  mpg_table$coefficients['(Intercept)'] #determine y-axis values from linear model
model <- lm(qsec ~ hp,mtcars) #create linear model
> yvals <- model$coefficients['hp']*mtcars$hp +
  model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter
total_summary <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
View(total_summary)
summarize_demo <- total_summary %>% group_by(condition) %>% summarize(Mean_Mileage=mean(PSI),Manufacturing_Lot=Man(Lot),VehicleID=n(), .groups = 'keep') #create summary table with multiple columns
summarize_demo <- total_summary %>% group_by(condition) %>% summarize(Mean_PSI=mean(PSI),VehicleID=n(), .groups = 'keep') #create summary table with multiple columns
View(total_summary)
summarize_demo <- total_summary %>% group_by(condition) %>% summarize(Mean_PSI=mean(PSI),VehicleID=n(), .groups = 'total_summary') #create summary table with multiple columns
summarize_demo <- total_summary %>% group_by(condition) %>% summarize(Mean_PSI=mean(PSI).groups = 'keep') #create summary
summarize_demo <- total_summary %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer), .groups = 'keep')
summarize_demo <- total_summary %>% group_by(condition) %>% summarize(Mean_PSI=mean(VehicleID), .groups = 'keep')
summarize_demo <- total_summary %>% group_by(condition) %>% summarize(PSI=mean(VehicleID), .groups = 'keep')

