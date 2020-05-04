library(ggplot2)
library(dpylr)
library(shiny)
library(knitr)

UKAccidents <- read.csv("UKAccidentData.csv", header = TRUE)

#subset function is used to keep only the necessary columns from the data frame
UKAccidents <- subset(data.frame(UKAccidents), select = c("weather_conditions","number_of_casualties", "number_of_vehicles", "accident_severity"))

#here, head function is used to produce sample output from the dataset
head(UKAccidents)

#To determine the correlation coefficient
cor(UKAccidents$weather_conditions, UKAccidents$number_of_casualties)

#Descriptive Statistics
#there is no function available in R to find mode
mean(UKAccidents$number_of_casualties, na.rm = TRUE)
median(UKAccidents$number_of_casualties, na.rm = TRUE)

#H0 is rejected because the p-value is less than alfa=0.05 
#conclude that the result is statiscally significant
fit <- aov(UKAccidents$number_of_casualties ~ UKAccidents$weather_conditions, data=UKAccidents)
summary(fit)

#Replacing the values of weather conditions with their respective meaning
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 1] <- 'Fine no high winds'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 2] <- 'Raining no high winds'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 3] <- 'Snowing no high winds'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 4] <- 'Fine + high winds'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 5] <- 'Raining + high winds'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 6] <- 'Snowing + high winds'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 7] <- 'Fog or mist'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 8] <- 'Other'
UKAccidents$weather_conditions[UKAccidents$weather_conditions == 9] <- 'Unknown'

#the highest and lowest number of accidents due to various type of weather condition
#'fine no high winds' caused higher number of deaths while 'snowing + high winds' caused the lowest
freq <- group_by(UKAccidents, weather_conditions)
summarise(freq, n())

#bar graph is plotted with the help of ggplot
plot <- ggplot(data.frame(UKAccidents),aes(x= weather_conditions, y= number_of_casualties))+ 
  geom_bar(stat = "identity", fill= "steelblue") +
  theme_minimal() +
  ylim(0,240000) +
  xlab("Weather Condition")+
  ylab("Frequency of accidents")+
  theme(axis.text.x=element_text(angle=45 ,hjust=1,vjust=0.1))
plot

sumOfCasualties <- aggregate(UKAccidents$number_of_casualties, by=list(Category=UKAccidents$weather_conditions), FUN=sum)

#the graph is plotted to show the number of casualties due to weather conditions 
plot <- ggplot(data.frame(sumOfCasualties),aes(x= Category, y= x))+ 
  geom_bar(stat = "identity", fill= "#ABDF2A") +
  xlab("Weather Condition")+
  ylab("Number of casualties")+
  theme_minimal() +
  ylim(0,375000) +
  theme(axis.text.x=element_text(angle=90 ,hjust=1,vjust=1))
plot

#this is to find number of records grouped by severity levels.
UKAccidents$accident_severity[UKAccidents$accident_severity == 1] <- 'Fatal'
UKAccidents$accident_severity[UKAccidents$accident_severity == 2] <- 'Serious'
UKAccidents$accident_severity[UKAccidents$accident_severity == 3] <- 'Slight'

severityFreq <- group_by(UKAccidents, accident_severity)
summarise(severityFreq, n())

#number of vechiles affected in the road accident in multiple weather condition 
sumOfVehiclesAffected <- aggregate(UKAccidents$number_of_vehicles, by=list(Category=UKAccidents$weather_conditions), FUN=sum)
colnames(sumOfVehiclesAffected) <- c("weather conditions","number of vehicles")
print(colnames(sumOfVehiclesAffected))
print(sumOfVehiclesAffected)