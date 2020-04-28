library(ggplot2)
library(dplyr)

UKAccidents <- read.csv("C:\\Users\\Dallo\\Desktop\\R Project\\road-accidents-incidence\\Kaagle_Upload.csv", header = TRUE)

#subset function is used to keep only the necessary columns from the data frame
UKAccidents <- subset(data.frame(UKAccidents), select = c("weather_conditions","number_of_casualties"))

#To determine the correlation coefficient
cor(UKAccidents$weather_conditions, UKAccidents$number_of_casualties)

#Descriptive Statistics
mean(UKAccidents$number_of_casualties, na.rm = TRUE)
median(UKAccidents$number_of_casualties, na.rm = TRUE)

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

#here, head function is used to produce sample output from the dataset
head(UKAccidents)

#the highest and lowest number of accidents due to various type of weather condition
#'fine no high winds' caused higher number of deaths while 'snowing + high winds' caused the lowest
carriers <- group_by(UKAccidents, weather_conditions)
summarise(carriers, n())

#bar graph is plotted with the help of ggplot
plot <- ggplot(data.frame(UKAccidents),aes(x= weather_conditions, y= number_of_casualties))+ 
  geom_bar(stat = "identity", fill= "steelblue") +
  theme_minimal() +
  ylim(0,240000) +
  theme(axis.text.x=element_text(angle=45 ,hjust=1,vjust=1))
plot