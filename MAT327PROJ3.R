table(collision$`NUMBER OF MOTORIST INJURED`)
hist(collision$`NUMBER OF PEDESTRIANS INJURED`, main = "Histogram of Number of Pedestrians Injured in Car Accidents in New York City", xlab = "Number of Pedestrians Injured")
table(collision$`NUMBER OF PEDESTRIANS INJURED`)
range(collision$`NUMBER OF PEDESTRIANS INJURED`)
table(collision$BOROUGH)
barplot(table(collision$BOROUGH), main = "Number of Car Accidents for each Burough", xlab = "Burough", ylab = "Frequency")
hist(collision$`NUMBER OF MOTORIST INJURED`, main = "Histogram of Number of Motorists Injured in Car Accidents in New York City", xlab = "Number of Motorists Injured")
range(collision$`NUMBER OF MOTORIST INJURED`)
barplot(table(collision$`VEHICLE TYPE CODE 1`), main = "Vehicle Type for Car Accidents in New York City", xlab = "Vehicle Type", ylab = "Frequency")
table(collision$`VEHICLE TYPE CODE 1`)
max(collision$`NUMBER OF PEDESTRIANS INJURED`)
median(collision$`NUMBER OF PEDESTRIANS INJURED`)
mean(collision$`NUMBER OF PEDESTRIANS INJURED`)
max(collision$`NUMBER OF PEDESTRIANS INJURED`)
range(collision$`NUMBER OF PEDESTRIANS INJURED`)mean(collision$`NUMBER OF PEDESTRIANS INJURED`, trim = 0.05)
mean(collision$`NUMBER OF MOTORIST INJURED`, trim = 0.05)

median(collision$`NUMBER OF PEDESTRIANS INJURED`)

#milestone 4
#cleaning up vehicle type chart
table(collision$'VEHICLE TYPE CODE 1')
collision_update <- collision[collision$'VEHICLE TYPE CODE 1' %in% names(y[y >= 10]), ]
barplot(table(collision_update$`VEHICLE TYPE CODE 1`), main = "Vehicle Type for Car Accidents in New York City", xlab = "Vehicle Type", ylab = "Frequency")

#minimizing outliers for pedestrian injury chart
pedestrianInj <- subset(collision, `NUMBER OF PEDESTRIANS INJURED`< 4)
hist(pedestrianInj$`NUMBER OF PEDESTRIANS INJURED`)

#minimizing outliers for motorist injury chart
motoristInj <- subset(collision,`NUMBER OF MOTORIST INJURED`< 8)
hist(motoristInj$`NUMBER OF MOTORIST INJURED`, main = "Histogram of Number of Motorists Injured in Car Accidents in New York City", xlab = "Number of Motorists Injured")

#milestone 5
median(collision$`NUMBER OF PEDESTRIANS INJURED`)
mean(collision$`NUMBER OF PEDESTRIANS INJURED`)
var(collision$`NUMBER OF PEDESTRIANS INJURED`)
sd(collision$`NUMBER OF PEDESTRIANS INJURED`)
mean(collision$`NUMBER OF PEDESTRIANS INJURED`, trim = 0.05)
median(collision$`NUMBER OF MOTORIST INJURED`)
mean(collision$`NUMBER OF MOTORIST INJURED`)
var(collision$`NUMBER OF MOTORIST INJURED`)
sd(collision$`NUMBER OF MOTORIST INJURED`)
mean(collision$`NUMBER OF MOTORIST INJURED`, trim = 0.05)

#milestone 6

plot(`NUMBER OF MOTORIST INJURED`~`NUMBER OF PEDESTRIANS INJURED`, data = collision, xlab = "Number of Pedestrians Injured", ylab = "Number of Pedestrians Injured", main = "Number of Pedestrians Injured vs Motorists Injured")
collision.lm <- lm(`NUMBER OF MOTORIST INJURED`~`NUMBER OF PEDESTRIANS INJURED`,data = collision)
collision.lm
abline(collision.lm, col = "red")
plot(`NUMBER OF MOTORIST INJURED`~`NUMBER OF PEDESTRIANS INJURED`, data = collision, xlab = "Number of Pedestrians Injured", ylab = "Number of Pedestrians Injured", main = "Number of Pedestrians Injured vs Motorists Injured",abline(collision.lm, col = "red"))

#milestone 7
#pedestrian data
-qnorm(.05/2)
n = 2029427
x = mean(collision$`NUMBER OF PEDESTRIANS INJURED`)
sd = sd(collision$`NUMBER OF PEDESTRIANS INJURED`)
z = -qnorm(.05/2)
x + z * (sd/(sqrt(n)))
x - z * (sd/(sqrt(n)))
#[0.05500552, 0.05567049]

#motorist data 
sd = sd(collision$`NUMBER OF MOTORIST INJURED`)
x = mean(collision$`NUMBER OF MOTORIST INJURED`)
x + z * (sd/(sqrt(n)))
x - z * (sd/(sqrt(n)))
#[ 0.2181028, 0.2199084]

#milestone 8
library(datasets)
plot(`NUMBER OF MOTORIST INJURED`~`NUMBER OF PEDESTRIANS INJURED`, data = collision, xlab = "Number of Pedestrians Injured", ylab = "Number of Pedestrians Injured", main = "Number of Pedestrians Injured vs Motorists Injured",abline(collision.lm, col = "red"))
collision.lm <-lm(`NUMBER OF MOTORIST INJURED` ~ `NUMBER OF PEDESTRIANS INJURED`, data = collision)
collision.lm
cor(collision$`NUMBER OF PEDESTRIANS INJURED`,collision$`NUMBER OF MOTORIST INJURED`)
cor(collision$`NUMBER OF PEDESTRIANS INJURED`,collision$`NUMBER OF MOTORIST INJURED`)^2
summary(collision.lm)
resid(collision.lm)
hist(resid(collision.lm), main = "Histogram of residuals")
plot(collision$`NUMBER OF PEDESTRIANS INJURED`,resid(collision.lm), xlab = "Number of Pedestrians Injured", ylab = "Residuals", main = "Residuals vs Number of Pedestrians Injured", abline = (h=0))

#milestone 9
collisionBrok <- subset(collision,BOROUGH == c("BROOKLYN"))
mean(collisionBrok$`NUMBER OF PEDESTRIANS INJURED`)
x = mean(collisionBrok$`NUMBER OF PEDESTRIANS INJURED`)
u = mean(collision$`NUMBER OF PEDESTRIANS INJURED`)
n = 443858
sd(collision$`NUMBER OF PEDESTRIANS INJURED`)
s = sd(collision$`NUMBER OF PEDESTRIANS INJURED`)
(x - u)/ (s/ (sqrt(n)))
qnorm(0.02/2)
mean(collisionBrok$`NUMBER OF MOTORIST INJURED`)
x = mean(collisionBrok$`NUMBER OF MOTORIST INJURED`)
s = sd(collision$`NUMBER OF MOTORIST INJURED`)
u = mean(collision$`NUMBER OF MOTORIST INJURED`)
(x - u)/ (s/ (sqrt(n)))
qnorm(0.1/2)

#Milestone 10
boxplot(`NUMBER OF PEDESTRIANS INJURED`~ BOROUGH, data = collision, main = "Boxplot of the Number of Pedestrians Injured per Car Accident in NYC for each Borough")
