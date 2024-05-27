library(Rcmdr)
attach(Prestige_New)

getwd() 
setwd("C:/Users/Suneth-PC/OneDrive/Desktop/BA IDU/R Final") 
occupations_data <- read.csv("Prestige_New.csv")
str(occupations_data)
summary(occupations_data)


#Task 3
min(Prestige_New$income)
max(Prestige_New$income)
mean(Prestige_New$income)
median(Prestige_New$income)

install.packages("DescTools")
library("DescTools")
Mode(Prestige_New$income)

#Task 4
summary(Prestige_New$prestige)
summary(Prestige_New$education)
summary(Prestige_New$income)

#Task 5



# Histogram for prestige
hist(Prestige_New$prestige, prob = TRUE, main = "Prestige Distribution", xlab = "Prestige", ylab = "Frequency")
curve(dnorm(x, mean = mean(Prestige_New$prestige), sd = sd(Prestige_New$prestige)), add = TRUE, col = "blue")


# Histogram for education
hist(Prestige_New$education, prob = TRUE, main = "Education Distribution", xlab = "Education", ylab = "Frequency")
curve(dnorm(x, mean = mean(Prestige_New$education), sd = sd(Prestige_New$education)), add = TRUE, col = "blue")


# Histogram for income
hist(Prestige_New$income, prob = TRUE, main = "Income Distribution", xlab = "Income", ylab = "Frequency")
curve(dnorm(x, mean = mean(Prestige_New$income), sd = sd(Prestige_New$income)), add = TRUE, col = "blue")


# Reset plotting settings
par(mfrow = c(1, 1))  # Reset to default plotting layout



# Task 6

filtered_data <- subset(Prestige_New, type %in% c("bc", "prof", "wc"))

library(ggplot2)
library(car)  

boxplot <- Boxplot(prestige ~ type, data = Prestige_New, id = list(method = "y"),
                   names = c("bc", "prof", "wc"),
                   main = "Prestige by Occupation Type", xlab = "Occupation Type", ylab = "Prestige")







#Task 7

if (!requireNamespace("nortest", quietly = TRUE)) {
  install.packages("nortest")
}
library(nortest)

anderson_darling_test <- ad.test(Prestige_New$prestige)
cat("Anderson-Darling Normality Test:\n")
print(anderson_darling_test)

# Lilliefors (Kolmogorov-Smirnov) normality test
lilliefors_test <- lillie.test(Prestige_New$prestige)
cat("\nLilliefors (Kolmogorov-Smirnov) Normality Test:\n")
print(lilliefors_test)

# Shapiro-Wilk normality test
shapiro_test <- shapiro.test(Prestige_New$prestige)
cat("\nShapiro-Wilk Normality Test:\n")
print(shapiro_test)










