# Assignment: 1.3 R Refresher

# Name: Valenzuela, Gabriel

# Date: 06 / 05 / 2020


### 1. Import, Plot, Summarize, and Save Data

# Import libraries
library(pastecs)
library(ggplot2)
library(hexbin)

# Importing the data
earnings <- read.csv("table1.csv")
head(earnings, 10)

# Converting to numeric
earnings$Number.of.workers..in.thousands....Total <- as.numeric(earnings$Number.of.workers..in.thousands....Total)
earnings$Number.of.workers..in.thousands....Men <- as.numeric(earnings$Number.of.workers..in.thousands....Men)
earnings$Number.of.workers..in.thousands....Women <- as.numeric(earnings$Number.of.workers..in.thousands....Women)

# Summary Descriptives
round(stat.desc(earnings$Number.of.workers..in.thousands....Men), 2)
round(stat.desc(earnings$Number.of.workers..in.thousands....Women), 2)

# Plot some of the features of several variables
men_weekly_earnings <- earnings$Median.weekly.earnings..in.current.dollars....Men
women_weekly_earnings <- earnings$Median.weekly.earnings..in.current.dollars....Women
hist(men_weekly_earnings, main="Men's Weekly Earnings", xlab = "Earnings ($)")
hist(women_weekly_earnings, main="Women's Weekly Earnings", xlab = "Earnings ($)")

boxplot(men_weekly_earnings, main="Men's Weekly Earnings", xlab = "Earnings ($)")
boxplot(women_weekly_earnings, main="Women's Weekly Earnings", xlab = "Earnings ($)")

# Save data locally
write.csv(earnings, file = 'MenVsWomenMedianEarnings.csv')


### 2. Explore Some Bivariate Relations

# Bivariate Relations
ggplot2::ggplot(earnings, ggplot2::aes(x = Year, y = men_weekly_earnings, fill = Quarter)) + ggplot2::geom_bar(stat = "identity")
ggplot2::ggplot(earnings, ggplot2::aes(x = Year, y = women_weekly_earnings, fill = Quarter)) + ggplot2::geom_bar(stat = "identity")


# Correlation to number of men workers and earnings
men_employees <- earnings$Number.of.workers..in.thousands....Men
cor.test(men_employees, men_weekly_earnings, method = "pearson")

# Correlation to number of women workers and earnings
women_employees <- earnings$Number.of.workers..in.thousands....Women
cor.test(women_employees, women_weekly_earnings, method = "pearson")


