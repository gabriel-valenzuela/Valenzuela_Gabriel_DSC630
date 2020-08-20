# Assignment: 3.3 Marketing Promotion

# Name: Valenzuela, Gabriel

# Date: 06 / 20 / 2020

# Import Libraries
library(ggplot2)


# Import Data
game_schedule <- read.csv('dodgers_cleaned.csv')
head(game_schedule, 10)
str(game_schedule)

# Box Plot - Day of Week & Months
plot(game_schedule$day_of_week, game_schedule$attend, xlab = 'Day of Week', ylab='Attendance', main='Attendance of Games per Weekday')
plot(game_schedule$month, game_schedule$attend, xlab = 'Month', ylab='Attendance', main='Attendance of Games per Month')

# Train and Test Data

trainSize <- round(nrow(game_schedule) * 0.7)
testSize <- nrow(game_schedule) - trainSize

set.seed(157)
training_indices <- sample(seq_len(nrow(game_schedule)),size=trainSize)
trainSet <- game_schedule[training_indices, ]
testSet <- game_schedule[-training_indices, ]

# Create model parameters
attendance_model <- (attend ~ month + day_of_week)

# Model for training
train_model <- lm(attendance_model, data = trainSet)
summary(train_model)
trainSet$prediction <- predict(train_model)

# Model for testing
testSet$prediction <-predict(train_model, newdata=testSet)


# Visualizing training data
# predict_month_plot_train <- ggplot2::ggplot(trainSet, aes(x=trainSet$month, y=trainSet$prediction)) + ggplot2::geom_point()
# 
# predict_month_plot_train + facet_grid(. ~ trainSet$day_of_week) + ggplot2::theme(axis.text.x = element_text(angle = 90)) + ggplot2::xlab('Day of the Week') + ggplot2::ylab('Attendance of Fans') + ggplot2::ggtitle('')
# 
# 
# predict_day_plot_train <- ggplot2::ggplot(trainSet, aes(x=trainSet$day_of_week, y=trainSet$prediction)) + ggplot2::geom_point()
# 
# predict_day_plot_train + facet_grid(. ~ trainSet$month) + ggplot2::theme(axis.text.x = element_text(angle = 90))




# Visualize testing data
predict_month_plot_test <- ggplot2::ggplot(testSet, aes(x=testSet$month, y=testSet$prediction)) + ggplot2::geom_point()

predict_month_plot_test + facet_grid(. ~ testSet$day_of_week) + ggplot2::theme(axis.text.x = element_text(angle = 90)) + ggplot2::xlab('Months') + ggplot2::ylab('Attendance of Fans') + ggplot2::ggtitle('Prediction of Attendance',subtitle = 'Day of the Week')


predict_day_plot_test <- ggplot2::ggplot(testSet, aes(x=testSet$day_of_week, y=testSet$prediction)) + ggplot2::geom_point()

predict_day_plot_test + facet_grid(. ~ testSet$month) + ggplot2::theme(axis.text.x = element_text(angle = 90)) + ggplot2::xlab('Days of the Week') + ggplot2::ylab('Attendance of Fans') + ggplot2::ggtitle('Prediction of Attendance',subtitle = 'Months')






