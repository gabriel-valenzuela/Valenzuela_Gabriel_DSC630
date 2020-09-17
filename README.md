# Video Games Sales Prediction Modeling

## Predictive Analytics Project 

![Video Game Icon](https://github.com/gabriel-valenzuela/gabriel-valenzuela.github.io/blob/master/images/VideoGameIcon2.png)

### Objective
Video games have become a multi-million form of entertainment, technology, and art all wrapped into one experience for someone of almost any age to experience on various platforms. As technology in our world continually evolves and allows for us to dive deeper into it, so do the systems that allow us to play all different types of video games. With these new systems, or consoles, being distributed around the world every seven to nine years, companies are interested to determine the future of video games sales as they release the newest platforms which have the possibility of acting faster, providing sharper and more realistic graphics, and creating whole new worlds within games. Therefore, this project focuses on the business inquiry of predicting future video game sales upon the release of the newest consoles allowing for these games to be played by the video game community. 

### Environment

Python and R were utilized within a jupyter notebook and R Studio, respectively to complete the analysis

#### Libraries

```python
from __future__ import division
import warnings
warnings.filterwarnings("ignore")

from datetime import datetime, timedelta,date
import pandas as pd
%matplotlib inline
import matplotlib.pyplot as plt
import numpy as np


import chart_studio.plotly as py
import plotly.offline as pyoff
import plotly.graph_objects as go

#import Keras
import keras
from keras.layers import Dense
from keras.models import Sequential
from keras.optimizers import Adam 
from keras.callbacks import EarlyStopping
from keras.utils import np_utils
from keras.layers import LSTM
from sklearn.model_selection import KFold, cross_val_score, train_test_split
```

### Data Sets

LINK

For this anaylsis, I had prepared the data within R studio as well as performed an exploratory analysis to better understand the video game market throughout the past and to practice using both Python and R within one single project.  

### Data Preparation
```r
# Import libraries
library(tidyverse)
library(directlabels)

# Import Data
vgsales <- read.csv('vgsales.csv')
head(game_schedule, 10)
str(game_schedule)

# Check for null values
apply(vgsales, 2, function(x) any(is.null(x)))

# Check for empty values
apply(vgsales, 2, function(x) any(is.na(x)))

# Convert Year into numeric
vgsales$Year <- as.numeric(vgsales$Year)

# Convert Genre into characters
vgsales$Genre <- as.character(vgsales$Genre) 

```

### Exploration and Data Analysis
```r
# North American Sales
vgsales %>%
  group_by(Genre) %>%
  summarise(mean_score = mean(NA_Sales)) %>%
  arrange(desc(mean_score)) %>%
  head(., n=10) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Genre, -mean_score), y=mean_score, fill=Genre), stat='identity') +
  geom_label(aes(x=reorder(Genre, -mean_score), y=mean_score, label=round(mean_score, digits = 2))) +
  guides(fill = FALSE) +
  labs(title = "Top 10 Genres by Mean North American Sales", x="Genre", y='Mean Sales') +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
  
  # Game Count per Genre
vgsales %>%
  group_by(Genre) %>%
  summarise(total_games = n()) %>%
  ggplot() +
  geom_point(aes(x=reorder(Genre, -total_games), y=total_games), size=3) +
  geom_segment(aes(x=reorder(Genre, -total_games),xend=reorder(Genre, -total_games),y=0,yend=total_games)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1)) +
  labs(title = "Lollipop Chart of Genres by Total Game Count", x="Genre", y='Game Count')

# Game Count per Platform
vgsales %>%
  group_by(Platform) %>%
  summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  head(., n=10) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Platform, -counts), y=counts, fill=Platform), stat='identity') +
  geom_label(aes(x=reorder(Platform, -counts), y=counts, label=counts)) +
  guides(fill = FALSE) +
  labs(title = "Top 10 Platforms by Number of Games", x="Platforms", y='Total Number of Games') +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

```

![Genre Global Sales](https://github.com/gabriel-valenzuela/Valenzuela_Gabriel_DSC630/blob/master/Mean%20Sales.png)

![Number of Games Per Platform](https://github.com/gabriel-valenzuela/Valenzuela_Gabriel_DSC630/blob/master/PlatformsGames.png)

Since I had done the analysis for multiple locaions as well as globally, I thoughout it would best to show the methodology for at least one of the locations, North America. 

### Feature Selection
```python

create a new dataframe to model the difference
df_diff = df_vgsales_NA.copy()

#add previous sales to the next row
df_diff['prev_sales'] = df_diff['NA_Sales'].shift(1)

#drop the null values and calculate the difference
df_diff = df_diff.dropna()
df_diff['diff'] = (df_diff['NA_Sales'] - df_diff['prev_sales'])
df_diff.head(10)

#create dataframe for transformation from time series to supervised
df_supervised = df_diff.drop(['prev_sales'],axis=1)

#adding lags
for inc in range(1,21):
    field_name = 'lag_' + str(inc)
    df_supervised[field_name] = df_supervised['diff'].shift(inc)

#drop null values
df_supervised = df_supervised.dropna().reset_index(drop=True)

# Import statsmodels.formula.api
import statsmodels.formula.api as smf

# Define the regression formula
model = smf.ols(formula='diff ~ lag_9 + lag_1 + lag_8 + lag_10 + lag_11 + lag_14 + lag_15 + lag_16 + lag_7 + lag_17 + lag_18 + lag_12 + lag_2 + lag_5', data=df_supervised)

# Fit the regression
model_fit = model.fit()

# Extract the adjusted r-squared
regression_adj_rsq = model_fit.rsquared_adj
print(regression_adj_rsq)
```

### Modeling
```python
import MinMaxScaler and create a new dataframe for LSTM model
from sklearn.preprocessing import MinMaxScaler
df_model = df_supervised.drop(['NA_Sales','Year'],axis=1)

#split train and test set
train_set, test_set = df_model[0:-11].values, df_model[-11:].values

#apply Min Max Scaler
scaler = MinMaxScaler(feature_range=(-1, 1))
scaler = scaler.fit(train_set)

# reshape training set
train_set = train_set.reshape(train_set.shape[0], train_set.shape[1])
train_set_scaled = scaler.transform(train_set)

# reshape test set
test_set = test_set.reshape(test_set.shape[0], test_set.shape[1])
test_set_scaled = scaler.transform(test_set)

# Build the LSTM model

X_train, y_train = train_set_scaled[:, 1:], train_set_scaled[:, 0:1]
X_train = X_train.reshape(X_train.shape[0], 1, X_train.shape[1])
X_test, y_test = test_set_scaled[:, 1:], test_set_scaled[:, 0:1]
X_test = X_test.reshape(X_test.shape[0], 1, X_test.shape[1])

# fit LSTM model
model = Sequential()
model.add(LSTM(4, batch_input_shape=(1, X_train.shape[1], X_train.shape[2]), stateful=True))
model.add(Dense(1))
model.compile(loss='mean_squared_error', optimizer='adam')
model.fit(X_train, y_train, nb_epoch=100, batch_size=1, verbose=1, shuffle=False)

#reshape y_pred
y_pred = y_pred.reshape(y_pred.shape[0], 1, y_pred.shape[1])

#rebuild test set for inverse transform
pred_test_set = []
for index in range(0,len(y_pred)):
    print(np.concatenate([y_pred[index],X_test[index]],axis=1))
    pred_test_set.append(np.concatenate([y_pred[index],X_test[index]],axis=1))

#create dataframe that shows the predicted sales
result_list = []
sales_dates = list(df_vgsales_NA[-13:].Year)
act_sales = list(df_vgsales_NA[-13:].NA_Sales)
for index in range(0,len(pred_test_set_inverted)):
    result_dict = {}
    result_dict['pred_value'] = int(pred_test_set_inverted[index][0] + act_sales[index])
    result_dict['Year'] = sales_dates[index+1]
    result_list.append(result_dict)
df_result = pd.DataFrame(result_list)

#merge with actual sales dataframe
df_sales_pred = pd.merge(df_vgsales_NA,df_result,on='Year',how='left')#plot actual and predicted
plot_data = [
    go.Scatter(
        x=df_sales_pred['Year'],
        y=df_sales_pred['NA_Sales'],
        name='actual'
    ),
        go.Scatter(
        x=df_sales_pred['Year'],
        y=df_sales_pred['pred_value'],
        name='predicted'
    )
    
]
plot_layout = go.Layout(
        title='Sales Prediction - North America'
    )
fig = go.Figure(data=plot_data, layout=plot_layout)
pyoff.iplot(fig)

```
North American Sales Prediction Result
<br>
![NA Sales Prediction](https://github.com/gabriel-valenzuela/Valenzuela_Gabriel_DSC630/blob/master/NA_Sales.png)

