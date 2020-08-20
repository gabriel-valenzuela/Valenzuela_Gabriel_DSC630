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

# European Sales
vgsales %>%
  group_by(Genre) %>%
  summarise(mean_score = mean(EU_Sales)) %>%
  arrange(desc(mean_score)) %>%
  head(., n=10) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Genre, -mean_score), y=mean_score, fill=Genre), stat='identity') +
  geom_label(aes(x=reorder(Genre, -mean_score), y=mean_score, label=round(mean_score, digits = 2))) +
  guides(fill = FALSE) +
  labs(title = "Top 10 Genres by Mean European Sales", x="Genre", y='Mean Sales') +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# Japan Sales
vgsales %>%
  group_by(Genre) %>%
  summarise(mean_score = mean(JP_Sales)) %>%
  arrange(desc(mean_score)) %>%
  head(., n=10) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Genre, -mean_score), y=mean_score, fill=Genre), stat='identity') +
  geom_label(aes(x=reorder(Genre, -mean_score), y=mean_score, label=round(mean_score, digits = 2))) +
  guides(fill = FALSE) +
  labs(title = "Top 10 Genres by Mean Japan Sales", x="Genre", y='Mean Sales') +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# Other Sales
vgsales %>%
  group_by(Genre) %>%
  summarise(mean_score = mean(Other_Sales)) %>%
  arrange(desc(mean_score)) %>%
  head(., n=10) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Genre, -mean_score), y=mean_score, fill=Genre), stat='identity') +
  geom_label(aes(x=reorder(Genre, -mean_score), y=mean_score, label=round(mean_score, digits = 2))) +
  guides(fill = FALSE) +
  labs(title = "Top 10 Genres by Mean Other Sales", x="Genre", y='Mean Sales') +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

# Global Sales
vgsales %>%
  group_by(Genre) %>%
  summarise(mean_score = mean(Global_Sales)) %>%
  arrange(desc(mean_score)) %>%
  head(., n=10) %>%
  ggplot() +
  geom_bar(aes(x=reorder(Genre, -mean_score), y=mean_score, fill=Genre), stat='identity') +
  geom_label(aes(x=reorder(Genre, -mean_score), y=mean_score, label=round(mean_score, digits = 2))) +
  guides(fill = FALSE) +
  labs(title = "Top 10 Genres by Mean Global Sales", x="Genre", y='Mean Sales') +
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

# # Game Count Across Platforms throughout the years
# vgsales %>%
#   #subset(., Year > 2000 & Year <= 2016) %>%
#   group_by(Genre, Year) %>%
#   summarise(total_games = n()) %>%
#   subset(., total_games > 10) %>%
#   ggplot() +
#   geom_line(aes(x=Year, y=total_games, group=Genre, col=Genre)) +
#   geom_point(aes(x=Year, y=total_games, group=Genre, col=Genre)) +
#   geom_dl(aes(x=Year, y=total_games, label = Genre), method=list('first.bumpup', cex=0.5)) +
#   guides(col=FALSE) +
#   #scale_x_continuous(breaks = c(2000:2016), labels=factor(2000:2016), limits = c(2000,2016)) + 
#   labs(title = "Number of Games Across Genres Throughout the Years", x="Year", y='Game Count')


# Convert data frame to CSV for modeling in R
write.csv(vgsales,"C:\\Users\\Gabe\\Documents\\Bellevue University\\Predictive Analytics\\Final Project\\VideoGameModeling.csv", row.names = FALSE)

