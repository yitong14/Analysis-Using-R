#### Read the data (NAN) ####
videos <- read.csv('USvideos.csv', stringsAsFactors = F)
View(videos)
videos <- videos[, 1:16]
## omit the na values
videos <- na.omit(videos)
nrow(videos)

#### Pre-process: Sampling && Variable Hadling ####
## sampling (to make the dataset smaller) ##
set.seed(1234)
sampling.index <- sample(1:nrow(videos), nrow(videos)*0.5)
videos <- videos[sampling.index, ]
nrow(videos)

## select the var ##
drop.var <- c(1, 4, 7, 12, 13, 14, 15)
selected.var <- c(2, 3, 5, 6, 8, 9, 10, 11, 16)
videos <- videos[, selected.var]

## variable handling ##
str(videos)
## as.factor (categorical_id)
videos$category_id <- factor(videos$category_id)
## as.numeric (likes, dislikes, comment_count)
videos$likes <- as.numeric(videos$likes)
videos$dislikes <- as.numeric(videos$dislikes)
videos$comment_count <- as.numeric(videos$comment_count)
## omit the na values
videos <- na.omit(videos)
## as.Date (publish_time)
date <- videos$trending_date
#date <- strftime(date, '%Y-%m-%d')
day.of.week <- weekdays(as.Date(date, '%y.%d.%m'))
day.of.week <- factor(day.of.week)
levels(day.of.week) <- c('Tue', 'Sat', 'Sun', 'Wed', 'Thu', 'Fri', 'Mon')
day.of.week <- relevel(day.of.week, ref = 'Mon')
str(day.of.week)
videos$day.of.week <- relevel(day.of.week, ref = 'Mon')

## Output ##
write.csv(videos, 'videos_tredingdate.csv')


#### Read the data ####
videos.df <- read.csv('videos_tredingdate.csv', stringsAsFactors = F)
View(videos.df)
videos.df <- videos.df[, 2:11]


#### Pre-process: TFIDF ####
## variable handling ##
str(videos.df)
## as.factor (categorical_id)
videos.df$category_id <- factor(as.numeric(videos.df$category_id))
levels(videos.df$category_id)

## tfidf
## corpus
library(tm)
corp <- Corpus(VectorSource(videos.df$description))
inspect(corp)
## clean the text data
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeWords, stopwords('english'))
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, stemDocument)
## tfidf
dtm <- DocumentTermMatrix(corp)
tfidf <- weightTfIdf(dtm)
ncol(as.matrix(tfidf))
## reduce term dimensions
tfidf.try <- removeSparseTerms(tfidf, 0.9)
ncol(as.matrix(tfidf.try))
inspect(tfidf.try)
## contruct the dataframe
likes.df <- data.frame(as.matrix(tfidf.try), views = videos.df$views,
                     category = videos.df$category_id, likes = videos.df$likes,
                     day.of.week = videos.df$day.of.week)
write.csv(likes.df, 'likes_trendingdate.csv')


#### Read the data ####
likes.df <- read.csv('likes.csv')


#### Pre-process: log ####
## log
likes.df$category <- factor(likes.df$category)
likes.df$likes <- log(likes.df$likes)
likes.df$views <- log(likes.df$views)
cor(likes.df$views, likes.df$likes)
## hour
date <- videos.df$publish_time
hour <- chartr('T', ' ', date)
hour <- as.POSIXct(hour)
hour <- strftime(hour, '%H')
hour <- factor(hour)
videos.df$hour <- hour

#### Exploratory Analysis ####
## boxplot
library(ggplot2)
cbgray <- c('#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999', '#999999')
ggplot(data=likes.df) + 
  geom_boxplot(aes(x=category, y=likes, group=category, fill = category)) +
  labs(fill='category', xlab='category') + ggtitle('Box Plot') +
  theme(plot.title=element_text(vjust=0.5, hjust=0.5, face='bold'),
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  scale_fill_manual(values = cbgray)
ggplot(data=likes.df) + 
  geom_boxplot(aes(x=day.of.week, y=likes, group=day.of.week, fill = day.of.week)) +
  labs(fill='day.of.week', xlab='day.of.week') + ggtitle('Box Plot') +
  theme(plot.title=element_text(vjust=0.5, hjust=0.5, face='bold')) +
  scale_fill_manual(values = cbgray)
cbgray <- c('#999999', '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999', '#999999',
            '#999999', '#999999', '#999999', '#999999')
ggplot(data = videos.df) + 
  geom_boxplot(aes(x=hour, y=log(likes), group=hour, fill = hour)) +
  labs(fill='hour', xlab='hour') + ggtitle('Box Plot') +
  theme(plot.title=element_text(vjust=0.5, hjust=0.5, face='bold'),
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  scale_fill_manual(values = cbgray)

## scatter plot
ggplot(data = likes.df) +
  geom_point(aes(x = views, y = likes)) +
  xlab("log(views)") +
  ylab("log(likes)")

## heat map
#write.csv(likes.df, 'likes_log.csv')
#likes.df.new <- read.csv('likes_log.csv')
library(corrplot)
words.var <- c(1:28, 31)
cor.data <- likes.df[, words.var]
res <- cor(cor.data)
round(res, 2)
corrplot(res, method = 'color',
         order = 'AOE', tl.col = 'black')
#corrplot(res, method = 'color',
#         order = 'hclust')
cor(likes.df$facebook, likes.df$instagram)
cor(likes.df$facebook, likes.df$twitter)
cor(likes.df$instagram, likes.df$twitter)


#### Partition ####
set.seed(1234)
train.index <- sample(1:nrow(likes.df), nrow(likes.df)*0.5)
train.df <- likes.df[train.index, ]
valid.df <- likes.df[-train.index, ]

#### Multiple Linear Regression ####
likes.lm <- lm(likes ~ ., data = train.df)
options(scipen=999, digits = 3)
summary(likes.lm)

#### Evaluate performance ####
likes.lm.pred <- predict(likes.lm, valid.df)
library(forecast)
accuracy(likes.lm.pred, valid.df$likes)

#### Reduce the number of predictors ####
likes.lm.step <- step(likes.lm, direction = 'backward')
summary(likes.lm.step)
likes.lm.step.pred <- predict(likes.lm.step, valid.df)
accuracy(likes.lm.step.pred, valid.df$likes)