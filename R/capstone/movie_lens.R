
#Create train and validation sets
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(stringr)
library(matrixStats)
library(tidyr)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()

#if(!file.exists("ml-10M100K")){
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
#}

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#number of rows and cols in edx
nrow(edx)
ncol(edx)

#number of rows and cols in edx
nrow(validation)
ncol(validation)


#Number of zeros as ratings:
 edx %>% filter(rating == 0) %>% dplyr::count()


#Number of 3's as rating:
edx %>% filter(rating == 3) %>% dplyr::count()


#Number of different movies:

length(unique(edx$movieId))

#Number of different user:
length(unique(edx$userId))


#Number movie ratings in Drama, Comedy, Thriller, Romance in edx dataset:

#filter by drama
drama <- edx %>% filter(str_detect(genres, pattern = "Drama"))

#filter by comedy
comedy <- edx %>% filter(str_detect(genres, pattern = "Comedy"))

#filter by thriller
thriller <- edx %>% filter(str_detect(genres, pattern = "Thriller"))

#filter by romance
romance <- edx %>% filter(str_detect(genres, pattern = "Romance"))

#create a data frame to show number of ratings in each genre filtered above
data.frame("Genre" = c("Drama", "Comedy", "Thriller", "Romance"), "n" = c(nrow(drama),nrow(comedy),nrow(thriller),nrow(romance)))


#Highest rated movies:
#group by movieId to summarize highest rated movie
edx %>% group_by(movieId) %>% summarise(n=n(), title= title[1]) %>% arrange(desc(n))


#Greatest number of ratings:

#group by rating to summarize which rating are rated more
edx %>% group_by(rating) %>% summarise(n=n()) %>% arrange(desc(n)) %>%ggplot(aes(x = rating, y = n)) +
	geom_line()


## Model Fitting

set.seed(755, sample.kind="Rounding")
test_indx <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_indx,]
temp_test_set <- edx[test_indx,]


# ensure userId and movieId in test set are also in train set
test_set <- temp_test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed_test <- anti_join(temp_test_set, test_set)

# add back the removed rows from temp to train set
train_set <- rbind(train_set, removed_test)



#Number of rows in test and train set respectively:

nrow(test_set)

nrow(train_set)


### First Model

#using movie avg to predict rating
mu_hat <- mean(train_set$rating)
just_avg <- RMSE(mu_hat, test_set$rating)
just_avg


### Movie Effect

mu <-  mean(train_set$rating)

#group by movieId to find movie effect
movie_avgs <- train_set %>%
              group_by(movieId) %>%
              summarise(b_i =  mean(rating - mu))

#on the test set, calculate rating using movie effect
pred_with_movie_effect <-  test_set %>%
                            left_join(movie_avgs,by='movieId') %>%
                            mutate(pred = mu + b_i)%>%
                            pull(pred)


#RMSE when bias
movie_effect <- RMSE(pred_with_movie_effect, test_set$rating)
movie_effect



#We can also visualize the movie effect:
movie_avgs %>% ggplot(aes(b_i))+geom_histogram(bins=10, color=I("black"))


### User Effect
mu <-  mean(train_set$rating)

#group by user id and calculate user effect b_u
user_avgs <- train_set %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarise(b_u =  mean(rating - mu -b_i))

#on the test set, calculate rating using user effect
pred_with_user_effect <-  test_set %>%
          left_join(movie_avgs,by='movieId') %>%
          left_join(user_avgs,by='userId') %>%
          mutate(pred = mu + b_i + b_u) %>% pull(pred)


#RMSE when bias
user_effect <-RMSE(pred_with_user_effect, test_set$rating)
user_effect


#We can also check visually how user to user variability looks like:

user_avgs %>% ggplot(aes(b_u))+geom_histogram(color=I("black"))


### Genre Effect

#visualize genre to genre variability, filtering by genres have more than 50000 ratings
train_set %>% group_by(genres) %>%
              summarize(n_of_ratings =  n()) %>%
              filter(n_of_ratings > 50000) %>%
              left_join(train_set , by="genres") %>%
              group_by(genres) %>%
              summarise(n = n(),avg_rating = mean(rating), se=sd(rating)/sqrt(n)) %>%
              ggplot(aes(x = genres, y = avg_rating)) +
              geom_errorbar(aes(ymin = avg_rating-2*se, ymax = avg_rating+2*se))+theme(axis.text  = element_text(angle = 90))


mu <-  mean(train_set$rating)

#ensure genres in test set are also in train set
test_genres <- semi_join(test_set,train_set, by="genres")

# group by genres to calculate genres effect
genre_avgs <- train_set %>%
                         left_join(movie_avgs, by='movieId') %>%
                         left_join(user_avgs, by='userId') %>%
                         group_by(genres) %>%
                         summarise(b_g = mean(rating - mu - b_i - b_u))

#on the test set, calculate rating using genre effect
pred_with_genre_effect <-  test_genres %>%
                left_join(movie_avgs,by='movieId') %>%
                left_join(user_avgs,by='userId') %>%
                left_join(genre_avgs, by="genres") %>%
                mutate(pred = mu + b_i + b_u + b_g) %>%
                pull(pred)



#RMSE when combined genre bias#
genre_effect <-RMSE(pred_with_genre_effect, test_set$rating)

genre_effect



### Regularization

# tuning param lambda from 3 to 6
lambdas <- seq(3, 6, 0.25)

rmses <- sapply(lambdas, function(l){

  mu <-  mean(train_set$rating)

  #penalize movie effect
  movie_reg_avgs <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))

  #penalize user effect
  user_reg_avgs <- train_set %>%
    left_join(movie_reg_avgs, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))

  #penalize genre effect
  genre_reg_avgs <- train_set %>%
    left_join(movie_reg_avgs, by='movieId') %>%
    left_join(user_reg_avgs, by='userId') %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - mu - b_i - b_u)/(n()+l))

  #predict with regularized movie, user and genre effect on the test set
  predicted_ratings <- test_set %>%
    left_join(movie_reg_avgs, by='movieId') %>%
    left_join(user_reg_avgs, by='userId') %>%
    left_join(genre_reg_avgs, by='genres') %>%
    mutate(pred = mu + b_i + b_u+b_g) %>%
    pull(pred)

  RMSE(predicted_ratings, test_set$rating)
})
# plot lambda vs rmse
qplot(lambdas, rmses)



#lambda that minimizes RMSE:
lambdas[which.min(rmses)]

#min rmse
reg_movie_user_genre_effect <- min(rmses)
reg_movie_user_genre_effect



# Result
data.frame("Method"=c('Just Avg', 'Movie Effect', 'User Effect', 'Genre Effect', 'Regularized Movie + User + Genres Effect'), "RMSE"= c(just_avg, movie_effect, user_effect, genre_effect,reg_movie_user_genre_effect ))


### Final RMSE on validation set for lambda = 4.75

lambda <- 4.75
mu_edx <-  mean(edx$rating)
movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda))

user_reg_avgs <- edx %>% left_join(movie_reg_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu_edx - b_i)/(n()+lambda))

genre_reg_avgs <-    edx %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - mu_edx - b_i - b_u)/(n()+lambda))

predicted_ratings <- validation %>%
  left_join(movie_reg_avgs, by='movieId') %>%
  left_join(user_reg_avgs, by='userId') %>%
  left_join(genre_reg_avgs, by='genres') %>%
  mutate(pred = mu_edx + b_i + b_u+b_g) %>%
  pull(pred)
RMSE(predicted_ratings, validation$rating)



