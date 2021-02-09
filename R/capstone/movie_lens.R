
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

#number of rows and cols
nrow(edx)
ncol(edx)


#Number of zeros as ratings:
edx %>% filter(rating == 0) %>% dplyr::count()
edx %>% filter(rating == 3) %>% dplyr::count()



#Number of different movies:
length(unique(edx$movieId))


#Number of different user:
length(unique(edx$userId))


#Number movie ratings in Drama, Comedy, Thriller, ROmance in edx dataset:

drama <- edx %>% filter(str_detect(genres, pattern = "Drama")) 
comedy <- edx %>% filter(str_detect(genres, pattern = "Comedy")) 
thriller <- edx %>% filter(str_detect(genres, pattern = "Thriller")) 
romance <- edx %>% filter(str_detect(genres, pattern = "Romance")) 
data.frame("Genre" = c("Drama", "Comedy", "Thriller", "Romance"), "n" = c(nrow(drama),nrow(comedy),nrow(thriller),nrow(romance)))


#Highest rated movies:
edx %>% group_by(movieId) %>% summarise(n=n(), title= title[1]) %>% arrange(desc(n))


#Greatest number of ratings:
edx %>% group_by(rating) %>% summarise(n=n()) %>% arrange(desc(n)) %>%ggplot(aes(x = rating, y = n)) +
	geom_line()


### First Model - just avg
mu_hat <- mean(edx$rating)
just_avg <- RMSE(mu_hat, validation$rating)
just_avg


### Movie Effect
movie_avgs <- edx %>% group_by(movieId) %>% summarise(b_i =  mean(rating - mu_hat))
bi <-  validation %>% left_join(movie_avgs,by='movieId') %>% .$b_i
pred_with_movie_effect <-  mu_hat + bi

#RMSE when bias
movie_effect <- RMSE(pred_with_movie_effect, validation$rating)
movie_effect

# plot movie effect
movie_avgs %>% ggplot(aes(b_i))+geom_histogram(bins=10, color=I("black"))


### User Effect
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarise(b_u =  mean(rating - mu_hat -b_i))
bu <-  validation %>%
          left_join(movie_avgs,by='movieId') %>%
          left_join(user_avgs,by='userId') %>% .$b_u
pred_with_user_effect <-  mu_hat + bi + bu

#RMSE when bias
user_effect <-RMSE(pred_with_user_effect, validation$rating)
user_effect

#Plot user effect
user_avgs %>% ggplot(aes(b_u))+geom_histogram(bins=10, color=I("black"))


### Genre Effect
#separate rows by genre in edx and validation set
edx_genres <- edx %>% separate_rows(genres)
validation_genres <- validation %>% separate_rows(genres)

#genre avg
genre_avgs <- edx_genres %>% 
                         left_join(movie_avgs, by='movieId') %>%
                         left_join(user_avgs, by='userId') %>% 
                         group_by(genres) %>% summarise(b_g = mean(rating - mu_hat - b_i - b_u))

# Multiple genres apply to a movie, so combine the genre effects for the movie 
bg_combined <-  validation_genres %>% 
          left_join(movie_avgs,by='movieId') %>%
          left_join(user_avgs,by='userId') %>%
          left_join(genre_avgs, by="genres") %>%
          mutate(b_g= ifelse(is.na(b_g),0,b_g)) %>%
          group_by(movieId,userId) %>% 
          summarise(b_g_combined = sum(b_g)) %>% pull(b_g_combined)


pred_with_genre_effect <-  mu_hat + bi + bu + bg_combined

# RMSE when combined genre bias
genre_effect <-RMSE(pred_with_genre_effect, validation$rating)

#Plot genre effect
genre_avgs %>% ggplot(aes(b_g))+geom_histogram(bins=10, color=I("black"))


### Regularization - movie and user effect

#tune with lambda parameter
lambdas <- seq(0, 10, 0.75)

rmses <- sapply(lambdas, function(l){
    
    movie_reg_avgs <- edx %>%
                      group_by(movieId) %>%
                      summarize(b_i = sum(rating - mu_hat)/(n()+l), n_i = n())

    user_reg_avgs <- edx %>% left_join(movie_reg_avgs, by="movieId") %>%
                      group_by(userId) %>%
                      summarize(b_u = sum(rating - mu_hat - b_i)/(n()+l), n_i = n())
    
    predicted_ratings <- validation %>%
                                left_join(movie_reg_avgs, by='movieId') %>%
                                left_join(user_reg_avgs, by='userId') %>%
                                mutate(pred = mu_hat + b_i + b_u) %>%
                                pull(pred)
    
   RMSE(predicted_ratings, validation$rating)
})

#plot regularization effect for lambdas
qplot(lambdas, rmses)

#lambda that minimizes rmse
lambdas[which.min(rmses)]

#min rmse
reg_movie_user_effect <- min(rmses)
reg_movie_user_effect



# Result
data.frame("Method"=c('Just Avg', 'Movie Effect', 'User Effect', 'Genre Effect', 'Regularized Movie + User Effect'), "RMSE"= c(just_avg, movie_effect, user_effect, genre_effect,reg_movie_user_effect ))

