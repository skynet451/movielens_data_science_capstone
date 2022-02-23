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

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
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

# No. of rows & coumns in EDX set
nrow(edx)
ncol(edx)

# No. of Zero & Three as rating in EDX set
sum(edx$rating == 0)
sum(edx$rating == 3)

# Different Movies in EDX set
n_distinct(edx$movieId)

# Different Users in EDX Set
n_distinct(edx$userId)

# Movies ratings to following genres in EDX Set
drama <- edx %>% filter(str_detect(genres,"Drama"))
comedy <- edx %>% filter(str_detect(genres,"Comedy"))
thriller <- edx %>% filter(str_detect(genres,"Thriller"))
romance <- edx %>% filter(str_detect(genres,"Romance"))
nrow(drama)
nrow(comedy)
nrow(thriller)
nrow(romance)

# Movie with greatest number of rating in EDX Set
edx %>% group_by(title) %>% summarise(number =n()) %>% arrange(desc(number))

# Five most given rating from most to least order
head(sort(-table(edx$rating)),5)

## Average rating model ##

# Compute the mean rating
 mean_rating <- mean(edx$rating)
 mean_rating

# simple prediction
s_rmse <- RMSE(validation$rating, mean_rating)
s_rmse

# Check results
# Save prediction in data frame
rmse_results <- data_frame(method = "Average rating model", RMSE = s_rmse)
rmse_results %>% knitr::kable()

## Movie effect model ##

# Simple model taking into account the movie effect b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mean_rating))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"),
                     ylab = "Number of movies", main = "Number of movies with the computed b_i")


# rmse results 
predicted_ratings <- mean_rating +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_s_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = model_s_rmse ))
# Check results
rmse_results %>% knitr::kable()

## Regularized effect model ##

# lambda is a tuning parameter
lambdas <- seq(0, 10, 0.25)


# Find b_i & b_u
rmses <- sapply(lambdas, function(l){
  
  mean_rating <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mean_rating)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mean_rating)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mean_rating + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})


# Funding the optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]
lambda

# save results                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized effect model",  
                                     RMSE = min(rmses)))

# Check result
rmse_results %>% knitr::kable()

#### Results ####                                                            
# RMSE results overview                                                          
rmse_results %>% knitr::kable()

