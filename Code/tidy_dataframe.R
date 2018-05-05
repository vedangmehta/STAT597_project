# Importing the libraries
library(tidyverse)

# Fetches words from a string
get_words <- function(s){
  return(strsplit(tolower(s), "[ |\-|:|,]+")[[1]] %>% setdiff(stopwords::stopwords(language = "en")))
}

# Checks if two vectors have any similar elements
check_for_similarity <- function(x, y){
  return(ifelse(length(intersect(x, y)) >= 1, TRUE, FALSE))
}

# Checks whether the movie at index i is a sequel of the movie at index (i - 1) or not
is_sequel <- function(index){
  if(ls$Year[index] < ls$Year[index - 1]){
    return(FALSE)
  }
  title_sim <- check_for_similarity(get_words(ls$Title[index]), get_words(ls$Title[index - 1]))
  actor_sim <- check_for_similarity(get_words(ls$Actors[index]), get_words(ls$Actors[index - 1]))
  director_sim <- (ls$Director[index] == ls$Director[index - 1])
  production_sim <- (ls$Production[index] == ls$Production[index - 1])
  genre_sim <- check_for_similarity(get_words(ls$Genres[index]), get_words(ls$Genres[index - 1]))
  rated_sim <- (ls$Rated[index] == ls$Rated[index - 1])
  if(actor_sim == TRUE & title_sim == TRUE){
    return(TRUE)
  }
  return(ifelse(title_sim + actor_sim + director_sim + production_sim + genre_sim + rated_sim >= 3, TRUE, FALSE))
}

# Reading the dataset
df <- read_csv("movie_data_new.csv") %>% select(Title, Year, Rated, Director, Production, Actors, Genres) %>% na.omit()

ls <- as.list(df)
cur <- c(ls$Title[1])

# Writing movies from the same franchise on the same line separated by a tabspace
for(index in 2:length(ls$Title)){
  if(is_sequel(index)){
    cur <- c(cur, ls$Title[index])
  }
  else{
    write(paste(cur, collapse = "\t"), file = "output.txt", append = TRUE, sep = "\t")
    cur <- c(ls$Title[index])
  }
}