# This code snippet is to get data using the OMDb API.

# Importing the libraries
library(httr)
library(curl)
library(jsonlite)
library(tidyverse)

API_KEY = "1ee0b287"

# Function to fetch OMDb data from title
fetch_omdb_data <- function(title, API_KEY) {
  url <-
    paste0("http://www.omdbapi.com/?t=",
           gsub(" ", "+", title),
           "&apikey=",
           API_KEY)
  json_result <-
    url %>% curl() %>% readLines(warn = FALSE) %>% fromJSON()
  movie_data <- list()
  # attribute_list <- c("Title", "Year", "Plot", "Rated", "Director", "Awards", "Metascore", "imdbRating", "BoxOffice", "Production")
  movie_data$Title <- json_result$Title
  movie_data$imdbID <- json_result$imdbID
  movie_data$Year <- as.integer(json_result$Year)
  movie_data$Plot <- json_result$Plot
  movie_data$Rated <- json_result$Rated
  movie_data$Director <- json_result$Director
  movie_data$Awards <- json_result$Awards
  movie_data$Metascore <- json_result$Metascore
  movie_data$imdbRating <- json_result$imdbRating
  if (nrow(json_result$Ratings) == 3) {
    movie_data$Tomatoes <- json_result$Ratings[[2]][[2]]
  }
  else{
    movie_data$Tomatoes <- NA
  }
  movie_data$Boxoffice <- fetch_box_office(movie_data$imdbID)
  movie_data$Production <- json_result$Production
  movie_data$Actors <- json_result$Actors
  movie_data$Genres <- json_result$Genre
  #actors <- str_trim(str_split(json_result$Actors, ",")[[1]])
  #genres <- str_trim(str_split(json_result$Genre, ",")[[1]])
  movie_data$Runtime <-
    as.integer(str_extract(json_result$Runtime, "\\d+"))
  return(movie_data)
}

# Function to get box-office collection from IMDb.
fetch_box_office <- function(imdbID) {
  url <- paste0("https://www.imdb.com/title/", imdbID)
  if (url %>% read_html() %>% html_nodes(".txt-block") %>% str_detect("Cumulative") %>% any()) {
    url %>% read_html() %>% html_nodes(".txt-block") %>% .[13] %>% html_text() %>% str_extract("[\\d,]+") %>% gsub(pattern = ",", replacement = "") -> box_office
  }
  else{
    box_office <- NA
  }
  return(box_office)
}

# This function was just for some debugging. Not really important.
fetch_omdb_data2 <- function(title, API_KEY) {
  url <-
    paste0("http://www.omdbapi.com/?t=",
           gsub(" ", "+", title),
           "&apikey=",
           API_KEY)
  json_result <- url %>% curl() %>% readLines() %>% fromJSON()
  return(json_result)
}

ls <- c()
index <- 1
movie_errors <- c()

# Fetching info for all the movies in "list_of_movies.txt"
for (movie_name in readLines(file("list_of_movies.txt"))) {
  flag = FALSE
  tryCatch({
    print(paste(index, movie_name))
    x <- fetch_omdb_data(movie_name, API_KEY = API_KEY)
    if (length(x) == 15) {
      ls <- c(ls, x)
      index <- index + 1
    }
    flag = TRUE
  }, error = function(err) {
    print(paste("Error generated for", movie_name))
    movie_errors <- c(movie_errors, movie_name)
  })
  Sys.sleep(runif(1, 1, 5))
  if (!flag)
  {
    print("Trying again...")
    tryCatch({
      print(paste(index, movie_name))
      x <- fetch_omdb_data(movie_name, API_KEY = API_KEY)
      if (length(x) == 15) {
        ls <- c(ls, x)
        index <- index + 1
      }
      flag = TRUE
    }, error = function(err) {
      print(paste("Error generated for", movie_name))
      movie_errors <- c(movie_errors, movie_name)
    })
    Sys.sleep(runif(1, 1, 5))
  }
}

# Tidying up the data and writing to a CSV file
df <-
  as.tibble(matrix(unlist(ls), nrow = length(ls) / 15, byrow = T), stringsAsFactors =
              FALSE)
colnames(df) <-
  c(
    "Title",
    "imdbID",
    "Year",
    "Plot",
    "Rated",
    "Director",
    "Awards",
    "Metascore",
    "IMDB",
    "Tomatoes",
    "Boxoffice",
    "Production",
    "Actors",
    "Genres",
    "Runtime"
  )

df <-
  mutate(
    df,
    Year = as.integer(Year),
    Metascore = as.integer(Metascore),
    IMDB = as.numeric(IMDB),
    Boxoffice = as.integer(Boxoffice),
    Tomatoes = as.integer(str_extract(Tomatoes, "\\d+"))
  )
df[df == "N/A"] <- NA

write_csv(df, "movie_data_new.csv")
