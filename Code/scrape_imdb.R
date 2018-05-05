# This code is to scrape the list of movie titles from IMDb.

# Importing the libraries
library(rvest)

# Generates IMDb URL
generate_imdb_url <- function(page) {
  url <-
    paste0(
      "https://www.imdb.com/list/ls003495084/?sort=list_order,asc&st_dt=&mode=detail&page=",
      page
    )
  return(url)
}

movies <- c()

for (i in 1:11) {
  i %>% generate_imdb_url() %>% read_html() %>% html_nodes(".mode-detail") %>% html_node("img") %>% html_attr("alt") -> movie_list
  movies <- c(movies, movie_list)
}

# Writing all the movie titles to a .txt file.
file_conn <- file("list_of_movies.txt")
writeLines(movies, file_conn)
close(file_conn)