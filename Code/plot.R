# This code snippet is to visualize the data.

# Importing the libraries
library(tidyverse)
library(ggthemes)

# Reading the dataset
df <- read_csv("movie_data_new.csv")

# Getting the data frame into the desired format
df <- mutate(df, Boxoffice = as.numeric(Boxoffice))
df %>% filter(str_detect(string = Awards, pattern = "Won \\d+ Oscars")) %>% mutate(Oscars = as.integer(str_extract(string = Awards, pattern = "\\d+"))) -> df_oscars
ls <- list()

# File "output.txt" contains all the movie titles with the titles from the same franchise being on the same line separated by a tab
sequels <- read_lines("output.txt")

# Aggregate data about franchises
for (line in sequels) {
  if (sum(str_count(line, "\t")) >= 1) {
    df %>% filter(Title %in% strsplit(line, "\t")[[1]]) %>% select(Boxoffice) %>% sum() -> total_gross
    df_oscars %>% filter(Title %in% strsplit(line, "\t")[[1]]) %>% select(Oscars) -> tmp
    if (nrow(tmp)) {
      oscar_count <- sum(tmp$Oscars)
    }
    else{
      oscar_count <- 0
    }
    df %>% filter(Title %in% strsplit(line, "\t")[[1]]) -> tmp
    mean_gross <- mean(tmp$Boxoffice, na.rm = TRUE)
    ls$Title <- c(ls$Title, strsplit(line, "\t")[[1]][1])
    ls$TotalGross <- c(ls$TotalGross, total_gross)
    ls$Diff <-
      ls$IMDB <- c(ls$IMDB, mean(tmp$IMDB, na.rm = TRUE))
    ls$MeanGross <- c(ls$MeanGross, mean_gross)
    ls$Oscars <- c(ls$Oscars, oscar_count)
  }
}


# Plotting the franchise data
# Apologies for using the same variable name df for a different data frame

# Assigning proper names to movie franchises
# Plotting Top grossing franchises by total gross
df <- as.data.frame(ls)
df %>% mutate(Title =  sub('(?<=\\:).*$', '', Title, perl = TRUE)) %>% mutate(Title = sub(':|\\d', '', Title, perl = TRUE)) %>% filter(Title != "Transformers") -> df
df %>% arrange(-TotalGross) %>% head(20) %>% ggplot(aes(reorder(Title, TotalGross), TotalGross)) + geom_bar(stat = "identity") + coord_flip() + labs(
  title = "Top Grossing Movie Franchises",
  subtitle = "Ordered by Total Worldwide Gross",
  x = "Movie Franchise",
  y = "Total Gross"
) + theme_tufte()

# Plotting top grossing franchises by mean gross
df %>% arrange(-MeanGross) %>% head(20) %>% ggplot(aes(reorder(Title, MeanGross), MeanGross)) + geom_bar(stat = "identity") + coord_flip() + labs(
  title = "Top Grossing Movie Franchises",
  subtitle = "Ordered by Average Movie Gross",
  x = "Movie Franchise",
  y = "Mean Gross"
) + theme_tufte()
df %>% arrange(-Oscars) %>% head(20) %>% ggplot(aes(reorder(Title, Oscars), Oscars)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Number of Oscars", x = "Movie Franchise", y = "Oscar Count") + theme_tufte()

# Plotting movie franchises by Oscar count
df %>% arrange(-Oscars) %>% head(20) %>% ggplot(aes(x = reorder(Title, Oscars), y =
                                                      Oscars)) +
  geom_point(size = 3) +
  geom_segment(aes(
    x = Title,
    xend = Title,
    y = 0,
    yend = Oscars
  )) +
  labs(title = "Number of Oscars", x = "Movie Franchise") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) + theme_tufte() + coord_flip()

# Plotting movie franchises by average IMDb rating of the franchise
df %>% arrange(-IMDB) %>% head(20) %>% ggplot(aes(x = reorder(Title, IMDB), y =
                                                    IMDB)) +
  geom_point(size = 3) +
  geom_segment(aes(
    x = Title,
    xend = Title,
    y = 0,
    yend = IMDB
  )) +
  labs(
    title = "Most Universally Loved Movie Franchises",
    subtitle = "Based on IMDb Rating",
    x = "Movie Franchise",
    y = "IMDb Rating"
  ) +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) + theme_tufte() + coord_flip()

# Reading the data frame again. Shouldn't have used the same variable name df again.
df <- read_csv("movie_data_new.csv")

# Plotting directors by movie count in these franchises
df %>% group_by(Director) %>% summarise(count = n()) %>% arrange(-count) %>%
  head(20) %>% ggplot(aes(x = reorder(Director, count), y = count)) +
  geom_point(col = "tomato2", size = 3) +   # Draw points
  geom_segment(
    aes(
      x = Director,
      xend = Director,
      y = min(count),
      yend = max(count)
    ),
    linetype = "dashed",
    size = 0.1
  ) +   # Draw dashed lines
  labs(title = "Directors with Most Sequels",
       x = "Director",
       y = "Count") +
  coord_flip() + theme_tufte()

# Plotting production house by movie count
df %>% na.omit(Production) %>% group_by(Production) %>% summarise(count = n()) %>% arrange(-count) %>% head(20) %>% ggplot(aes(x =
                                                                                                                                 reorder(Production, count), y = count)) +
  geom_point(col = "tomato2", size = 3) +   # Draw points
  geom_segment(
    aes(
      x = Production,
      xend = Production,
      y = min(count),
      yend = max(count)
    ),
    linetype = "dashed",
    size = 0.1
  ) +   # Draw dashed lines
  labs(title = "Production Houses with Most Sequels",
       x = "Production House",
       y = "Count") +
  coord_flip() + theme_tufte()

strsplit(df$Actors, ", ") %>% rbind() %>% apply(1, unlist) %>% data.frame() -> tmp
colnames(tmp) <- "Actor"
tmp %>% group_by(Actor) %>% summarise(count = n()) %>% arrange(-count) %>% head(20) %>% ggplot(aes(reorder(Actor, count), count)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Actors with Most Number of Sequels", x = "Actors", y = "Movie Count") + theme_tufte()

# Everything starting from here was added after the in-class presentation. This is the "Future Work" segment from the presentation.

# Why do I keep using the same variable names?
# Really sorry!
ls <- list()
df <- read_csv("movie_data_new.csv")

# Just considering franchises with two movies
# Calculating the change in IMDb rating between the two movies (rating of sequel - rating of original)
for (line in sequels) {
  if (sum(str_count(line, "\t")) == 1) {
    df %>% filter(Title %in% strsplit(line, "\t")[[1]]) -> tmp
    ls$Title <- c(ls$Title, strsplit(line, "\t")[[1]][1])
    ls$Diff <- c(ls$Diff, tmp$IMDB[2] - tmp$IMDB[1])
  }
}

# 10 franchises with highest positive difference = df1
# 10 franchises with highest negative difference = df2
# df = df1 + df2 (merging row-wise)
as.data.frame(ls) %>% arrange(-Diff) %>% mutate(Title =  sub('(?<=\\:).*$', '', Title, perl = TRUE)) %>% mutate(Title = sub(':|\\d', '', Title, perl = TRUE)) %>% head(10) -> df1
as.data.frame(ls) %>% arrange(Diff) %>% mutate(Title =  sub('(?<=\\:).*$', '', Title, perl = TRUE)) %>% mutate(Title = sub(':|\\d', '', Title, perl = TRUE)) %>% head(10) -> df2
df <- rbind(df1, df2)

# Some formatting in the Title
# New column DiffType to suggest negative and positive differences
df %>% mutate(Title = sub("[II]+", "", Title, perl = TRUE)) -> df
df$DiffType <- ifelse(df$Diff < 0, "neg", "pos")

# Finally it's time to plot
ggplot(df, aes(
  x = reorder(Title, Diff),
  y = Diff,
  label = Diff
)) +
  geom_bar(stat = 'identity', aes(fill = DiffType), width = .5)  +
  scale_fill_manual(
    name = "Original vs Sequel",
    labels = c("Sequel Is Better", "Original Is Better"),
    values = c("neg" = "#00ba38", "pos" = "#f8766d")
  ) +
  labs(title = "Franchises with Extreme Change in Quality", y = "IMDb Rating Change", x = "Movie Franchise") +
  coord_flip() + theme_tufte()

# Histogram of rating differences
# Most sequels are doing worse (as expected)
# The magnitude of positive difference is also generally smaller than the magnitude of negative difference
as.data.frame(ls) %>% mutate(DiffType = ifelse(Diff >= 0, "Positive", "Negative")) %>% ggplot(aes(Diff)) + geom_histogram(aes(fill = DiffType), bins = 40) +
  scale_fill_manual(
    name = "Type of Difference",
    labels = c("Negative", "Positive"),
    values = c("Negative" = "#f8766d", "Positive" = "#00ba38")
  ) + labs(title = "Histogram of IMDb Rating Differences", x = "Difference", y = "Count") + theme_bw()
