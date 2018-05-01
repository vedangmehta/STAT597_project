df <- read_csv("movie_data_new.csv")

df <- mutate(df, Boxoffice = as.numeric(Boxoffice))
df %>% filter(str_detect(string = Awards, pattern = "Won \\d+ Oscars")) %>% mutate(Oscars = as.integer(str_extract(string = Awards, pattern = "\\d+"))) -> df_oscars
ls <- list()
sequels <- read_lines("output.txt")

for (line in sequels) {
  if (length(grep("\t", line)) >= 1) {
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
    ls$IMDB <- c(ls$IMDB, mean(tmp$IMDB, na.rm = TRUE))
    ls$MeanGross <- c(ls$MeanGross, mean_gross)
    ls$Oscars <- c(ls$Oscars, oscar_count)
  }
}

df <- as.data.frame(ls)
df %>% mutate(Title =  sub('(?<=\\:).*$', '', Title, perl = TRUE)) %>% mutate(Title = sub(':|\\d', '', Title, perl = TRUE)) %>% filter(Title != "Transformers") -> df
df %>% arrange(-TotalGross) %>% head(20) %>% ggplot(aes(reorder(Title, TotalGross), TotalGross)) + geom_bar(stat = "identity") + coord_flip() + labs(
  title = "Top Grossing Movie Franchises",
  subtitle = "Ordered by Total Worldwide Gross",
  x = "Movie Franchise",
  y = "Total Gross"
) + theme_tufte()
df %>% arrange(-MeanGross) %>% head(20) %>% ggplot(aes(reorder(Title, MeanGross), MeanGross)) + geom_bar(stat = "identity") + coord_flip() + labs(
  title = "Top Grossing Movie Franchises",
  subtitle = "Ordered by Average Movie Gross",
  x = "Movie Franchise",
  y = "Mean Gross"
) + theme_tufte()
df %>% arrange(-Oscars) %>% head(20) %>% ggplot(aes(reorder(Title, Oscars), Oscars)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Number of Oscars", x = "Movie Franchise", y = "Oscar Count") + theme_tufte()


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

df %>% arrange(-IMDB) %>% head(20) %>% ggplot(aes(x = reorder(Title, IMDB), y =
                                                    IMDB)) +
  geom_point(size = 3) +
  geom_segment(aes(
    x = Title,
    xend = Title,
    y = 0,
    yend = IMDB
  )) +
  labs(title = "Most Universally Loved Movie Franchises",
       subtitle = "Based on IMDb Rating",
       x = "Movie Franchise",
       y = "IMDb Rating") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) + theme_tufte() + coord_flip()

df <- read_csv("movie_data_new.csv")

df %>% group_by(Director) %>% summarise(count = n()) %>% arrange(-count) %>% 
  head(20) %>% ggplot(aes(x=reorder(Director, count), y=count)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=Director, 
                   xend=Director, 
                   y=min(count), 
                   yend=max(count)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Directors with Most Sequels",
       x = "Director",
       y = "Count"
       ) +  
  coord_flip() + theme_tufte()

df %>% na.omit(Production) %>% group_by(Production) %>% summarise(count = n()) %>% arrange(-count) %>% head(20) %>% ggplot(aes(x=reorder(Production, count), y=count)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=Production, 
                   xend=Production, 
                   y=min(count), 
                   yend=max(count)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Production Houses with Most Sequels",
       x = "Production House",
       y = "Count"
       # subtitle="Make Vs Avg. Mileage", 
  ) +  
  coord_flip() + theme_tufte()

strsplit(df$Actors, ", ") %>% rbind() %>% apply(1, unlist) %>% data.frame() -> tmp
colnames(tmp) <- "Actor"
tmp %>% group_by(Actor) %>% summarise(count = n()) %>% arrange(-count) %>% head(20) %>% ggplot(aes(reorder(Actor, count), count)) + geom_bar(stat = "identity") + coord_flip() + labs(title = "Actors with Most Number of Sequels", x = "Actors", y = "Movie Count") + theme_tufte()

