## dplyr practice 

library(ggplot2) 
library(dplyr) 

albums = read.csv("data/albums.csv") 


# filtering observations of the dataset based on genre & year of publication 

unique(albums$genre) 
ten_genres = c("Alternative", "Boy Band", "Country", "Folk", "Indie", "Jazz", "K-Pop", 
               "Pop", "Rap", "Rock") 

albums_less_rows = albums %>% 
  filter(genre %in% ten_genres, year_of_pub %in% 2010:2015) 

# creating a smaller dataset with a subset of variables 

albums_less_cols = select(albums, genre:music_maniac_critic) 

# adding columns 

albums_new_cols = mutate(albums, avg_rating = (rolling_stone_critic + mtv_critic + 
                                                   music_maniac_critic)/3) 

# creating a data frame of grouped summaries on 1+ numeric variables by 1+ 
# categorical variables 

genres_num_sales = albums %>% 
  group_by(genre) %>% 
  summarize(mean_sales = mean(num_of_sales), count = n()) 

# creating 1+ visualizations using some form of an updated dataset 

ggplot(data = albums_less_rows, aes(x = genre, color = genre)) + 
  geom_histogram(stat = "count") + 
  labs(title = "Albums Released From 2010-2015:", x = "Genre", y = "Number of Albums") 

indie_and_pop = filter(albums_new_cols, genre %in% c("Indie", "Pop"))
ggplot(data = indie_and_pop, aes(x = year_of_pub, color = genre)) + 
  geom_line(stat = "count") + 
  labs(title = "Number of Indie and Pop Albums Released Over Time:", x = "Year of 
       Publication", y = "Number of Albums") 

