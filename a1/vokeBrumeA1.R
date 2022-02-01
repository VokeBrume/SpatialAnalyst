movies <- read.csv("A1/data_query/matts_movies.csv")

library(dplyr)
library(forcats)

#T1: Number of differentiated Genre
length((levels(as.factor(movies$Genre))))

#Answer: 16


#T2: Genre with most entries
genre_cnt <- fct_count(fct_infreq(movies$Genre))
head(genre_cnt, 1)

#Answer: Drama (321)


#T3: Percentage of movies above 8.5
(nrow(movies %>% filter(My.Rating > 8.5)) / nrow(movies)) * 100

#Answer: 11.60907


#T4: Percentage of movies that are Dramas
(genre_cnt[1,2]/nrow(movies)) * 100

#Answer: 17.33261


#T5: Percentage of movies that are dramas with rating higher than 9.5
(nrow(movies %>% filter(Genre == "Drama" & My.Rating > 9.5)) / nrow(movies)) * 100

#Answer: 0.2699784


#T6: Number of movies directed by Wes Anderson
nrow(movies %>% filter(Director == "Wes Anderson"))

#Answer: 8

#T7: Percentage of movies made after 2010, are comedies, with rating above 9
(nrow(movies %>% filter(Release.Year > 2010 & Genre == "Comedy" & My.Rating > 9)) / nrow(movies)) * 100

#Answer: 0.161987


#T8: Number of romance or independent movies released during or after 2005 with rating above 9
nrow(movies %>% filter((Genre == "Romance" | Genre == "Independent") & Release.Year >= 2005 & My.Rating > 9))

#Answer: 10


#T9: Percentage of independent movies with rating above 8.5
(nrow(movies %>% filter(Genre == "Independent" & My.Rating > 8.5)) / nrow(movies %>% filter(Genre == "Independent"))) * 100

#Answer: 10.52632


#T10: Percentage of movies owned by Matt
(nrow(movies %>% filter(Own == "Yes")) / nrow(movies)) * 100

#Answer: 11.55508


#T11: Code to subset "Movie.Name" and "Director" columns
movie_name_and_director_subset <- movies %>% select(Movie.Name, Director)

#Answer: movie_name_and_director


#T12: Code to obtain data frame of mean rating by genre
mean_rating_by_genre <- movies %>% group_by(Genre) %>% summarise(Mean.Rating = mean(My.Rating) )%>% arrange(Mean.Rating)

#Answer: mean_rating_by_genre


#T13: Genre with highest mean rating
tail(mean_rating_by_genre, 1)

#Answer: Fantasy (7.47)


#T14: Which of Wes or David has highest average rating
wes_vs_david_rating <- movies %>% filter(Director == "Wes Anderson" | Director == "David Fincher") %>% group_by(Director) %>% summarise(Average.Rating = mean(My.Rating)) %>% arrange()
tail(wes_vs_david_rating, 1)

#Answer: Wes Anderson (9.35250)


#T15: Code to randomly sample 25 movies
random_sampled_movies <- sample_n(movies, 25)

#Answer: random_sampled_movies


#T16: Code to sample one movie per genre
random_sampled_movies_per_genre <- movies %>% group_by(Genre) %>% sample_n(1)

#Answer: random_sampled_movies_per_genre


#T17: Director with highest range of ratings
dir_with_largest_range_ratings <- movies %>% group_by(Director) %>% summarise(Max = max(My.Rating), Min = min(My.Rating), Range = max(My.Rating) - min(My.Rating)) %>% arrange(Range)
tail(dir_with_largest_range_ratings, 1)

#Answer: M. Night Shyamalan (9.01)