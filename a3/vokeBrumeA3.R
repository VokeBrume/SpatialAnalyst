#Name:  Voke Brume
#Assignment:  3
#Date: 03/04/2021

#setwd("D:/GEOG693C/assignments/a3/Summary_and_Stats")
mine_data <- read.csv("a3/Summary_and_Stats/mine_classification_with_lidar.csv")
movies <- read.csv("a3/Summary_and_Stats/matts_movies.csv")
wetland <- read.csv("a3/Summary_and_Stats/wetland_binary.csv")

library(dplyr)
library(car)
library(asbio)


#T1: Number of different land cover types
length((levels(as.factor(mine_data$class))))

#Answer: 5


#T2: Land cover type with highest mean normalized difference vegetation index
mine_data %>% group_by(class) %>% summarize(mean_ndvi = mean(ndvi)) %>% arrange(mean_ndvi)

#Answer: Shrub (0.301)


#T3: Land cover type with highest mean height ("diff")
mine_data %>% group_by(class) %>% summarize(mean_height = mean(diff)) %>% arrange(mean_height)

#Answer: Forest (11.1)


#T4: Standard deviation of NDVI for the forest class
mine_data %>% filter(class == "forest") %>% summarize(standard_deviation = sd(ndvi))

#Answer: 0.04083046


#T5: Mean rating for the dramas in the movie data set
movies %>% filter(Genre == "Drama") %>% summarize(mean_rating = mean(My.Rating))

#Answer: 7.140561


#T6: Genre with the largest range of ratings in the movies data set
genre_with_largest_range_ratings <- movies %>% group_by(Genre) %>% summarise(Max_Rating = max(My.Rating), Min_Rating = min(My.Rating), Range = max(My.Rating) - min(My.Rating)) %>% arrange(Range)
tail(genre_with_largest_range_ratings, 1)

#Answer: Action (9.2)


#T7: Spearman method
cor(wetland[ , c(7, 9)], method = "spearman")

#Answer: 0.5006004


#T8: t-test
wetland_sub <- wetland %>% select(class, slp_d)
t.test(slp_d ~ class, data = wetland_sub)


#Answer: Going by the wide difference in mean of group "not" (15.478290) and "wet" (2.172118), coupled with the very low p-value,
#        we can deduce that they are statistically different. The slope on wetlands is statistically different than
#        the slope of none-wetlands


#T9: Mann-whitney U test
wilcox.test(slp_d ~ class, data = wetland_sub)

#Answer: The alternative hypothesis reported from the test shows that the data sets are different. A statistically significant p-value
#        is obtained, again suggesting that mean slope in both areas are different.


#T10: t-test
genre_sub <- movies %>% filter(Genre == "Drama" | Genre == "Comedy") %>% select(My.Rating, Genre)
t.test(My.Rating ~ Genre, data = genre_sub)

#Answer: Going by the little difference in mean of group "Comedy" (6.735978) and "Drama" (7.140561 ), coupled with 
#        a p-value that is less than 0.05 for a 95% confidence interval, we can deduce that they are statistically similar.
#        The rating of comedy movies is only slightly different than the rating of drama movies.


#T11: Mann-whitney U test
wilcox.test(My.Rating ~ Genre, data = genre_sub)

#Answer: The alternative hypothesis reported from the test shows that the data sets are different. The p-value of 0.0001081
#        paints it clearer picture, showing that average ratings of both movie genre are not too different.


#T12: ANOVA
mine_data_sub <- mine_data %>% filter(class == "forest" | class == "shrub" | class == "water") %>% select(class, ndvi)
fit <- aov(ndvi ~ class, data = mine_data_sub)
summary(fit)

#Answer: The p-value of <2e-16 *** suggests that there is a statistical difference among atleast two of the three land cover types.
#        ANOVA does not specify which two of the three classes are statistically different. A pairwise test is needed to determine this.


#T13: Tukey's Honest Significant Difference test
TukeyHSD(fit)

#Answer: The p-values of 0 shows that all pairs are statistically different.


#T14: QQ plot
qqPlot(lm(ndvi ~ class, data = mine_data_sub), simulate = TRUE, main = "Q-Q Plot", labels = FALSE)

#Answer: From the plot, it can be observed that the residuals lost confidence (deviated from within the confidence interval)
#        around the high values but mostly around the low values.This suggests that there is an issue with the normal distribution
#        of the residuals.


#T15: Bartlett Test of Homogeneity of variance
bartlett.test(ndvi ~ class, data = mine_data_sub)

#Answer:  The p-value of < 2.2e-16 is statistically significant. This means that each class of land cover does not have similar variants,
#         which is an issue.


#T16: Bonferroni Outlier Test
outlierTest(fit)

#Answer:  Yes, the high values in the first column suggests that the are outliers. Justification is needed before outliers can be thrown out.


#T17: Kruskal-Wallis Rank Sum Test
kruskal.test(ndvi ~ class, data = mine_data_sub)

#Answer:  The p-value of < 2.2e-16 which is less than 0.05 suggests that there is a statistical difference among atleast two of the three land cover types.


#T18: Pairwise Kruskal-Wallace Test
mine_data_sub_2 <- mine_data %>% filter(class == "forest" | class == "herb") %>% select(class, ndvi)
pairw.kw(mine_data_sub_2$ndvi, mine_data_sub_2$class, conf = 0.95)

#Answer:  The p-value of 3e-06 which is less than 0.05 and rejected null hypothesis suggests that they are statistically different.