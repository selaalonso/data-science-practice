### class notes 7/18 ### 

library(dplyr) 

## one-sample t-test 
sleep_hours = c(5, 5, 5, 6, 6, 7, 7, 7, 8, 9) 
mean(sleep_hours) 
sd(sleep_hours) # sd - standard deviation 
t.test(sleep_hours, mu = 7) # mu is the mean that you're comparing it to - it 
# comes from the null hypothesis 

# p-value: probability of getting the observed difference or a greater 
# observed difference, based on the way that we sampled our data 

# making the t test one-tailed: 
t.test(sleep_hours, mu = 7, alternative = "less") 

# wrong direction: 
t.test(sleep_hours, mu = 7, alternative = "greater") 

## example using iris dataset 
# looking at one iris species, comparing it to the mean sepal lengths of the other species 

pop_mean = mean(iris$Sepal.Length) 
setosa_sl = filter(iris, Species == "setosa") 

t.test(setosa_sl$Sepal.Length, mu = pop_mean) 
# if the sample mean (of setosa sepal lengths) is 5.84333 (the same as the pop mean) 
# then (a very very small %) of the time, this difference (or more) will 
# happen by chance 

### practice 

## choose a different numeric variable (sepal width, petal length, petal width) 
## and compare any of the 3 species to the population mean (setosa, versicolor, 
## and virginica) 

# petal width, virginica 
population_m = mean(iris$Petal.Width) 
virginica_pw = filter(iris, Species == "virginica") 

t.test(virginica_pw$Petal.Width, mu = population_m) 
# if the population mean truly is 1.199333, then the p-value is small enough that 
# there is a significant difference 

# petal length, setosa 
mPopulation = mean(iris$Petal.Length) 
setosa_pl = filter(iris, Species == "setosa") 

t.test(setosa_pl$Petal.Length, mu = mPopulation) 
# if the population mean is truly 3.758, then the p-value is small enough that 
# there is a significant difference 
# basically, the true mean of the setosa species ≠ population mean 

## two-sample t-test 

versicolor = filter(iris, Species == "versicolor") 
setosa = filter(iris, Species == "setosa") 

t.test(versicolor$Sepal.Length, setosa$Sepal.Length) 

# (mean of setosa sepal lengths) - (mean of versicolor sepal lengths) != 0, 
# which means that these two means are not equal to each other 


## comparing versicolor and virginica sepal lengths. what is the p value? 
## is it significant? 

virginica = filter(iris, Species == "virginica") 

t.test(versicolor$Sepal.Length, virginica$Sepal.Length) 
# p value = 1.866e-07, it's significant 


## comparing setosa and versicolor petal lengths. what is the p value? 
## is it significant? 

t.test(setosa$Petal.Length, versicolor$Petal.Length) 
# p-value < 2.2e-16, it's significant 


## paired t-test 
install.packages("datarium") 
library(datarium) 
?mice2 
mice2 

t.test(mice2$before, mice2$after, paired = T) 
# the true difference in means != 0 
# the p value (1.039e-09) is less than 0.05, which means that there is a 
# significant difference between the before and after groups 

# making it one-tailed instead of two-tailed: 
t.test(mice2$before, mice2$after, paired = T, alternative = "less") 

# since they're paired together it's comparing the differences between the before 
# and after weights of each mice; it uses the mean of the differences to compare 
# that to 0 to see if there's an overall difference 


## ANOVA 

sepal_len_anova = aov(data = iris, Sepal.Length ~ Species) 

# are any categories different? 
summary(sepal_len_anova) 

# which groups are significantly different? 
TukeyHSD(sepal_len_anova) 
# answer: all of them 

# TukeyHSD(): shows comparisons of the means, and p values 
# if the p values are small enough it'll round them down to 0 (if it says that 
# the p value is 0 it's usually not, but it's so small it got rounded down) 


sepal_width_anova = aov(data = iris, Sepal.Width ~ Species) 
# calculating variance of sepal lengths for each of the iris species (both 
# within and between them) 
summary(sepal_width_anova) 
TukeyHSD(sepal_width_anova) 
# all groups are significantly different 


## looking at the diamonds dataset 
View(diamonds) 

# seeing if the price is different depending on the color of the diamond 
diamond_price_color = aov(data = diamonds, price ~ color) 
summary(diamond_price_color) 
# to save results, use $categorical_variable 
signif_results = TukeyHSD(diamond_price_color)$color 
# there is a significant difference between all of the groups except for E-D & J-I 

# it has to be converted to a data frame in order to use dplyr functions 
arrange(as.data.frame(signif_results), `p adj`) 


## applying these concepts to my practice dataset: 
# 2 sample t test and/or anova test; update github repo 

albums = read.csv("data/albums.csv") 
View(albums) 

## comparing pop and rock number of sales 
pop = filter(albums, genre == "Pop") 
rock = filter(albums, genre == "Rock") 

t.test(pop$num_of_sales, rock$num_of_sales) 
# the p value is 0.3219, so there is no significant difference 


## comparing pop and live number of sales 
live = filter(albums, genre == "Live")  

t.test(pop$num_of_sales, live$num_of_sales) 
# the p value is 0.001755, so there is a significant difference 


## ANOVA test - number of tracks by genre 

list_of_genres = c("Alternative", "Country", "Indie", "K-Pop", "Rock") 
albums_small = filter(albums, genre %in% list_of_genres) 

num_tracks_anova = aov(data = albums_small, num_of_sales ~ genre) 
summary(num_tracks_anova) 
results = TukeyHSD(num_tracks_anova)  
# all of the p values were well above 0.05, so there is no significant difference 
# between any of the genres 
