##### Reading in Data, extracting interesting features ###
library(dplyr)
library(magrittr)
library(ggplot2)

movies<-read.csv("tmdb_5000_movies.csv")
movies$release_date<-movies$release_date%>%as.Date()
budget<-movies$budget
revenue<-movies$revenue
rating<-movies$vote_average
genres<-movies$genres

##### plotting the features w/ histograms ###
hist(budget)
hist(revenue)
hist(rating)

##### correlation btw. features ###
movieM<-t(rbind(budget, rating, revenue))
cor(movieM)
cor(budget,revenue)
plot(log(budget),log( revenue))

##### comparing horror genre w/ non horror ###
horror<-movies[grepl("Horror",genres),]
non_horror<-movies[!grepl("Horror",genres),]
hist(horror$vote_average)
hist(non_horror$vote_average)
mean(horror$vote_average)
mean(non_horror$vote_average)


##### plotting results ###
y_name <- "rating"
x_name <- "genregroup"

##### Helper function to create data.frame for the chunk of the data ###
prepare <- function(name, value, xname = x_name, yname = y_name) {
  data_frame(rep(name, length(value)), value) %>%
    set_colnames(c(xname, yname))
}

df<-bind_rows(
  prepare("horror", horror$vote_average),
  prepare("nonhorror", non_horror$vote_average)
)

##### groupwise plotting ###
ggplot(df, aes(rating, fill = genregroup)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 20)+
  ggtitle(paste("avg rating for genregroup"))

##### performing the welsh t test for significant differences ###
#one sided because we want to test if horror movies are rated WORSE
hm.test <-t.test(horror$vote_average, non_horror$vote_average, alternative = "less")
hm.test$p.value

# this suggests horror movies are indeed rated worse than non horror
# my suspicion has been confirmed, 
# but maybe that is because there are more low butget horror movies that fail in box office?
# what happens if we only test the movies which generated a revenue over a certain threshold?


##### comparing successfull horror genre w/ non-horror ###
successful_horror<-horror[horror$revenue>1000000,]
successful_non_horror<-non_horror[non_horror$revenue>1000000,]
mean(succesful_horror$vote_average)
mean(succesful_non_horror$vote_average)

dh<-bind_rows(
  prepare("horror", successful_horror$vote_average),
  prepare("nonamerican", successful_non_horror$vote_average)
)
ggplot(dh, aes(rating, fill = genregroup)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 20)+
  ggtitle(paste("avg rating for genregroup (successful movies only)"))

##### performing the welsh t test for significant differences ###
shm.test <- t.test(successful_horror$vote_average, successful_non_horror$vote_average, alternative = "less")
shm.test$p.value
#this suggests that my suspicion is also true for successful movies 

##### comparing american and non american movies ###
american<-movies[grepl("United States of America", movies$production_countries),]
non_american<-movies[!grepl("United States of America", movies$production_countries),]
mean(american$budget)
mean(non_american$budget)
mean(american$revenue)
mean(non_american$revenue)
mean(american$vote_average)
mean(non_american$vote_average)

dg<-bind_rows(
  prepare("american", american$vote_average),
  prepare("nonamerican", non_american$vote_average)
)
ggplot(dg, aes(rating, fill = genregroup)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 20)+
  ggtitle(paste("avg rating by production location"))
