library(ggplot2)

movies<-read.csv("tmdb_5000_movies.csv")
movies$release_date <- as.Date(movies$release_date)
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

combine <- function(group1, group2, name1, name2) {
  return(data.frame(
    genregroup=c(rep(name1, length(group1)), rep(name2, length(group2))),
    rating=c(group1, group2)
  ))
}

df <- combine(horror$vote_average, non_horror$vote_average, "horror", "non_horror")

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
mean(successful_horror$vote_average)
mean(successful_non_horror$vote_average)

dh <- combine(successful_horror$vote_average, successful_non_horror$vote_average, "horror", "non_horror")

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

dg <- combine(american$vote_average, non_american$vote_average, "american", "non_american")

ggplot(dg, aes(rating, fill = genregroup)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 20)+
  ggtitle(paste("avg rating by production location"))
