library(ggplot2)

movies<-read.csv("tmdb_5000_movies.csv")
movies$release_date <- as.Date(movies$release_date)
budget<-movies$budget
revenue<-movies$revenue
rating<-movies$vote_average
genres<-movies$genres

# correlation between features
movieM<-t(rbind(budget, rating, revenue))
cat("Correlation of budget, rating and revenue:\n")
cor(movieM)
plot(log(budget), log(revenue))

# select horror and non-horror movies
horror<-movies[grepl("Horror",genres),]
non_horror<-movies[!grepl("Horror",genres),]
cat("Average rating of horror movies:", mean(horror$vote_average), "\n")
cat("Average rating of non-horror movies:", mean(non_horror$vote_average), "\n")

combine <- function(group1, group2, name1, name2) {
  return(data.frame(
    genregroup=c(rep(name1, length(group1)), rep(name2, length(group2))),
    rating=c(group1, group2)
  ))
}

# compare ratings of horror and non-horror
df <- combine(horror$vote_average, non_horror$vote_average, "horror", "non_horror")
ggplot(df, aes(rating, fill = genregroup)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 20)+
  ggtitle(paste("avg rating for genregroup"))

# performing the welsh t test for significant differences
# one sided because we want to test if horror movies are rated WORSE
hm.test <-t.test(horror$vote_average, non_horror$vote_average, alternative = "less")
cat("t-test:", hm.test$p.value, "\n")

# this suggests horror movies are indeed rated worse than non horror
# my suspicion has been confirmed, 
# but maybe that is because there are more low butget horror movies that fail in box office?
# what happens if we only test the movies which generated a revenue over a certain threshold?

# compare ratings of successful horror and non-horror
revenue_limit <- 1000000
successful_horror <- horror[horror$revenue > revenue_limit,]
successful_non_horror <- non_horror[non_horror$revenue > revenue_limit,]
cat("Average rating of successful horror movies:", mean(successful_horror$vote_average), "\n")
cat("Average rating of successful non-horror movies:", mean(successful_non_horror$vote_average), "\n")

dh <- combine(successful_horror$vote_average, successful_non_horror$vote_average, "horror", "non_horror")
ggplot(dh, aes(rating, fill = genregroup)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 20)+
  ggtitle(paste("avg rating for genregroup (successful movies only)"))

# performing the welsh t test for significant differences
shm.test <- t.test(successful_horror$vote_average, successful_non_horror$vote_average, alternative = "less")
cat("t-test:", shm.test$p.value, "\n")
# again, this suggests that my suspicion is also true for successful movies 

# and now something completely different...
# compare american and non american movies
american<-movies[grepl("United States of America", movies$production_countries),]
non_american<-movies[!grepl("United States of America", movies$production_countries),]

dg <- combine(american$vote_average, non_american$vote_average, "american", "non_american")

ggplot(dg, aes(rating, fill = genregroup)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins = 20)+
  ggtitle(paste("avg rating by production location"))
