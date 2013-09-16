# Q1. Calculate the mean rating for each movie, 
# order with the highest rating listed first, and submit the top 5.
setwd("~/prj/courses/recsys-001/recsys")
ratings = read.csv('ratings.csv', header = TRUE, sep = ',')
# cat(labels(head(sort(colMeans(ratings[,-1], na.rm=TRUE), decreasing = TRUE), 5)), sep="\n")

# Q2. Calculate the percentage of ratings
# for each movie that are 4 or higher. Order with the highest percentage first, 
# and submit the top 5.
pcent <- function(x) {  
  y = x[!is.na(x)]
  z = y[y >= 4]
  100 * (length(z) / length(y)) 
}
# cat(labels(head(sort(sapply(ratings[,-1], pcent, simplify = TRUE), decreasing = TRUE), 5)), sep="\n")

# Q3. Count the number of ratings for each movie, 
# order with the most number of ratings first, and submit the top 5.
numRatings <- function(x) {
  y = x[!is.na(x)]
  z = y[y > 0]
  length(z)
}
# cat(labels(head(sort(sapply(ratings[,-1], numRatings, simplify = TRUE), decreasing = TRUE), 5)),sep="\n")

# Q4. Calculate movies that most often occur with Star Wars: Episode IV - A New Hope(1977) 
# using the "x + y / x" method described in class. 
# In other words, for each movie, calculate the percentage of Star Wars raters 
# who also rated that movie. Order with the highest percentage first, and 
# submit the top 5.
relationWithStarWars <- function(x, sw) {
  (length(na.omit(x & sw)) / length(na.omit(sw)) )*100
}
# cat(names(head(sort(sapply(ratings[,c(3:21)], relationWithStarWars, ratings[,2], simplify=TRUE),decreasing = TRUE),5)),sep="\n")
rm(ratings)

# Programming Assignment 1
# Simple: (x and y) / x
mratings = read.csv('recsys-data-ratings.csv', sep = ',', header = FALSE)
wratings = reshape(mratings, idvar="V1", timevar="V2", direction="wide")
# View(wratings)
round(tail(head(sort(sapply(wratings[,-1], relationWithStarWars, wratings[,"V3.11"], simplify=TRUE),decreasing = TRUE),6),-1),2)

# Advance: ((x and y) / x) / ((!x and y) / !x)
zratings = wratings
zratings[is.na(zratings)] <- 0
advanceRelation <- function(x, base) {
  xbase = x & base
  xnotbase = x & !base
   (length(xbase[xbase == TRUE]) / length(base[base>0])) / (length(xnotbase[xnotbase == TRUE]) / length(base[base==0]))
}
round(tail(head(sort(sapply(zratings[,-1], advanceRelation, zratings[,c("V3.11")], simplify=TRUE),decreasing = TRUE),6),-1),2)
rm(mratings,wratings,zratings)

