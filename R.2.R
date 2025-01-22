library(data.table) 
?fread # VS read.csv 
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')

## TODO count the number of bookings below 100 EUR
bookings[price < 100, .N]

## TODO count the number of bookings below 100 EUR without an offer
bookings[price<100 & offer ==0, .N]


## TODO compute the average price of the bookings below 100 EUR
bookings[price<100, mean(price, na.rm=TRUE)]

## TODO compute the average price of bookings on weekends
bookings[weekend==1, mean(price, na.rm=TRUE)]

## TODO compute the average price of bookings on weekdays
bookings[weekend==0, mean(price, na.rm=TRUE)]

## TODO include nnights, holiday and year as well in the aggregate variables
bookings[, .(mean_price = mean(price, na.rm = TRUE)), by = .(weekend, nnights, holiday, year)][order(year)]

## TODO avg price per number of stars
bookings[, .(mean_price = mean(price, ma.rm=TRUE)), by = ]


bookings[1]
features[hotel_id ==1]
bookings[hotel_id==1]

##join: left, right, ..

?merge
#x[y, rolling = ..] Check syntax for more details
#?data.table
#we will just do merge
merge(bookings, features)[, mean(price), by = stars]
bookings

## why do e miss 3 rows:
bookings[is.na(hotel_id)]
features[is.na(hotel_id)]
features[duplicated(hotel_id)]

features

bookings[hotel_id %in% features$hotel_id]
features[hotel_id ==2]

merge(bookings, features)
merge(bookings, features, all = TRUE)

merge(bookings, features)[, .(.N, price = mean(price)), by = stars][order(stars)]

library(ggplot2)
ggplot(merge(bookings, features)[stars ==2.5], aes(price)) + geom_histogram()


merge(bookings, features)[stars==2.5][, mean(price), by = nnights]



dt <- merge(bookings, features)
dt$price_per_night <- dt$price / dt$nnights #OR dt[, price_per_night := price/nnights]
dt[, price_per_night := price/nnights]

dt[, mean(price_per_night), by = stars][order(stars)]
bookings[, .N, by =.(weekend,nnights)]


dt[, weighted.mean(price_per_night, nnights), by = stars][order(stars)]


##TODO hotels dataset: features + avg price of a night

hotels = merge(features, bookings[, .(price_per_night = mean(price/nnights), bookings =.N), by = hotel_id])

hotels[, weighted.mean(price_per_night), by = stars][order(stars)]

##TODO dataviz of price per nights per stars
dta <- hotels[, .(weighted_mean_price_per_night = weighted.mean(price_per_night)), by = stars][order(stars)][!is.na(stars)] #!is.na removes stars with N/A value after aggregation
ggplot(dta, aes(x = factor(stars), y = weighted_mean_price_per_night )) + geom_point() + xlab("Number of stars")


#TODO dataviz on avg price per nights per stars split by country (facet)
dta <- hotels[, .(weighted_mean_price_per_night = weighted.mean(price_per_night)), by = .(stars, country)][order(stars)][!is.na(stars)]
ggplot(dta, aes(factor(stars), weighted_mean_price_per_night)) + geom_point() + xlab("Number of stars") + facet_wrap("country", scale = "free")


## aggregated dataset by country, avg price, ratings, stars
countries <- hotels[, .(
  price = mean(price_per_night, na.rm = TRUE),
  ratings = mean(rating, na.rm = TRUE),
  stars = mean(stars, na.rm = TRUE)
), by = country]


## TO list countries with above avg rating
avg_rating <- mean(countries$ratings, na.rm = TRUE)
countries[ratings > avg_rating]

hotels[, pricecat := cut(price_per_night, 3)]
hotels[, .N ,by = pricecat]


hotels[, pricecat := cut(price_per_night, c(0,100,250, Inf), labels = c("cheap", "avg", "expensive"))]

hotels[, .N, by = pricecat]

?quantile
lower <- quantile(hotels$price_per_night, 1/3)
upper <- quantile(hotels$price_per_night, 2/3)
hotels[, pricecat := cut(price_per_night, c(0,lower,upper, Inf), labels = c("cheap", "avg", "expensive"))]

hotels[, pricecat := cut(price_per_night, c(0, quantile(price_per_night, c(1/3, 2/3)), Inf), labels = c("cheap", "avg", "expensive"))]
hotels[, .N, by = pricecat]



hotels[, pricecat := cut(price_per_night, c(0, quantile(price_per_night, c(1/3, 2/3)), Inf), labels = c("cheap", "avg", "expensive"))]
hotels[, .N, by = .(pricecat, country)]


hotels[, lower := quantile(price_per_night, 1/3), by = country]
hotels[, upper := quantile(price_per_night, 2/3), by = country]

rm(lower)
rm(upper)
#^to make sure we dont include global lower and global upper, we remove.
hotels[, pricecat := cut(price_per_night, c(0,lower[1] ,upper[1], Inf), labels = c("cheap", "avg", "expensive")), by = country]
hotels[, .(0, lower[1], upper[1], inf) , by  = country]


hotels[upper != lower, pricecat := cut(price_per_night, c(0, lower[1], upper[1], Inf), labels = c("cheap", "avg", "expensive")), by = country]


##TODO data.table with x(1:100), y (1:100), color columns (red/white)

points <- data.table(x = rep(1:100, 100), y = rep(1:100, each = 100), col = "white")
points[(x - 50)^2 + (y - 50)^2 < 50, col := "red"]
points[, .N, by = col]
library(ggplot2)
ggplot(points, aes(x, y, color = col)) + geom_point() + theme_void() +
  scale_color_manual(values = c("red", "white")) +
  theme(legend.position = 'none')


##todo model: col -x + y

?lm
lm(col ~ x + y, data = points, family = binomial(link = logit))

str(points)


#better & complete code
points[, col := factor(col)]
fit <- glm(col ~ x + y, data = points, family = binomial(link = logit))
summary(fit)

predict(fit, type = "response")

ggplot(points, aes(x,y, color = factor(round(pred)))) + geom_point() + theme_void() + 
  scale_color_manual(values = c("red", "white")) + theme(legend.position = 'none')

library(rpart)
fit <- rpart(col ~ x + y, points)
fit
plot(fit)
text(fit)
points$pred <- predict(fit, type = "class")
ggplot(points, aes(x, y, color = pred)) + geom_point() + theme_void() +
  scale_color_manual(values = c("red", "white")) +
  theme(legend.position = 'none')  

ggplot(points, aes(x, y, fill = pred)) + geom_tile() + theme_void() +
  scale_fill_manual(values = c("red", "white")) +
  theme(legend.position = 'none')

ggplot(points, aes(x, y)) + geom_tile(aes(fill = pred)) + geom_tile(aes(fill = col), alpha = 0.5) + theme_void() +
  scale_fill_manual(values = c("red", "white")) +
  theme(legend.position = 'none')



library(partykit)
plot(as.party(fit))

?rpart


fit <- rpart(col ~ x+ y, points, maxdepth =1 )

fit <- rpart(col ~ x+ y, points, maxdepth =1 ) #

library(randomForest)# h2o
fit <- randomForest(col ~ x + y, points)
points$pred <- predict(fit, type = "class")
ggplot(points, aes(x, y)) + 
  geom_tile(aes(fill = pred)) + 
  geom_tile(aes(fill = col), alpha = 0.5) + 
  theme_void() +
  scale_fill_manual(values = c("red", "white")) +
  theme(legend.position = 'none')

