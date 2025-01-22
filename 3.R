remotes::install_github("daroczig/students")


library(students)
?students
str(students)
lm(math ~ shoe, students)

plot(students$shoe, students$math)
abline(lm(math ~ shoe, students), col = 'red')

##TODO ggplot on the above
library(ggplot2)
ggplot(students, aes(x = shoe, y= math)) + 
  geom_point() + geom_smooth(method = 'lm', color = 'red') + theme_bw()

##TODO general EDA
library(GGally)
ggpairs(students)


summary(students)


library(gtExtras)
install.packages("gtExtras")
install.packages("svglite")
install.packages("gt")
library(gt)
gt_plt_summary(students)


lm(math ~ shoe + x, students)
rm(list = ls())

.secret #this msg is part of the r file on github. so its better to check whenever u r installing dataset from github to avoid getting arbitrary code

download.file("https://bit.ly/de-cities-distance", "cities.xls", mode = "wb") 
library(readxl) 
cities <- read_excel("cities.xls")
str(cities)

cities <- cities[1:(nrow(cities)-3), ]
cities <- cities[, -1]

#row.names
str(cities)

?cmdscale


#used as.distance as cities is already a distance matrix
mds <- cmdscale(as.dist(cities))

plot(mds)
text(mds[, 1], mds[, 2], names(cities))

#rotating axes
mds < - -mds
plot(mds)
text(mds[, 1], mds[, 2], names(cities))

mds[1,] <- mds[1,] * -1
mds[,2] <- mds[, 2] * -1

##TODO redo plot using ggplot2
mds <- cmdscale(as.dist(cities))
mds <- as.data.frame(mds)
ggplot()
mds$city <- rownames(mds)
str(mds)
ggplot(mds, aes(x=V1, y = -V2, label = city )) + geom_text() + theme_void()

##TODO using eurodist
?eurodist
eurodist
str(eurodist)

mds <- cmdscale(as.dist(eurodist))
mds <- as.data.frame(mds)
mds$city <- rownames(mds) #take column names from row names of mdshttp://127.0.0.1:20765/graphics/plot_zoom_png?width=1888&height=939
ggplot(mds, aes(x=V1, y = -V2, label = city )) + geom_text() + theme_void()

str(mds)

install.packages("ggmap")
library(ggmap)
install.packages("tidygeocoder")
library("tidygeocoder")
?geocode

library(data.table)
mds <- data.table(geocode(mds, "city")) #geocode package takes actual coordinates of address from nominatim website API.

str(mds)
ggplot(mds, aes(x=long, y = lat, label = city )) + 
  geom_text() + theme_void()

world <- map_data("world")
ggplot() +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = region)) +
  theme_void() + coord_fixed(1.3) +
  geom_point(data = mds, aes(long, lat), color = 'orange')


world$a <- grepl("^A", world$region)
str(world)
ggplot()+
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = region, fill = a)) +
  theme_void() + coord_fixed(1.3) +
  geom_point(data = mds, aes(long, lat), color = 'orange')

install.packages("countrycode")
library(countrycode)
##TODO lookup continent of the regions in world

?countrycode

world <- data.table(world)
world[, continent := countrycode(region ,origin = 'country.name' , destination = 'continent'), by = region ]

ggplot()+
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = region, fill = continent)) +
  theme_void() + coord_fixed(1.3) +
  geom_point(data = mds, aes(long, lat), color = 'black') + theme(legend.position = 'none')

?geom_point

register_stadiamaps('c8880324-95c7-4b2c-be2f-c7faf72b49db')
?get_stadiamap

mds <- mds[]

map <- get_stadiamap(
  c(left = min(mds$long)* 0.995,
    right = max(mds$long) * 1.001,
    bottom = min(mds$lat) * 0.995,
    top = max(mds$lat) * 1.001),
  zoom = 4,
  maptype = "stamen_toner"
)

ggmap(map) + theme_void() + geom_point(data = mds, aes(long, lat), color ="orange")

download.file('https://stacks.stanford.edu/object/rc343vz5889','Austria_boundary.zip', mode = 'wb') 
download.file('https://stacks.stanford.edu/object/yv617vc9132', 'Austria_divisions.zip', mode = 'wb') 
unzip('Austria_boundary.zip')
unzip('Austria_divisions.zip')

install.packages("sf")
library(sf)
st_layers(".")

adm0 <- st_read(".", "AUT_adm0") #- this one is imp, specifying folder and loading adm0 layer
plot(adm0)
st_geometry(adm0)
ggplot() + geom_sf(data = adm0) #and this one is imp

##TODO visualize adm2
adm2 <- st_read(".", "AUT_adm2") #- this one is imp, specifying folder and loading adm2 layer
#plot(adm0)
#st_geometry(adm0)
ggplot() + geom_sf(data = adm2)http://127.0.0.1:20765/graphics/plot_zoom_png?width=1904&height=978
ggplot()+ geom_sf(data = adm0, color = "black", size = 2) + 
  geom_sf(data = adm2, color = "gray", size = .1) + theme_void()

cities <- fread('https://simplemaps.com/static/data/country-cities/at/at.csv')
ggplot()+ geom_sf(data = adm0, color = "red", linewidth = 2) + 
  geom_sf(data = adm2, color = "gray", size = .1) + geom_point(data = cities, aes(lng, lat, size = population), color = "red")+ theme_void() 

download.file('https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-95/bezirke_95_topo.json','austria.geojson')
map <- st_read('austria.geojson')

install.packages("leaflet")
library("leaflet") #used to create interactive maps

leaflet(map)

#MDS
?mtcars
?cmdscale
cmdscale(dist(mtcars))
mds <- cmdscale(dist(scale(mtcars)) #it uses all columns to calculate distance. we can specify or transform certain columsn as well. dist(mtcarsc[])
mds <- as.data.frame(mds)
mds$car <- rownames(mtcars)
ggplot(mds, aes(V1, -V2, label = car)) + geom_text() + theme_void()

install.packages("ggrepel") #to solve overallping issue
library(ggrepel)
ggplot(mds, aes(V1, -V2, label = car)) + geom_text_repel() + theme_void()


#by scaling, it standardizeds all columns values.the average becomes 0, powerful cars have value over or around 2. less than avg ones have less than 0.

##berkeley
?UCBAdmissions
UCBAdmissions
as.data.frame(UCBAdmissions)
plot(UCBAdmissions)


##TODO visualization
berkeley <- as.data.frame(UCBAdmissions)
ggplot(berkeley, aes(interaction(Dept, Gender), Freq, fill = Admit)) + 
  geom_col(position = 'fill') + facet_wrap (~Dept) +
  #scale_fill_brewer(palette = "Dark2")
  scale_fill_manual(values = c("Admitted" = "darkgreen", "Rejected" = "brown"))+
  scale_y_continuous(labels = scales::label_percent())

?iris
 iris                    
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species))+ geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  geom_smooth(method = 'lm', se = FALSE, color = 'black')

#OR
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(color = Species), method = 'lm', se = FALSE) +
  geom_smooth(method = 'lm', se = FALSE)

#OR
overall_iris <-copy(iris)
overall_iris$Species <- 'Overall'
str(rbind(iris, overall_iris))

ggplot(rbind(iris, overall_iris), aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = 'lm', se = FALSE)


anscombe

mean(anscombe$x1)
mean(anscombe$x3)
sd(anscombe$x1)
sd(anscombe$x2)
cor(anscombe$x1, anscombe$y1)

#all 4 data sets have same mean, median, cor but visually all are different. Looking at descriptive stats, it suggests that
#eveyrthing is same

for (i in 1:4) {
  print(mean(anscombe[, i]))
}
lapply(1:4, function(i) mean(anscombe[, i]))
lapply(1:4, function(i) sd(anscombe[, i]))
lapply(1:4, function(i) anscombe[, i])
lapply(1:4, function(i) cor(anscombe[, i], anscombe[, i +4]))

dt <- rbindlist(lapply(1:4, function(i) 
  data.frame(
    x = anscombe[, i], 
    y = anscombe[, i + 4],
    dataset_id = i)))


##
anscombedf <- data.frame(anscombe)

ggplot(dt, aes(x, y)) + geom_point(color = "orange") + geom_smooth(method='lm', se = FALSE ) + facet_wrap(~dataset_id) + theme_bw()


##with datasaurus dataset
install.packages("datasauRus")
library(datasauRus)
df <- copy(datasaurus_dozen_wide)

dt <- rbindlist(lapply(1:13, function(i) 
  data.frame(
    x = df[, 2*i -1, drop = TRUE], 
    y = df[, 2*i, drop = TRUE],
    dataset_id = i)))

str(df)

ggplot(dt, aes(x, y)) + geom_point(color = "orange") + geom_smooth(method='lm', se = FALSE ) + facet_wrap(~dataset_id) + theme_bw()
