#---------------------------------#
#- Data Analysis & Graphics in R -#
#-------Shapefiles----------------#
#---------------------------------#

#PPP's planar point patterns#

# Please install package if not already installed
#install.packages('spatstat')
#install.packages('maptools')
#install.packages('rgdal')
#if (!"maps" %in% installed.packages()) install.packages("maps")

# library for analysing spatial data
library(spatstat)
# loads locations of 195 redwood seedlings
# in a square sampling region
data(redwoodfull)
View(redwoodfull)
summary(redwoodfull)
plot(redwoodfull)

plot(density(redwoodfull, .05))
plot(density(redwoodfull, .05),col=brewer.pal('BuPu',n=9))
contour(density(redwoodfull, .05), axes=FALSE)

image(density(redwoodfull, .05))
contour(density(redwoodfull, .05), axes=FALSE, add=TRUE)

Q<-quadratcount(redwoodfull, nx=2, ny=2)
Q
plot(redwoodfull)
plot(Q, add=TRUE, cex=2)


#Shape Files#

library(maptools)
library(rgdal)
library(maps)
require(mapproj)

# low resolution map of the world
map()
# national boundaries
map('usa')
# county map of Texas
map('county', 'Texas')
# map of three states
map('state', region = c('Texas', 'New Mexico', 'Arizona'))  
# show the effect of myborder = 0
map.axes()

# Bonne equal-area projection of states
map('state', proj = 'bonne', param = 45)
# map of the dakotas
map("state", ".*dakota", myborder = 0)
# names of the San Juan islands in Washington state
map('county', 'washington,san', names = TRUE, plot = FALSE)

# national boundaries in one linetype, states in another
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)

# plot the ozone data on a base map
data(ozone)
View(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
# put in x,y position the median value
text(ozone$x, ozone$y, ozone$median)
box()
   
# mapproj is used here for projection="polyconic"
# color US county map by 2009 unemployment rate
# match counties to map using FIPS county codes
# load unemp data which includes data for some counties not on the "lower 48 states" county map,
# such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia cities
data(unemp)
data(county.fips)

# define color buckets
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")

# align data with map definitions by (partial) matching state,county
# names, which include multiple polygons for some counties
cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                    county.fips$polyname)]
colorsmatched <- unemp$colorBuckets [match(cnty.fips, unemp$fips)]

# draw map
map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lwd = 0.25, projection = "polyconic")
map("state", col = "black", fill = FALSE, add = TRUE, lwd = 1,
    projection="polyconic")
title("unemployment by county, 2009")
legend("bottomleft", leg.txt, cex=0.60, horiz = FALSE, fill = colors)

