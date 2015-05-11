#PPP's planar point patterns#

# Please install package if not already installed
# install.packages('spatstat')
library(spatstat)     # comprehensive toolbox for analysing spatial data
data(redwoodfull)     # loads locations of 195 redwood seedlings in a square sampling region
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

# Please install package if not already installed
#install.packages('maptools')
#install.packages('rgdal')
#if (!"maps" %in% installed.packages()) install.packages("maps")

library(maptools)
library(rgdal)
library(maps)

map() # low resolution map of the world
map('usa')  # national boundaries
map('county', 'new jersey') # county map of New Jersey
map('state', region = c('new york', 'new jersey', 'penn'))  # map of three states
map("state", ".*dakota", myborder = 0)  # map of the dakotas
map.axes()  # show the effect of myborder = 0
if(require(mapproj))
  map('state', proj = 'bonne', param = 45)    # Bonne equal-area projection of states

# names of the San Juan islands in Washington state
map('county', 'washington,san', names = TRUE, plot = FALSE)

# national boundaries in one linetype, states in another
map("state", interior = FALSE)
map("state", boundary = FALSE, lty = 2, add = TRUE)

# plot the ozone data on a base map
data(ozone)
map("state", xlim = range(ozone$x), ylim = range(ozone$y))
text(ozone$x, ozone$y, ozone$median)
box()
if(require(mapproj)) {    
  # mapproj is used for  projection="polyconic"
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
      lty = 0, projection = "polyconic")
  map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
      projection="polyconic")
  title("unemployment by county, 2009")
  legend("bottomleft", leg.txt, cex=0.60, horiz = FALSE, fill = colors)
}
