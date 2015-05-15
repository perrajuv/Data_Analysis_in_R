#---------------------------------#
#- Data Analysis & Graphics in R -#
#-------------ggplot2-------------#
#---------------------------------#

# Please install package if not already installed
# install.packages('ggplot2')
# install.packages('RColorBrewer')
# install.packages('devtools')
# install.packages('scales')
library(ggplot2); library(scales); library(grid); library(RColorBrewer)

# Theme created at start of file for orgazinational purposes
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#Import cars dataset:
cars <- read.csv('./data/Cars.csv')
head(cars)

#ggplot uses a different "grammar" for building plots 
#than other graphics functions. It takes 2 steps:
#1. Define a gglot object based on a data frame
#2. Build layers to your ggplot object on a single command line

###
### Chart 0-1: Barplot & Scatterplot
###
typebar <- ggplot(cars,aes(x=Type))
typebar + geom_bar()
mpg <- ggplot(cars,aes(x=Weight,y=HighwayMPG))
mpg + geom_point()
#Change properties of the graph by adding more layers...
mpg + geom_point(pch=24,cex=2.5) + 
  labs(list(title='Car Weights and MPG', x='Weight',y='MPG'))
mpg + geom_point() + 
  geom_vline(xintercept=mean(cars$Weight)) + 
  geom_hline(yintercept=mean(cars$HighwayMPG))
#Smoothing method="gam" if max(frequency)>1000. If <1000, method="loess".
mpg + geom_point() + 
  stat_smooth(method="loess")
mpg + geom_point() + 
  xlim(c(2500,4000)) + 
  ylim(c(20,35))
#You can color or fill your graphs with another attribute:
typebar2 <- ggplot(cars,aes(x=Type,fill=Origin))
typebar2 + geom_bar()
mpg2 <- ggplot(cars,aes(x=Weight,y=HighwayMPG,col=Origin))
mpg2 + geom_point()
typebar2 + geom_bar() + scale_fill_brewer(type='seq',palette=3)
mpg2 + geom_point(pch=16,cex=2.5) + scale_colour_brewer(type='qual',palette=3)

###
### Chart 0-2: Facet Grid
###

# Divide with "Origin" horizontal:
mpg + geom_point() + facet_grid(. ~ Origin)
# Divide with "Origin" vertical:
mpg + geom_point() + facet_grid(Origin ~ .)
# Divide with "Origin" vertical, "Type" horizontal
mpg + geom_point() + facet_grid(Origin ~ Type)

#Import buzzfeed headlines dataset:
df <- read.csv("./data/buzzfeed_headlines.csv")
head(df)


###
### Chart 1-1: Histogram of Listicle sizes
###

ggplot(df, aes(listicle_size)) + geom_histogram(binwidth=1)
ggsave("ggplot2_graphics_1.png", dpi=300, width=4, height=3)

###
### Chart 1-2: + Theme
###

ggplot(df, aes(listicle_size)) +
  geom_histogram(binwidth=1) +
  fte_theme()
ggsave("ggplot2_graphics_2.png", dpi=300, width=4, height=3)

###
### Chart 1-3: + Axis Labels
###

ggplot(df, aes(listicle_size)) +
  geom_histogram(binwidth=1) +
  fte_theme() +
  labs(title="Distribution of Listicle Sizes for BuzzFeed Listicles", x="# of Entries in Listicle", y="# of Listicles")
ggsave("ggplot2_graphics_3.png", dpi=300, width=4, height=3)

###
### Chart 1-4: + Final Tweaks
###

ggplot(df, aes(listicle_size)) +
  geom_histogram(binwidth=1, fill="#c0392b", alpha=0.75) +
  fte_theme() +
  labs(title="Distribution of Listicle Sizes for BuzzFeed Listicles", x="# of Entries in Listicle", y="# of Listicles") +
  scale_x_continuous(breaks=seq(0,50, by=5)) +
  scale_y_continuous(labels=comma) + 
  geom_hline(yintercept=0, size=0.4, color="black")
ggsave("ggplot2_graphics_4.png", dpi=300, width=4, height=3)


###
### Chart 2-1: Scatterplot of Shares vs. Likes
###

ggplot(df, aes(x=listicle_size, y=num_fb_shares)) +
  geom_point()
ggsave("ggplot2_graphics_5.png", dpi=300, width=4, height=3)
  
###
### Chart 2-2: Transparency + Log
###

ggplot(df, aes(x=listicle_size, y=num_fb_shares)) +
  geom_point(alpha=0.05) +
  scale_y_log10(labels=comma) 
ggsave("ggplot2_graphics_6.png", dpi=300, width=4, height=3)

###
### Chart 2-3: Theme + Axis
###

ggplot(df, aes(x=listicle_size, y=num_fb_shares)) +
  geom_point(alpha=0.05) +
  scale_y_log10(labels=comma) +
  fte_theme() +
  labs(x="# of Entries in Listicle", y="# of Facebook Shares", title="FB Shares vs. Listicle Size for BuzzFeed Listicles")
ggsave("ggplot2_graphics_7.png", dpi=300, width=4, height=3)

###
### Chart 2-4: Tidy Up
###
#Smoothing method="gam" if max(frequency)>1000. If <1000, method="loess".
ggplot(df, aes(x=listicle_size, y=num_fb_shares)) +
  geom_point(alpha=0.05, color="#c0392b") +
  scale_x_continuous(breaks=seq(0,50, by=5)) +
  scale_y_log10(labels=comma, breaks=10^(0:6)) +
  geom_hline(yintercept=1, size=0.4, color="black") +
  geom_smooth(method="gam", alpha=0.25, color="black", fill="black") +
  fte_theme() +
  labs(x="# of Entries in Listicle", y="# of Facebook Shares", 
       title="FB Shares vs. Listicle Size for BuzzFeed Listicles")  
ggsave("ggplot2_graphics_8.png", dpi=300, width=4, height=3)

