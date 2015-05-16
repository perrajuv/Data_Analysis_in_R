#---------------------------------#
#- Data Analysis & Graphics in R -#
#--------Simple Graphics------ ---#
#---------------------------------#

#Import the med trial dataset:
med <- read.csv('./data/MedicalData.csv')
head(med)
View(med)

#BMI and BP measurements#
# Explaining fivenum():
# five number summary (minimum, lower-hinge, median, upper-hinge, maximum)
# for the input data

mean(med$BMI)
median(med$BMI)
fivenum(med$BMI) 

mean(med$BP)
median(med$BP)
fivenum(med$BP)


#Scatterplots#

#Basic Scatterplot:
plot(med$BMI,med$BP)

#Export your plot:
pdf('mygraph.pdf')
plot(med$BMI,med$BP)
dev.off()

#We can add layers to plots to do a lot of different things...
#See http://www.statmethods.net/advgraphs/parameters.html for more options

#Adding options to plot():
plot(med$BMI, med$BP, main='BMI and Blood Pressure',
     xlab='BMI',ylab='Blood Pressure (mmHg)',
     pch=20)
plot(med$BMI,med$BP,main='BMI and Blood Pressure', 
     xlab = 'BMI', ylab = 'Blood Pressure', 
     pch = 20, col = 'red')
plot(med$BMI,med$BP,main='BMI and Blood Pressure', 
     xlab = 'BMI', ylab = 'Blood Pressure', 
     pch = 20, col = 'red', xlim = c(0,80))

#1. Play with pch 
plot(med$BMI,med$BP,main='BMI and Blood Pressure',
     xlab='BMI',ylab='Blood Pressure (mmHg)',
     pch=20,col='grey')
# points() draws a sequence of points
# centered at the specified coordinates.
points(med$BMI,med$BP,main='BMI and Blood Pressure',
       xlab='BMI',ylab='Blood Pressure (mmHg)')

#2. Color different groups and add a legend
# Group patients by gender:
females <- med[med$Gender=='Female',]
males <- med[med$Gender=='Male',]
plot(females$BMI,females$BP,main='BMI and Blood Pressure',
     xlab='BMI',ylab='Blood Pressure (mmHg)',
     pch=20,col='orange')
points(males$BMI,males$BP,
       pch=17,col='blue')
legend(45,70,
       c('Females','Males'),
       pch=c(20,17),
       col=c('orange','blue'))

#3. Add model fit lines or lines at meaningful values with abline()
plot(med$BMI,med$BP,main='BMI and Blood Pressure',
     xlab='BMI',ylab='Blood Pressure (mmHg)',
     pch=20)
abline(h=mean(med$BP),col='blue',lty=2)
abline(v=mean(med$BMI),col='dark green',lwd=4)
abline(lm(med$BP~med$BMI),col='red')

#Histograms#
hist(med$Cholesterol)

# Set a graphical parameter within the plotting function
hist(med$Cholesterol, col.lab="blue", xlim=c(0,2000))

#Adding options:
hist(med$Cholesterol,main='Histogram of Cholesterol',
     xlab='Cholesterol (mg/dL)',col='grey',
     xlim=c(0,2000))

#Grouped histograms#
# 8 bit color code - last 2 bits denote the transparency
hist(males$BP,main='Histogram of Blood Pressure',
     xlab='Blood Pressure (mmHg))',col='#31a35475',
     ylim=c(0,20),xlim=c(40,100))
# add=T lets layer one histogram over the other 
# color transparecy is needed here too.
hist(females$BP,col='#756bb175',add=T)


#Boxplots#
boxplot(med$Glucose,main='Saliva Glucose Levels',
        ylab='Glucose (mg/dL)')

# Boxplots make it easy to compare groups:
# here is a Boxplot of Glucose by Diabetic
boxplot(med$Glucose~med$Diabetic, main='Saliva Glucose Levels',
        ylab='Glucose (mg/dL)', xlab='Diabetic Status',
        names=c('No','Yes'), col=c('#0571b0','#ca0020'))



#Barplot - Displays for categorical variables#
# 1 is True, 0 is False
table(med$Diabetic)
barplot(table(med$Diabetic))

#Options are definitely needed:
barplot(table(med$Diabetic),main='Frequency of Diabetic Status',
        xlab='Diabetic Status',names=c('No','Yes'),
        ylab='Frequency',ylim=c(0,100),
        col=c('#018571','#b2abd2'))


# For two categorical variables:
# Diabetic Status by Gender
table(med$Diabetic,med$Gender)
a <- table(med$Diabetic,med$Gender)
barplot(a,ylim=c(0,100),main='Diabetic Status by Gender',
        ylab='Frequency',col=c('#018571','#b2abd2'))
#Option to add categories beside each other in barplot
barplot(a,main='Diabetic Status by Gender',
        ylab='Frequency',col=c('#018571','#b2abd2'),beside=T)
legend("topright", inset=.05,title="Status", c('No','Yes'),
       pch=c(20,20), col=c('#018571','#b2abd2'))



#Density plots#
x <- seq(-4,4,.1)
y <- dnorm(x) # gives the density function
degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")
plot(x, y, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)



#par() and layout()#
par()                      # view current settings
opar <- par()              # make a copy of current settings
par(col.lab="red")         # red x and y labels 
hist(med$Cholesterol)      # create a plot with these new settings
layout(matrix
      (1:4,ncol=2,byrow=TRUE))      # with layout(), you can tile your graphs
hist(med$Age)
hist(med$Cholesterol)
hist(med$BMI)
hist(med$BP)
par(opar)                  # restore original settings
