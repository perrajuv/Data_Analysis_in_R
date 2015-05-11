# Exercise 1 Solution:
#Import the med trial dataset:
med <- read.csv('/Users/atrivedi/R/Code/Dallas_Tutorial/data/MedicalData.csv')
View(med)
plot(med$Cholesterol, med$Glucose, main='Cholesterol and Glucose', 
     xlab = 'Cholesterol', ylab = 'Glucose', 
     pch = 20, col = 'red')
abline(v=mean(med$Cholesterol), col='blue', lwd=2)
abline(v=median(med$Cholesterol), col='dark green', lwd=2)
max_cholesterol <- which(med$Cholesterol == max(med$Cholesterol))
points(med$Cholesterol[max_cholesterol], 
       med$Glucose[max_cholesterol], 
       col = 'black', cex = 3)
legend(1500,180,
       c('Mean','Median','Highest'),
       pch=c(20,20),
       col=c('blue','dark green','black'))


# Exercise 2 Solution:
pdf('myboxplot.pdf')
boxplot(med$BMI~med$AgeGroup, 
        col=c('#0571b0','#ca0020', '#54278f'), 
        xlab='Age Group', 
        ylab= 'BMI')
dev.off()
