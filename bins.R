library(ggplot2)
library(reshape2)
library("readxl")
library('stringr')
attach(mtcars)

setwd('/home/noble_mannu/Documents/PhD/Research/Fall_2020')
load('bins.RData')

breaks <- seq(from = -2, to = 7, by =0.5)

par(mfrow=c(2,2))
hist(bin.1.1[,2], breaks = breaks, main = 'Charge +2, Bin (0,0.1]',
     xlab = 'Intensity')
hist(bin.1.2[,2], breaks = breaks, main = 'Charge +2, Bin (0.1,0.2]',
     xlab = 'Intensity')
hist(bin.1.3[,2], breaks = breaks, main = 'Charge +2, Bin (0.2,0.3]',
     xlab = 'Intensity')
hist(bin.1.4[,2], breaks = breaks, main = 'Charge +2, Bin (0.3,0.4]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.1.5[,2], breaks = breaks, main = 'Charge +2, Bin (0.4,0.5]',
     xlab = 'Intensity')
hist(bin.1.6[,2], breaks = breaks, main = 'Charge +2, Bin (0.5,0.6]',
     xlab = 'Intensity')
hist(bin.1.7[,2], breaks = breaks, main = 'Charge +2, Bin (0.6,0.7]',
     xlab = 'Intensity')
hist(bin.1.8[,2], breaks = breaks, main = 'Charge +2, Bin (0.7,0.8]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.1.9[,2], breaks = breaks, main = 'Charge +2, Bin (0.8,0.9]',
     xlab = 'Intensity')
hist(bin.1.10[,2], breaks = breaks, main = 'Charge +2, Bin (0.9,1]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.2.1[,2], breaks = breaks, main = 'Charge +3, Bin (0,0.1]',
     xlab = 'Intensity')
hist(bin.2.2[,2], breaks = breaks, main = 'Charge +3, Bin (0.1,0.2]',
     xlab = 'Intensity')
hist(bin.2.3[,2], breaks = breaks, main = 'Charge +3, Bin (0.2,0.3]',
     xlab = 'Intensity')
hist(bin.2.4[,2], breaks = breaks, main = 'Charge +3, Bin (0.3,0.4]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.2.5[,2], breaks = breaks, main = 'Charge +3, Bin (0.4,0.5]',
     xlab = 'Intensity')
hist(bin.2.6[,2], breaks = breaks, main = 'Charge +3, Bin (0.5,0.6]',
     xlab = 'Intensity')
hist(bin.2.7[,2], breaks = breaks, main = 'Charge +3, Bin (0.6,0.7]',
     xlab = 'Intensity')
hist(bin.2.8[,2], breaks = breaks, main = 'Charge +3, Bin (0.7,0.8]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.2.9[,2], breaks = breaks, main = 'Charge +3, Bin (0.8,0.9]',
     xlab = 'Intensity')
hist(bin.2.10[,2], breaks = breaks, main = 'Charge +3, Bin (0.9,1]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.3.1[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0,0.1]',
     xlab = 'Intensity')
hist(bin.3.2[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.1,0.2]',
     xlab = 'Intensity')
hist(bin.3.3[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.2,0.3]',
     xlab = 'Intensity')
hist(bin.3.4[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.3,0.4]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.3.5[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.4,0.5]',
     xlab = 'Intensity')
hist(bin.3.6[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.5,0.6]',
     xlab = 'Intensity')
hist(bin.3.7[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.6,0.7]',
     xlab = 'Intensity')
hist(bin.3.8[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.7,0.8]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
hist(bin.3.9[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.8,0.9]',
     xlab = 'Intensity')
hist(bin.3.10[,2], breaks = breaks, main = 'Charge +4~+6, Bin (0.9,1]',
     xlab = 'Intensity')

par(mfrow=c(2,2))
relative1 <- c( dim(bin.1.1)[1], dim(bin.1.2)[1], dim(bin.1.3)[1],
                dim(bin.1.4)[1], dim(bin.1.5)[1], dim(bin.1.6)[1],
                dim(bin.1.7)[1], dim(bin.1.8)[1], dim(bin.1.9)[1],
                dim(bin.1.10)[1] )
names(relative1) <- seq(from =1, to =10, by =1)

barplot(height = relative1, ylab = 'Peaks', xlab = 'Bin', 
        main = 'Relative number of peaks by bin (Charge +2)')

relative2 <- c( dim(bin.2.1)[1], dim(bin.2.2)[1], dim(bin.2.3)[1],
                dim(bin.2.4)[1], dim(bin.2.5)[1], dim(bin.2.6)[1],
                dim(bin.2.7)[1], dim(bin.2.8)[1], dim(bin.2.9)[1],
                dim(bin.2.10)[1] )
names(relative2) <- seq(from =1, to =10, by =1)

barplot(height = relative2, ylab = 'Peaks', xlab = 'Bin', 
        main = 'Relative number of peaks by bin (Charge +3)')

relative3 <- c( dim(bin.3.1)[1], dim(bin.3.2)[1], dim(bin.3.3)[1],
                dim(bin.3.4)[1], dim(bin.3.5)[1], dim(bin.3.6)[1],
                dim(bin.3.7)[1], dim(bin.3.8)[1], dim(bin.3.9)[1],
                dim(bin.3.10)[1] )
names(relative3) <- seq(from =1, to =10, by =1)

barplot(height = relative3, ylab = 'Peaks', xlab = 'Bin', 
        main = 'Relative number of peaks by bin (Charge +4~+6)')

n.peaks.1 <- sum(relative1)
expected1 <- n.peaks.1/10
stat1 <- sum((relative1 - expected1)^2/expected1)
chi1 <- qchisq(0.95, df = 9)
stat1 > chi1
pv1 <- pchisq(stat1, df = 9, lower.tail = FALSE)

n.peaks.2 <- sum(relative2)
expected2 <- n.peaks.2/10
stat2 <- sum((relative2 - expected2)^2/expected2)
chi2 <- qchisq(0.95, df = 9)
stat2 > chi2
pv2 <- pchisq(stat2, df = 9, lower.tail = FALSE)

n.peaks.3 <- sum(relative3)
expected3 <- n.peaks.3/10
stat3 <- sum((relative3 - expected3)^2/expected3)
chi3 <- qchisq(0.95, df = 9)
stat3 > chi3
pv3 <- pchisq(stat3, df = 9, lower.tail = FALSE)

temp.1.1 <- density(bin.1.1[,2])
df.1.1 <- data.frame('x' = temp.1.1$x, 'y' = temp.1.1$y)
temp.1.2 <- density(bin.1.2[,2])
df.1.2 <- data.frame('x' = temp.1.2$x, 'y' = temp.1.2$y)
temp.1.3 <- density(bin.1.3[,2])
df.1.3 <- data.frame('x' = temp.1.3$x, 'y' = temp.1.3$y)
temp.1.4 <- density(bin.1.4[,2])
df.1.4 <- data.frame('x' = temp.1.4$x, 'y' = temp.1.4$y)
temp.1.5 <- density(bin.1.5[,2])
df.1.5 <- data.frame('x' = temp.1.5$x, 'y' = temp.1.5$y)
temp.1.6 <- density(bin.1.6[,2])
df.1.6 <- data.frame('x' = temp.1.6$x, 'y' = temp.1.6$y)
temp.1.7 <- density(bin.1.7[,2])
df.1.7 <- data.frame('x' = temp.1.7$x, 'y' = temp.1.7$y)
temp.1.8 <- density(bin.1.8[,2])
df.1.8 <- data.frame('x' = temp.1.8$x, 'y' = temp.1.8$y)
temp.1.9 <- density(bin.1.9[,2])
df.1.9 <- data.frame('x' = temp.1.9$x, 'y' = temp.1.9$y)
temp.1.10 <- density(bin.1.10[,2])
df.1.10 <- data.frame('x' = temp.1.10$x, 'y' = temp.1.10$y)

ggplot()+ geom_line(data = df.1.1, aes(x=x,y=y, colour = 'Bin 1')) +
        geom_line(data = df.1.2, aes(x=x,y=y, colour = 'Bin 2')) +
        geom_line(data = df.1.3, aes(x=x,y=y, colour = 'Bin 3')) +
        geom_line(data = df.1.4, aes(x=x,y=y, colour = 'Bin 4')) +
        geom_line(data = df.1.5, aes(x=x,y=y, colour = 'Bin 5')) +
        geom_line(data = df.1.6, aes(x=x,y=y, colour = 'Bin 6')) +
        geom_line(data = df.1.7, aes(x=x,y=y, colour = 'Bin 7')) +
        geom_line(data = df.1.8, aes(x=x,y=y, colour = 'Bin 8')) +
        geom_line(data = df.1.9, aes(x=x,y=y, colour = 'Bin 9')) +
        geom_line(data = df.1.10, aes(x=x,y=y, colour = 'Bin 10'))+
        labs(title = 'Bin densities Charge +2', x = 'Intensity',
             y = 'Density', color = 'Function')+
        theme(plot.title = element_text(hjust = 0.5))

temp.2.1 <- density(bin.2.1[,2])
df.2.1 <- data.frame('x' = temp.2.1$x, 'y' = temp.2.1$y)
temp.2.2 <- density(bin.2.2[,2])
df.2.2 <- data.frame('x' = temp.2.2$x, 'y' = temp.2.2$y)
temp.2.3 <- density(bin.2.3[,2])
df.2.3 <- data.frame('x' = temp.2.3$x, 'y' = temp.2.3$y)
temp.2.4 <- density(bin.2.4[,2])
df.2.4 <- data.frame('x' = temp.2.4$x, 'y' = temp.2.4$y)
temp.2.5 <- density(bin.2.5[,2])
df.2.5 <- data.frame('x' = temp.2.5$x, 'y' = temp.2.5$y)
temp.2.6 <- density(bin.2.6[,2])
df.2.6 <- data.frame('x' = temp.2.6$x, 'y' = temp.2.6$y)
temp.2.7 <- density(bin.2.7[,2])
df.2.7 <- data.frame('x' = temp.2.7$x, 'y' = temp.2.7$y)
temp.2.8 <- density(bin.2.8[,2])
df.2.8 <- data.frame('x' = temp.2.8$x, 'y' = temp.2.8$y)
temp.2.9 <- density(bin.2.9[,2])
df.2.9 <- data.frame('x' = temp.2.9$x, 'y' = temp.2.9$y)
temp.2.10 <- density(bin.2.10[,2])
df.2.10 <- data.frame('x' = temp.2.10$x, 'y' = temp.2.10$y)

ggplot()+ geom_line(data = df.2.1, aes(x=x,y=y, colour = 'Bin 1')) +
        geom_line(data = df.2.2, aes(x=x,y=y, colour = 'Bin 2')) +
        geom_line(data = df.2.3, aes(x=x,y=y, colour = 'Bin 3')) +
        geom_line(data = df.2.4, aes(x=x,y=y, colour = 'Bin 4')) +
        geom_line(data = df.2.5, aes(x=x,y=y, colour = 'Bin 5')) +
        geom_line(data = df.2.6, aes(x=x,y=y, colour = 'Bin 6')) +
        geom_line(data = df.2.7, aes(x=x,y=y, colour = 'Bin 7')) +
        geom_line(data = df.2.8, aes(x=x,y=y, colour = 'Bin 8')) +
        geom_line(data = df.2.9, aes(x=x,y=y, colour = 'Bin 9')) +
        geom_line(data = df.2.10, aes(x=x,y=y, colour = 'Bin 10'))+
        labs(title = 'Bin densities Charge +3', x = 'Intensity',
             y = 'Density', color = 'Function')+
        theme(plot.title = element_text(hjust = 0.5))

temp.3.1 <- density(bin.3.1[,2])
df.3.1 <- data.frame('x' = temp.3.1$x, 'y' = temp.3.1$y)
temp.3.2 <- density(bin.3.2[,2])
df.3.2 <- data.frame('x' = temp.3.2$x, 'y' = temp.3.2$y)
temp.3.3 <- density(bin.3.3[,2])
df.3.3 <- data.frame('x' = temp.3.3$x, 'y' = temp.3.3$y)
temp.3.4 <- density(bin.3.4[,2])
df.3.4 <- data.frame('x' = temp.3.4$x, 'y' = temp.3.4$y)
temp.3.5 <- density(bin.3.5[,2])
df.3.5 <- data.frame('x' = temp.3.5$x, 'y' = temp.3.5$y)
temp.3.6 <- density(bin.3.6[,2])
df.3.6 <- data.frame('x' = temp.3.6$x, 'y' = temp.3.6$y)
temp.3.7 <- density(bin.3.7[,2])
df.3.7 <- data.frame('x' = temp.3.7$x, 'y' = temp.3.7$y)
temp.3.8 <- density(bin.3.8[,2])
df.3.8 <- data.frame('x' = temp.3.8$x, 'y' = temp.3.8$y)
temp.3.9 <- density(bin.3.9[,2])
df.3.9 <- data.frame('x' = temp.3.9$x, 'y' = temp.3.9$y)
temp.3.10 <- density(bin.3.10[,2])
df.3.10 <- data.frame('x' = temp.3.10$x, 'y' = temp.3.10$y)

ggplot()+ geom_line(data = df.3.1, aes(x=x,y=y, colour = 'Bin 1')) +
        geom_line(data = df.3.2, aes(x=x,y=y, colour = 'Bin 2')) +
        geom_line(data = df.3.3, aes(x=x,y=y, colour = 'Bin 3')) +
        geom_line(data = df.3.4, aes(x=x,y=y, colour = 'Bin 4')) +
        geom_line(data = df.3.5, aes(x=x,y=y, colour = 'Bin 5')) +
        geom_line(data = df.3.6, aes(x=x,y=y, colour = 'Bin 6')) +
        geom_line(data = df.3.7, aes(x=x,y=y, colour = 'Bin 7')) +
        geom_line(data = df.3.8, aes(x=x,y=y, colour = 'Bin 8')) +
        geom_line(data = df.3.9, aes(x=x,y=y, colour = 'Bin 9')) +
        geom_line(data = df.3.10, aes(x=x,y=y, colour = 'Bin 10'))+
        labs(title = 'Bin densities Charge +4~+6', x = 'Intensity',
             y = 'Density', color = 'Function')+
        theme(plot.title = element_text(hjust = 0.5))

library(ggpubr)

A<- ggplot()+ geom_line(data = df.1.1, aes(x=x,y=y, colour = 'Bin 1')) +
        geom_line(data = df.1.2, aes(x=x,y=y, colour = 'Bin 2')) +
        geom_line(data = df.1.3, aes(x=x,y=y, colour = 'Bin 3')) +
        geom_line(data = df.1.4, aes(x=x,y=y, colour = 'Bin 4')) +
        geom_line(data = df.1.5, aes(x=x,y=y, colour = 'Bin 5')) +
        geom_line(data = df.1.6, aes(x=x,y=y, colour = 'Bin 6')) +
        geom_line(data = df.1.7, aes(x=x,y=y, colour = 'Bin 7')) +
        geom_line(data = df.1.8, aes(x=x,y=y, colour = 'Bin 8')) +
        geom_line(data = df.1.9, aes(x=x,y=y, colour = 'Bin 9')) +
        geom_line(data = df.1.10, aes(x=x,y=y, colour = 'Bin 10'))+
        labs(title = 'Bin densities Charge +2', x = 'Intensity',
             y = 'Density', color = 'Function')+
        theme(plot.title = element_text(hjust = 0.5))

B <- ggplot()+ geom_line(data = df.2.1, aes(x=x,y=y, colour = 'Bin 1')) +
        geom_line(data = df.2.2, aes(x=x,y=y, colour = 'Bin 2')) +
        geom_line(data = df.2.3, aes(x=x,y=y, colour = 'Bin 3')) +
        geom_line(data = df.2.4, aes(x=x,y=y, colour = 'Bin 4')) +
        geom_line(data = df.2.5, aes(x=x,y=y, colour = 'Bin 5')) +
        geom_line(data = df.2.6, aes(x=x,y=y, colour = 'Bin 6')) +
        geom_line(data = df.2.7, aes(x=x,y=y, colour = 'Bin 7')) +
        geom_line(data = df.2.8, aes(x=x,y=y, colour = 'Bin 8')) +
        geom_line(data = df.2.9, aes(x=x,y=y, colour = 'Bin 9')) +
        geom_line(data = df.2.10, aes(x=x,y=y, colour = 'Bin 10'))+
        labs(title = 'Bin densities Charge +3', x = 'Intensity',
             y = 'Density', color = 'Function')+
        theme(plot.title = element_text(hjust = 0.5))

C <- ggplot()+ geom_line(data = df.3.1, aes(x=x,y=y, colour = 'Bin 1')) +
        geom_line(data = df.3.2, aes(x=x,y=y, colour = 'Bin 2')) +
        geom_line(data = df.3.3, aes(x=x,y=y, colour = 'Bin 3')) +
        geom_line(data = df.3.4, aes(x=x,y=y, colour = 'Bin 4')) +
        geom_line(data = df.3.5, aes(x=x,y=y, colour = 'Bin 5')) +
        geom_line(data = df.3.6, aes(x=x,y=y, colour = 'Bin 6')) +
        geom_line(data = df.3.7, aes(x=x,y=y, colour = 'Bin 7')) +
        geom_line(data = df.3.8, aes(x=x,y=y, colour = 'Bin 8')) +
        geom_line(data = df.3.9, aes(x=x,y=y, colour = 'Bin 9')) +
        geom_line(data = df.3.10, aes(x=x,y=y, colour = 'Bin 10'))+
        labs(title = 'Bin densities Charge +4~+6', x = 'Intensity',
             y = 'Density', color = 'Function')+
        theme(plot.title = element_text(hjust = 0.5))

ggarrange(A, B, C, ncol = 2, nrow = 2)

hist(bin.1.1[,2], breaks = breaks, main = 'Charge +2, Bin (0,0.1]',
     xlab = 'Intensity', probability = TRUE)
lines(density(bin.1.1[,2]))
test <- density(bin.1.1[,2])
plot(test$x,test$y, type = 'l')
lines(density(bin.1.1[,2]), color = 'red')
