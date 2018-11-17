library("readxl")
wine <- read_excel("wineQualityReds.xls")
View(wineQualityReds)

wine$quality <- factor(wine$quality, ordered = T)

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,levels = c('bad', 'average', 'good'))

str(wine)

summary(wine)

#plot the distribution of quality
ggplot(data = wine, aes(x = quality)) + geom_bar(width = 1, color = 'black',fill = I('orange'))

#plot the distribution of rating
ggplot(data = wine, aes(x = rating)) + geom_bar(width = 1, color = 'black',fill = I('blue'))

##Based on the plot diagram we have data set with average quality wine

## fixed acidity on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = fixed.acidity  )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(4,14)),
             ggplot(data = wine, aes(x = fixed.acidity )) +
               geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) + 
               scale_x_continuous(lim = c(4,14)),ncol = 2)


## volatile acidity on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = volatile.acidity ) ) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,1)),
             ggplot(data = wine, aes(x = volatile.acidity)) +
               geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,1)), ncol = 2)


## citric acidity on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = citric.acid )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = citric.acid)) +
               geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)

## residual.sugar on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = residual.sugar )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(1,8)),
             ggplot(data = wine, aes(x = residual.sugar)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(1,8)), ncol = 2)

## chlorides on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = chlorides )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,0.25)),
             ggplot(data = wine, aes(x = chlorides)) +
               geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,0.25)), ncol = 2)

## free.sulfur.dioxide on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = free.sulfur.dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,45)),
             ggplot(data = wine, aes(x = free.sulfur.dioxide)) +
               geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,80,5), lim = c(0,45)), ncol = 2)

## total.sulfur.dioxide on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = total.sulfur.dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,180)),
             ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
               geom_histogram(binwidth = 5, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,180)), ncol = 2)

## density on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = density)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = density)) +
               geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange')), ncol = 2)

## pH on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = pH)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = pH)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')), ncol = 2)


## sulphates on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = sulphates)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0.3,1.6)),
             ggplot(data = wine, aes(x = sulphates)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0.3,1.6)), ncol = 2)

## alcohol on the graph
grid.arrange(ggplot(wine, aes( x = 1, y = alcohol)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(8,14)),
             ggplot(data = wine, aes(x = alcohol)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(8,14)), ncol = 2)