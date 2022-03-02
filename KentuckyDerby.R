#Chapter 1, Beyond Multiple Linear Regression by Paul Roback and Julie Legler
#Kentucky Derby example
#February 25, 2022

#Description of using grid and cowplot
  #http://www.sthda.com/english/wiki/wiki.php?id_contents=7930


library(tidyverse)  
library(ggplot2)
library(GGally)
library(gridExtra)

derby.df <- read.csv(
  file = "derbyplus.csv",
  header = TRUE,
  sep = ",",
  dec = ".",  #indicates the decimal point in your language
  stringsAsFactors = TRUE #FALSE by default starting with R 4.0.0
)


head(derby.df) #first six rows
tail(derby.df) #last six rows

#add a small set of indicator/binary columns
#add a factor variable to make an indicator (fast) more readable

derby.df <- derby.df %>%
  mutate( fast = ifelse(condition=="fast",1,0), 
          good = ifelse(condition=="good",1,0),
          yearnew = year - 1896,
          fastfactor = ifelse(fast == 0, "not fast", "fast"))

head(derby.df) #first six rows
tail(derby.df) #last six rows

str(derby.df)


#another way to display head/tail rows
#table1 <- derby.df %>%
#  filter(row_number() < 6 | row_number() > 117)


###################################### UNIVARIATE ANALYSIS ####################################

############# DESCRIPTIVE STATISTICS #########################
#https://rpubs.com/bhagirathi_dash/ggplot_continous_outcome

summary(select(derby.df, -c(winner, year, yearnew, fast,  good, fastfactor)))

sqrt(count(derby.df))

leanDerby <- select(derby.df, -c(winner, year, yearnew, fast,  good, fastfactor))

by(leanDerby, leanDerby$condition,  summary)

library(pacman)
library(MASS)
library(psych)

t(describe(derby.df[c(4)])[, -1])

#density plots in a grid
grid.arrange(
  
  ggplot(data=derby.df) +
    geom_density(mapping=aes(x=speed), color="blue"),
  
  ggplot(data=derby.df, mapping=aes(y=speed, x=1)) +
    geom_boxplot(fill = "white", colour = "#3366FF", outlier.colour = "red") +
    geom_text(aes(label = ifelse(speed %in% 
                                   boxplot.stats(speed)$out,
                                 as.character(speed), "")), hjust = 1.5),
  # 
  # ggplot(data=tbwt) +
  #   geom_density(mapping=aes(x=bwt), color="red"),
  
  nrow=1,
  
  top="Density plots: Univariates")

############ VISUAL DESCRIPTIVE ###########################


ggplot(derby.df, aes(y = speed, x = year)) + 
  geom_jitter(position = position_jitter(height = 0, width = 0.2))


ggplot(derby.df, aes(y = starters, x = year)) + 
  geom_jitter(position = position_jitter(height = 0, width = 0.2))


hist(derby.df$speed)

#rule of thumb, Sturges' law, number of bins should
#be rounded value of the square root of the # of
#observations
#Example: 150 observations, sqrt = 12.24745, so 12 bins

histogram1 <- ggplot(derby.df) +
  aes(x = speed) +
  geom_histogram(fill = "white", 
                 color = "black",
                 bins=round(sqrt(count(derby.df)))) + 
  annotate("text", x = c(50, 50), y= c(20, 22), 
            label =  c(paste("Bin size:", as.character(round(sqrt(count(derby.df))))),
               paste("Observations:", as.character(count(derby.df))))
          ) +
  theme(panel.grid = element_line(color = "white",
                                  size = 0.75,
                                  linetype = 1))


histogram1


#### Histogram of Winning Speeds #####

speed_hist <- ggplot(data = derby.df, aes(x = speed)) + 
  geom_histogram(binwidth = 0.5, fill = "white",
                 color = "black") + 
  xlab("Winning speed (ft/s)") + ylab("Frequency") + labs(title="(a)")

speed_hist

#### Histogram of # of starters in race #####

starters_hist <- ggplot(data = derby.df, aes(x = starters)) + 
  geom_histogram(binwidth = 3, fill = "white",
                 color = "black") + 
  xlab("Number of starters") + ylab("Frequency") + labs(title="(b)")

grid.arrange(speed_hist, starters_hist, ncol = 2)




########### Racetrack Condition frequencies/histogram/treemap ##############

#frequency table with percentages
library(summarytools)
freqTableTrackConditions <- as.data.frame.matrix(summarytools::freq(derby.df$condition, order = "freq"))

# HISTOGRAM

library(ggplot2)
ggplot(derby.df, aes(x = condition)) +  geom_bar()

# TREEMAP

conditionFrequencies <-setNames(data.frame(table(derby.df$condition)), c("Track Condition", "Freq"))

library(treemap)

treemap(conditionFrequencies,
        
        # data
        index="Track Condition",
        vSize="Freq",
        type="index",
        
        # Main
        title="Track Conditions",
        palette="Dark2",
        
        # Borders:
        border.col=c("black"),             
        border.lwds=1,                         
        
        # Labels
        fontsize.labels=18,
        fontcolor.labels="white",
        fontface.labels=1         
        # #bg.labels=c("transparent"),              
        # align.labels=c("left", "top"),                                  
        # overlap.labels=0.5,
        #inflate.labels=T # If true, labels are bigger when rectangle is bigger.
)

gfg_plot <- ggplot(derby.df, aes(x = year, colour = condition)) +  
  #geom_point(aes(y = speed), color = "black") +
  geom_point(aes(y = starters), color = "red") +
  #geom_smooth(aes(linetype = condition, y=speed), method = lm, se = FALSE) +
  geom_smooth(aes(linetype = condition, y=starters), method = lm, se = FALSE)

  #geom_line(aes(y = fastfactor), color = "green") 
  #geom_line(aes(y = y4), color = "blue") +
  #geom_line(aes(y = y5), color = "purple")
gfg_plot

gfg_plot2 <- ggplot(derby.df, aes(x = year, colour = condition)) +  
  geom_point(aes(y = speed), color = "black") +
  #geom_point(aes(y = starters), color = "red") +
  geom_smooth(aes(linetype = condition, y=speed), method = lm, se = FALSE)
  #geom_smooth(aes(linetype = condition, y=starters), method = lm, se = FALSE)

#geom_line(aes(y = fastfactor), color = "green") 
#geom_line(aes(y = y4), color = "blue") +
#geom_line(aes(y = y5), color = "purple")
gfg_plot2

############################## BIVARIATE ANALYSIS ###################################

#ggpairs produces a matrix of scatter plots for visualizing correlation
#   data: data set. Can have both numerical and categorical data.
#   columns: columns to be used for the plots. Default is all columns.
#   title: title for the graph
#   axisLabels: Allowed values are either “show” to display axisLabels, “internal” for labels in the diagonal plots, or “none” for no axis labels
#   columnLabels: label names to be displayed. Defaults to names of columns being used.



gg <- ggpairs(data = derby.df, title="Kentucky Derby Bivariate Analysis",
              columns = c("condition", "year", "starters", "speed"))
gg[4,1] <- gg[4,1] + geom_histogram( binwidth = 0.75)
gg[2,1] <- gg[2,1] + geom_histogram( binwidth = 20)
gg[3,1] <- gg[3,1] + geom_histogram( binwidth = 3)
gg


library(lares)

#Negatives in red/Positives in blue

corr_cross(derby.df, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 20 # display top X couples of variables (by correlation coefficient)
)

ggplot(derby.df, aes(x = year, y = speed, colour = fastfactor)) +
  geom_point(aes(shape = fastfactor)) +
  geom_smooth(aes(linetype = fastfactor), method = lm, se = FALSE)


####################### SIMPLE LINEAR MODELS ################

model1 <- lm(speed ~ year, data = derby.df)

coef(summary(model1))
cat(" R squared = ", summary(model1)$r.squared, "\n", 
    "Residual standard error = ", summary(model1)$sigma)

###center so that the intercept is more meaningful
#this only changes the intercept, not the year coefficient or
#anything else

model2 <- lm(speed ~ yearnew, data = derby.df)

coef(summary(model2))
cat(" R squared = ", summary(model2)$r.squared, "\n", 
    "Residual standard error = ", summary(model2)$sigma)

ggplot(derby.df, aes(x = year, y = speed)) +
  geom_point() + xlim(0,2020) + ylim(0,55) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_vline(xintercept = 1896, linetype = 2) +
  geom_segment(aes(x = 100, y = 0, xend = 1800, yend = 0), 
               arrow = arrow(length = unit(0.5, "cm")))


# Residual diagnostics for Model 2
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

