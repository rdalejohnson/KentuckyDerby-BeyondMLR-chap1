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

#another way to display head/tail rows
#table1 <- derby.df %>%
#  filter(row_number() < 6 | row_number() > 117)


###################################### UNIVARIATE ANALYSIS ####################################

#### Histogram of Winning Speeds #####

speed_hist <- ggplot(data = derby.df, aes(x = speed)) + 
  geom_histogram(binwidth = 0.5, fill = "white",
                 color = "black") + 
  xlab("Winning speed (ft/s)") + ylab("Frequency") + labs(title="(a)")

#### Histogram of # of starters in race #####

starters_hist <- ggplot(data = derby.df, aes(x = starters)) + 
  geom_histogram(binwidth = 3, fill = "white",
                 color = "black") + 
  xlab("Number of starters") + ylab("Frequency") + labs(title="(b)")

grid.arrange(speed_hist, starters_hist, ncol = 2)


summary(select(derby.df, -c(winner, year, yearnew, fast,  good, fastfactor)))


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

