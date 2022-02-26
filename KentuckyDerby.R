#Chapter 1, Beyond Multiple Linear Regression by Paul Roback and Julie Legler
#Kentucky Derby example
#February 25, 2022

#Description of using grid and cowplot
  #http://www.sthda.com/english/wiki/wiki.php?id_contents=7930


library(tidyverse)  
library(ggplot2)
library("gridExtra")

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


################### UNIVARIATE ANALYSIS ###################

speed_hist <- ggplot(data = derby.df, aes(x = speed)) + 
  geom_histogram(binwidth = 0.5, fill = "white",
                 color = "black") + 
  xlab("Winning speed (ft/s)") + ylab("Frequency") + labs(title="(a)")

starters_hist <- ggplot(data = derby.df, aes(x = starters)) + 
  geom_histogram(binwidth = 3, fill = "white",
                 color = "black") + 
  xlab("Number of starters") + ylab("Frequency") + labs(title="(b)")

grid.arrange(speed_hist, starters_hist, ncol = 2)

########### categorical frequencies ##############

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
