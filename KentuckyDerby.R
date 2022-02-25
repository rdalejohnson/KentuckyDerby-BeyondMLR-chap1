#Chapter 1, Beyond Multiple Linear Regression by Paul Roback and Julie Legler
#Kentucky Derby example
#February 25, 2022

#Description of using grid and cowplot
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930

library(tidyverse)  
library(ggplot2)
#install.packages("gridExtra")
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

#add a set of descriptive/categorical columns

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



speed_hist <- ggplot(data = derby.df, aes(x = speed)) + 
  geom_histogram(binwidth = 0.5, fill = "white",
                 color = "black") + 
  xlab("Winning speed (ft/s)") + ylab("Frequency") + labs(title="(a)")

starters_hist <- ggplot(data = derby.df, aes(x = starters)) + 
  geom_histogram(binwidth = 3, fill = "white",
                 color = "black") + 
  xlab("Number of starters") + ylab("Frequency") + labs(title="(b)")

grid.arrange(speed_hist, starters_hist, ncol = 2)


