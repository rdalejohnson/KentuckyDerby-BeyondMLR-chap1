#Better, do-it-yourself residual vs fitted plot
#http://www.contrib.andrew.cmu.edu/~achoulde/94842/homework/regression_diagnostics.html

# Plot model residuals on y axis, fitted values on x axis
# Add red trend curve with better choice of smoothing bandwidth
qplot(y = lm.curved$residuals, x = lm.curved$fitted.values,
      ylab = "Residuals", xlab = "Fitted values", 
      main = "The Do-it-yourself Residuals vs. Fitted plot") +
  stat_smooth(method = "loess", span = 0.1, colour = I("red"), se = FALSE)