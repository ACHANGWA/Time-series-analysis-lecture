library(ggplot2)
library(tidyverse)
library(extrafont)
library(patchwork)
library(scales)

#import the data

estimatesf<-read_csv("C:/Users/ACHANGWA CHIARA/Desktop/my folder/fertility.csv") # Please use the dataset attached
colnames(estimatesf)

# the factual model

cg1 <- lm(fertility ~ t + family_plan+childgrant+t_family+t_child, data=estimatesf)
cg1

estimatesf$fit_cg1 = cg1$fitted.values
family_2025=estimatesf$t_family 
child_2025=estimatesf$t_child

datanew1 <- as.data.frame(cbind(year=rep(1975:2022),
                                t = rep(1 : 48), 
                                family_plan = rep(0),
                                childgrant = rep(0),
                                t_family=family_2025,
                                t_child=child_2025)) 

fit_cg_result1=predict(cg1,datanew1,interval='confidence')
fit_cg_result1
datanew1=cbind(datanew1,fit_cg_result1)
estimatesf=cbind(estimatesf,fit_cg_result1)

# Set up the plot frame
plot(estimatesf$year, estimatesf$fertility, type = "n",
     xlim = c(1975, 2022), ylim = c(0, 4),
     xlab = "Year", ylab = "Fertility rate",
     xaxt = "n", yaxt = "n")

# Customize axes
axis(1, at = seq(1975, 2022, 5), cex.axis = 1.2)
axis(2, at = seq(-2, 4, 1), cex.axis = 1.2)

# Add background and grid lines manually if needed (optional)
# abline(h=seq(0, 4, 1), col="gray90", lty=3)

# 1. Points (dotted time series)
points(estimatesf$year, estimatesf$fertility, pch = 21, bg = "white", col = "black")

# 2. Fitted line before 1991
lines(estimatesf$year[estimatesf$year < 1991],
      estimatesf$fit_cg1[estimatesf$year < 1991],
      col = "black", lwd = 2, lty = 1)

# 3. Fitted line after 1992
lines(estimatesf$year[estimatesf$year > 1992],
      estimatesf$fit_cg1[estimatesf$year > 1992],
      col = "black", lwd = 2, lty = 1)

# 4. Counterfactual line
lines(datanew1$year[19:48], datanew1$fit[19:48],
      col = "darkorange2", lwd = 2, lty = "longdash")

# 5. Confidence interval ribbon
polygon(c(datanew1$year[19:48], rev(datanew1$year[19:48])),
        c(datanew1$lwr[19:48], rev(datanew1$upr[19:48])),
        col = rgb(255, 140, 0, alpha = 0.15 * 255, maxColorValue = 255),
        border = NA)

# 6. Vertical lines
abline(v = 1993, col = "dodgerblue3", lty = "dashed")
abline(v = 2004, col = "dodgerblue3", lty = "dashed")

# 7. Horizontal dashed line at 2.1
abline(h = 2.1, col = "grey60", lty = "dashed", lwd = 0.6)

# 8. Text annotations (customize font and size as needed)
text(2015, 3.2, " - Fitted values", adj = 0, col = "black", cex = 1.2)
text(2015, 2.8, " - Counterfactual", adj = 0, col = "darkorange2", cex = 1.2)

# Optional:
# text(1991.5, 3, "Discontinuing\nFamily Planning", adj = 0, col = "firebrick", cex = 1)
# text(2004.5, 3, "Birth\nEncouragement Policy", adj = 0, col = "firebrick", cex = 1)

# Box around plot
box()
