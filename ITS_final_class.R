#### Interrupted ITS
library(tidyverse)
library(nlme)
library(AICcmodavg)

## ITS model

data <-  read_csv("controlled_its_dataset.csv")
head(data)

#Model factual scenario
model.a = gls(Number_of_IFV_cases ~ Time + Intervention + Post_intervention_time, data = data,method="ML")

# Show a summary of the model
summary(model.a)

#  Add predicted values and standard errors
data <-data %>% mutate(
  model.a.predictions = predictSE.gls (model.a, data, se.fit=T)$fit,
  model.a.se = predictSE.gls (model.a, data, se.fit=T)$se
)

#plot time series
plot(data$Time, data$Number_of_IFV_cases,
     pch = 16, col = rgb(0, 0, 0, 0.3), # semi-transparent black
     xlab = "Time", ylab = "Number of IFV Cases",
     main = "ITS Model with 95% CI")

# Add the confidence ribbon using polygon
polygon(c(data$Time, rev(data$Time)),
        c(data$model.a.predictions - 1.96 * data$model.a.se,
          rev(data$model.a.predictions + 1.96 * data$model.a.se)),
        col = "lightgreen", border = NA)


# Add the model prediction line
lines(data$Time, data$model.a.predictions, col = "black", lty = 1)

##### Counterfactual model
model.b = gls(Number_of_IFV_cases ~ Time + Intervention + Post_intervention_time, data = data,method="ML", correlation= corARMA(p=2,q=2, form = ~ Time))
model.b
data<- data %>% 
  mutate(
    model.b.predictions = predictSE.gls (model.b, data, se.fit=T)$fit,
    model.b.se = predictSE.gls (model.b, data, se.fit=T)$se
  )
df2<-filter(data,Time<51)
model.c = gls(Number_of_IFV_cases ~ Time, data = df2, correlation= corARMA(p=1, q=1, form = ~ Time),method="ML")
coefficients(model.c)
data<-data %>% mutate(
  model.c.predictions = predictSE.gls (model.c, newdata = data, se.fit=T)$fit,
  model.c.se = predictSE.gls (model.c, data, se.fit=T)$se
)

# Set up the plot with observed data points
plot(data$Time, data$Number_of_IFV_cases,
     pch = 16, col = rgb(0, 0, 0, 0.3), # semi-transparent black points
     xlab = "Time", ylab = "Quantity",
     main = "ITS Model Comparison with 95% CI",
     ylim = range(c(data$Number_of_IFV_cases,
                    data$model.b.predictions + 1.96 * data$model.b.se,
                    data$model.b.predictions - 1.96 * data$model.b.se,
                    data$model.c.predictions + 1.96 * data$model.c.se,
                    data$model.c.predictions - 1.96 * data$model.c.se)))

# Add the green ribbon for model B
polygon(c(data$Time, rev(data$Time)),
        c(data$model.b.predictions - 1.96 * data$model.b.se,
          rev(data$model.b.predictions + 1.96 * data$model.b.se)),
        col = "lightgreen", border = NA)

# Add the pink ribbon for model C
polygon(c(data$Time, rev(data$Time)),
        c(data$model.c.predictions - 1.96 * data$model.c.se,
          rev(data$model.c.predictions + 1.96 * data$model.c.se)),
        col = "pink", border = NA) 

# Add the black line for model B predictions
lines(data$Time, data$model.b.predictions, col = "black", lty = 1)

# Add the red dashed line for model C predictions
lines(data$Time, data$model.c.predictions, col = "red", lty = 2)





