library(dplyr)
##MALE##
## Visit 1 lactate (x) and Load (y)
vo2data <- data.frame(Lactate=c(1.5,1.1,1.6,3.3,5.0,6.9),
                   Load = c(0,50,100,150,200,250))
## Plot
library(ggplot2)
ggplot(data=vo2data,aes(Lactate,Load)) + geom_line(linetype="solid") +
  theme_bw()+ labs(title="Nonlinear Regression") + 
  xlab("Lactate (mmol/L)") + ylab("Power (watts)")

## 4th order Polynomial Regression Model
model <- lm(formula = Load ~ poly(Lactate,4), data=vo2data)
summary(model)

## Predicted Loads for Visit 2
predicted_load <- data.frame(Lactate=c(1.5,3.0,4.2))

## rounded predicted model to 0 decimal points
round(predict(model,predicted_load),digits=0)

## Plot (Flipped)
library(plotly)
library(ggplot2)
plot_male <- ggplot(data=vo2data,aes(Load,Lactate)) + geom_line(linetype="solid") +
  theme_classic()+ labs(title="Nonlinear Regression, 4th order polynomial regression") + 
  xlab("Power (watts)") + ylab("Lactate (mmol/L)")
ggplotly(plot_male)


#### FEMALE ####

library(dplyr)
## Visit 1 lactate (x) and Load (y)
vo2data <- data.frame(Lactate=c(.9,1.4,1.7,2.3,3.9,5.1,8.0),
                      Load = c(0,50,75,100,125,150,175))
## Plot
library(ggplot2)
ggplot(data=vo2data,aes(Lactate,Load)) + geom_line(linetype="solid") +
  theme_bw()+ labs(title="Nonlinear Regression") + 
  xlab("Lactate (mmol/L)") + ylab("Power (watts)")

## 4th order Polynomial Regression Model
model <- lm(formula = Load ~ poly(Lactate,4), data=vo2data)
summary(model)

## Predicted Loads for Visit 2
predicted_load <- data.frame(Lactate=c(1.7,3.0,4.5))

## rounded predicted model to 0 decimal points
round(predict(model,predicted_load),digits=0)

## Plot (Flipped)
library(plotly)
library(ggplot2)
plot_female <- ggplot(data=vo2data,aes(Load,Lactate)) + geom_line(linetype="solid") +
  theme_classic()+ labs(title="Nonlinear Regression, 4th order polynomial regression") + 
  xlab("Power (watts)") + ylab("Lactate (mmol/L)")
ggplotly(plot_female)