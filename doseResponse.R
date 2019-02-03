
#https://stackoverflow.com/questions/44685484/issues-plotting-dose-response-curves-with-ggplot-and-glm

library(ggplot2)
library(Hmisc)
library(plyr)
library(MASS)

# create dataframe: 
  #1) column is exposure concentration
  #2) column is the total number of organism died over 12 h of exposure to the   corresponding concentration 
#3) column is the total number that survived over 12 h to the corresponding concentration
#4) column is the total number of organism exposed to the corresponding concentration
#5) fifth is the percentage of organism that survived exposure at the corresponding concentration 

conc <- c(0.02, 0.45, 0.46, 0.50, 0.78, 0.80, 0.80, 0.92, 0.93, 1.00, 1.16, 
          1.17, 1.17, 1.48,1.51, 1.55, 1.88, 1.90, 2.02)

dead <- c(0, 0,  0,  0,  0,  0,  0,  0,  0,  1,  7, 11, 4, 14, 14, 12, 12, 18, 17)

survive <- c(15, 16, 15, 15, 15, 16, 14, 14, 10, 15, 12,  5, 12,  0,  1,  3,  0,  0,  0)

total <- c(15, 16, 15, 15, 15, 16, 14, 14, 10, 16, 19, 16, 16, 14, 15, 15, 12, 18, 17)

perc <- c(1.00, 1.00, 1.00, 1.00, 1.00,1.00, 1.00, 1.00, 1.00, 0.94,0.63, 
          0.31,0.75,0.00, 0.07, 0.20, 0.00, 0.00,0.00)

data<-data.frame(conc,dead,survive,total,perc)
head(data)
attach(data)
#create matrix of dead and survival
y = cbind(dead,survive)

#create binomial glm (probit model)
model.results = glm(data = data, y ~ conc,binomial(link="probit"))
summary(model.results)



#use function from MASS to calculate LC
dose.p(model.results,p=0.5)
dose.p(model.results,p=c(0.1,0.25,0.5,0.99))

#plot curve 
plot(conc,(survive/(survive+dead)), ylab = "Percent Survival", 
     xlab="Concentration ")


#To make function use the Estimate parameters from the binomial glm 
# used above
logisticline <- function(z) {eta = -6.7421 + 5.4468 * z;1 / (1 + 
                                                               exp(eta))}
x <- seq(0,200.02,0.01)
lines(x,logisticline(x),new = TRUE)




#plot using ggplot

ggplot(data, aes(x = conc, y = perc)) +
  geom_point() +
  geom_smooth(method="glm",method.args = list(family = "binomial"))





# You can draw the fitted line with ggplot2 by making predictions from the model or by fitting the model directly with geom_smooth. To do the latter, you'll need to fit the model with the proportion dead as the response variable with total as the weights instead of using the matrix of successes and failures as the response variable.

#Using glm, fitting a model with a proportion plus weights looks like:
# Calculate proportion
data$prop = with(data, dead/total)

# create binomial glm (probit model)
model.results2 = glm(data = data, prop ~ conc, 
                     family = binomial(link="probit"), weights = total)


# You can predict with the dataset you have or, to make a smoother line, you can create a new dataset to predict with that has more values of conc as you did.

preddat = data.frame(conc = seq(0, 2.02, .01) )

# Now you can predict from the model via predict, using this data.frame as newdata. If you use type = "response", you will get predictions on the data scale via the inverse link. Because you fit a probit model, this will use the inverse probit. In your example you used the inverse logit for predictions.

# Predictions with inverse probit
preddat$pred = predict(model.results2, newdata = preddat, type = "response")
# Predictions with inverse logit (?)
preddat$pred2 = plogis( predict(model.results2, newdata = preddat) )

# To fit the probit model in ggplot, you will need to use the proportion as the y variable with weight = total. Here I add the lines from the model predictions so you can see the probit model fit in ggplot gives the same estimated line as the fitted probit model. Using the inverse logit gives you something different, which isn't surprising.

ggplot(data, aes(conc, prop) ) +
     geom_smooth(method = "glm", method.args = list(family = binomial(link = "probit") ), 
                 aes(weight = total, color = "geom_smooth line"), se = FALSE) +
     geom_line(data = preddat, aes(y = pred, color = "Inverse probit") ) +
     geom_line(data = preddat, aes(y = pred2, color = "Inverse logit" ) )

