
# install.packages("drc")


# Example 1: A cautionary example  -----
# We start out fitting a five-parameter log-logistic dose-response model to an artificial dataset consisting of two dose values and corresponding response values.
dose <- 1:2
resp <- rnorm(length(dose))
# no need to set the seed for the random number generation!
resp
## [1] 0.1934897 1.3011434
library(drc)
m <- drm(resp ~ dose, fct = LL.5())
summary(m)
# Standard errors close to 0 reflect that the dose-response model fitted the two points perfectly as is also seen in the figure, just like a straight line will always fit exactly through two points as well. However, both these examples illustrate overfitting: the statistical models are too complex in view of the limited available data.
plot(m)



# Example 2: Continuous response: one dose-response curve  ----
# We consider dose-response data on the effect of a herbicide in growth of perennial ryegrass (Inderjit et al., 2002). 
ryegrass.LL.4 <- drm(rootl ~ conc, data = ryegrass, fct = LL.3())
summary(ryegrass.LL.4)
# Next, we use the R packages lmtest and sandwich to obtain robust standard errors to address the fact that some variance heterogeneity is present.
library(sandwich)
library(lmtest)
coeftest(ryegrass.LL.4, vcov = sandwich)
# Simultaneous inference is also possible through the use of the function glht() in the R package multcomp:
library(multcomp)
summary(glht(ryegrass.LL.4))
# Estimating effective doses ED5, ED10, and ED50 is accomplished using ED():
ED(ryegrass.LL.4, c(5, 10, 50), interval = "delta")
# where 95% confidence intervals are obtained using the delta method.
plot(ryegrass.LL.4, broken = TRUE, type = "all",
     xlab = "Ferulic acid (mM)", xlim = c(0, 100),
     ylab = "Root length (cm)")

# Now we show how functionality of drc may provide output for constructing a high-  quality plot with the extension package ggplot2.
# new dose levels as support for the line
newdata <- expand.grid(conc=exp(seq(log(0.5), log(100), length=100)))
# predictions and confidence intervals
pm <- predict(ryegrass.LL.4, newdata=newdata, interval="confidence")
# new data with predictions
newdata$p <- pm[,1]
newdata$pmin <- pm[,2]
newdata$pmax <- pm[,3]
# plot curve
library(ggplot2)
# need to shift conc == 0 a bit up, otherwise there are problems with coord_trans
ryegrass$conc0 <- ryegrass$conc
ryegrass$conc0[ryegrass$conc0 == 0] <- 0.5
# plotting the curve
ggplot(ryegrass, aes(x = conc0, y = rootl)) +
  geom_point() +
  geom_ribbon(data=newdata, aes(x=conc, y=p, ymin=pmin, ymax=pmax), alpha=0.2) +
  geom_line(data=newdata, aes(x=conc, y=p)) +
  coord_trans(x="log") +
  xlab("Ferulic acid (mM)") + ylab("Root length (cm)")
# As dose-response analysis is a type of regression analysis it is natural to show con-fidence bands around the fitted regression curve instead of providing error bars at each dose in the dataset.

# Example 3: Continuous response: two dose-response curves ----
# Here is another example involving a continuous response. Data are from an experiment comparing the potency of the two herbicides glyphosate and bentazone in white mustard (Sinapis alba) (Christensen et al., 2003). Pay special attention to the use of the argument pmodels to incorporate the assumption that the lower and upper limits for the two herbicides are identical, whereas slopes and ED50 parameters are different (in total 6 parameters).
S.alba.LL.4.1 <- drm(DryMatter~Dose, Herbicide, data=S.alba, fct = LL.4(),
                     pmodels=list(~Herbicide-1, ~1, ~1, ~Herbicide-1))
summary(S.alba.LL.4.1)

# To demonstrate that the two dose-response curves are not the same we fitted the simplermodel not distinguishing between the two curves (in total 4 parameters). Note that for the specification of this model in drm() the second argument, which is used to indicate multiple curves, and the argument pmodels were omitted. Based on these two model fits we calculated an approximate F-test:
S.alba.LL.4.2 <- drm(DryMatter~Dose, data=S.alba, fct = LL.4())
anova(S.alba.LL.4.2, S.alba.LL.4.1)

# The p-value is below 0.0001, rejecting the null hypothesis that the two slopes and ED50 parameters are the same for the two herbicides and, consequently, rejecting that the two dose-response curves are identical. The estimated relative potency based on the two ED50s is obtained using the function EDcomp():
  EDcomp(S.alba.LL.4.1, c(10, 50, 50), interval = "delta")
  # Thus, bentazone is approximately half as potent as glyphosate. Comparison of slopes by means of a z-test may be achieved using the function compParm() as follows:
    compParm(S.alba.LL.4.1, "b", "-")
# Note that this is another way to compare slopes and the p-value is still significant but different from the one we found above using the approximate F test.

# Example 4: Generalized nonlinear regression for a binomial response  ----
## Fitting an extended logistic regression model
## where the upper limit is estimated
earthworms.m1<- drm(number/total ~ dose, weights = total, data = earthworms,
fct = LL.3(), type = "binomial")
summary(earthworms.m1)    
# Perhaps even better (more true to the design) it would be fit a log-logistic model where the upper limit is not estimated but instead fixed at 0.5; this is done in the following R lines:
  ## Fitting an extended logistic regression model
  ## where the upper limit is estimated
earthworms.m2 <- drm(number/total ~ dose, weights = total, data = earthworms,
                       fct = LL.3(fixed = c(NA, 0.5, NA)), type = "binomial")
summary(earthworms.m2)    
# By fixing the upper limit there is a very slight gain in precision for the parameter e, which corresponds to ED50. In contrast, but in this case less interesting, the precision of the slope parameter b is reduced as the dose 0 is important for estimating the slope.

# Example 5: Binomial response and four dose-responses curves  ----
# Now we considered comparison of four dose-response curves corresponding to four types of selenium (Jeske et al., 2009). First, we fit a (joint) model assuming different ED50 values for the different types of selenium:    
selenium.LL.2.1 <- drm(dead/total ~ conc, type, weights = total,
                       data = selenium, fct = LL.2(), type = "binomial")
# Next, we fitted a model assuming a common ED50 for all four types of selenium and then we compared the two model fits using a likelihood ratio test (a chi-square test):
selenium.LL.2.2 <- drm(dead/total~conc, type, weights = total,
                         data = selenium, fct = LL.2(), type="binomial",
                         pmodels = list(~factor(type)-1, ~1))
anova(selenium.LL.2.2, selenium.LL.2.1)           
# The four ED50 values are not identical (p < 0.0001), which was also concluded previously (Jeske et al., 2009). To quantify differences we calculated unadjusted as well as adjusted 95% confidence intervals (adjustment for simultaneous inference):
ED(selenium.LL.2.1, c(50), interval = "delta")    

library(multcomp)
selenium.EDres <- ED(selenium.LL.2.1, c(50), interval = "delta",
multcomp = TRUE, display = FALSE)
confint(glht(selenium.EDres[["EDmultcomp"]]))
    

#drc to fit a 4-parameter or 5-parameter dose-response curve with/without contraints" ----
#https://rpubs.com/rainyrainy/105823
rm(list=ls())
library(drc)

data = read.table("data.txt", header=T, sep="\t")
colnames(data) = c("Conc.uM", "log.Conc", "Pctrl")

plot(data$Conc.uM, data$Pctrl, 
     main="With original concentration", xlab="Concentration (uM)", ylab="% control", col="deeppink", pch=16)
plot(data$log.Conc, data$Pctrl, 
     main="With log concentration", xlab="Log concentration", ylab="% control", col="skyblue4", pch=16)

# Fit a 4-parameter model with no constrain  -----
m.4para = drm(Pctrl ~ Conc.uM, data = data, fct = L.4())
summary(m.4para)
plot(m.4para, 
     main="4-parameter; no contraints", 
     xlab="Concentration (uM)", ylab="% control", 
     col="blue", pch=16)

# Fit a 5-parameter model with no constrain
m.5para = drm(Pctrl ~ Conc.uM, data = data, fct = L.5())
summary(m.5para)
plot(m.5para, main="5-parameter; no contraints", 
     xlab="Concentration (uM)", ylab="% control", 
     col="forestgreen", pch=16)

# Fit a 4-parameter model with 1000≤d≤2000 and −17≤e≤−1  ----
m.4para.c1 = drm(Pctrl ~ Conc.uM, data = data, fct = L.4(), 
              lowerl=c(NA, NA, 1000, -17), upperl=c(NA, NA, 2000, -1))
summary(m.4para.c1)
plot(m.4para.c1, main="5-parameter; with contraints", 
     xlab="Concentration (uM)", ylab="% control", 
     col="orangered", pch=16)

# Fit a 5-parameter model with 1000≤d≤2000 and −17≤e≤−1 and 0≤f≤1
m.5para.c2 = drm(Pctrl ~ Conc.uM, data = data, fct = L.5(), 
              lowerl=c(NA, NA, 1000, -17, 0), upperl=c(NA, NA, 2000, -1, 1))
summary(m.5para.c2)
plot(m.5para.c2, main="5-parameter; with contraints", 
     xlab="Concentration (uM)", ylab="% control", 
     col="royalblue", pch=16)


