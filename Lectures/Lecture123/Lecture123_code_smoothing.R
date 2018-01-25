# Important note:  The "postscript" functions below send
# graphical output to a "postscript" file - these postscript files
# are incorporated into the lecture notes.  You might find it easier
# if you run the R code below removing the lines with the postscript commands.
# The occasional "dev.off()" commands end the direction of graphical output
# to the postscript device.

# smoother example
diab = read.csv("diabetes.csv")
# Variables are
# subject: subject ID number
# age: age diagnosed with diabetes
# acidity: a measure of acidity called base deficit
# y: natural log of serum C-peptide concentration
# Original source is Sockett et al. (1987)
# mentioned in Hastie and Tibshirani's book 
# "Generalized Additive Models".

xpred=(0:160)/10

# Plot of age vs C-Peptide concentration 

postscript(file="diab1.ps",hori=T)
plot(diab$age, diab$y, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration",
     main="Diabetes data")

fit1.lm = lm(y ~ age, data=diab)
fit2.lm = lm(y ~ poly(age,3,raw=T), data=diab)

pred1 = predict(fit1.lm, data.frame(age=xpred), se=T)
pred2 = predict(fit2.lm, data.frame(age=xpred), se=T)

postscript(file="diab2a.ps",hori=T)
plot(diab$age, diab$y, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration",
     main="Diabetes data with least-squares linear fit")
lines(xpred, pred1$fit, col="green",lwd=2)
lines(xpred, pred1$fit-1.96*pred1$se.fit, col="blue",lwd=2, lty=2)
lines(xpred, pred1$fit+1.96*pred1$se.fit, col="blue",lwd=2, lty=2)

postscript(file="diab2b.ps",hori=T)
plot(diab$age, diab$y, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration",
     main="Diabetes data with least-squares cubic fit")
lines(xpred, pred2$fit, col="green",lwd=2)
lines(xpred, pred2$fit-1.96*pred2$se.fit, col="blue",lwd=2, lty=2)
lines(xpred, pred2$fit+1.96*pred2$se.fit, col="blue",lwd=2, lty=2)

# logistic regression
diab$ybin = factor(diab$y>4.0,labels=c("no","yes"))

fit1.glm = glm(ybin ~ age, family=binomial,data=diab)
fit2.glm = glm(ybin ~ poly(age,3,raw=T), family=binomial, data=diab)
pred1 = predict(fit1.glm, data.frame(age=xpred), type="link", se=T)
pred2 = predict(fit2.glm, data.frame(age=xpred), type="link", se=T)
 
postscript(file="diab2c.ps",hori=T)
plot(diab$age, as.numeric(diab$ybin)-1, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration > 4.0",
     ylim=c(-0.2, 1.2),
     main="Diabetes data with logistic linear fit")
lines(xpred, plogis(pred1$fit), col="green",lwd=2)
lines(xpred, plogis(pred1$fit-1.96*pred1$se.fit), col="blue",lwd=2, lty=2)
lines(xpred, plogis(pred1$fit+1.96*pred1$se.fit), col="blue",lwd=2, lty=2)

postscript(file="diab2d.ps",hori=T)
plot(diab$age, as.numeric(diab$ybin)-1, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration > 4.0",
     ylim=c(-0.2, 1.2),
     main="Diabetes data with logistic cubic fit")
lines(xpred, plogis(pred2$fit), col="green",lwd=2)
lines(xpred, plogis(pred2$fit-1.96*pred2$se.fit), col="blue",lwd=2, lty=2)
lines(xpred, plogis(pred2$fit+1.96*pred2$se.fit), col="blue",lwd=2, lty=2)

# four examples with lowess fits with different spans
postscript(file="diab4.ps",hori=T)
par(mfrow=c(2,2))
span.vals = c(0.15, 0.25, 0.7, 4.0)
for(i in 1:4){
ss = loess.smooth(diab$age,diab$y,span=span.vals[i])
plot(diab$age, diab$y, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration",
     sub="lowess fit",
     main=paste("Span =", span.vals[i]))
  lines(ss, col="blue",lwd=2)
  }

# Bias vs variance tradeoff with lowess examples above
postscript(file="diab5.ps",hori=T)
par(mfrow=c(1,2))
plot(diab$age, diab$y, pch=19, col="red",
     main="Large variance, low bias smoother",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration")
  lines(loess.smooth(diab$age,diab$y,span=0.15),col="green",lwd=2)
plot(diab$age, diab$y, pch=19, col="red",
     main="Low variance, large bias smoother",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration")
  lines(loess.smooth(diab$age,diab$y,span=4.00),col="green",lwd=2)


# show examples of smooth.spline with spar getting lower (spar=0.1 is
# nearly
# connect the dots, spar=1 is smooth)

postscript(file="diab6.ps",hori=T)
par(mfrow=c(2,2))
spar.vals = c(0, 0.5, 0.9, 2.0)
for(i in 1:4){
ss = smooth.spline(diab$age,diab$y,spar=spar.vals[i])
plot(diab$age, diab$y, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration",
     main=bquote(lambda~"="~.(round(ss$lambda,6))))
  lines(ss, col="darkgreen",lwd=2)
  }
dev.off()

postscript(file="diab7.ps",hori=T)
ss = smooth.spline(diab$age,diab$y,cv=T)
plot(diab$age, diab$y, pch=19, col="red",
     xlab="Age at Diagnosis",
     ylab="Log C-Peptide Concentration",
     main=bquote("Smoothing spline with"~
lambda~"="~.(round(ss$lambda,5))~"
chosen by CV"))
lines(ss,col="darkgreen",lwd=2)

# LRT
print(anova(fit1.lm,fit2.lm,test="F"))
print(anova(fit1.glm,fit2.glm,test="Chi"))

# splines
h = function(x, xi, pow=1) ifelse(x>xi,(x-xi)^pow,0) # spline
xvals = (0:100)/10
postscript(file="linspline.ps",hori=T)
plot(xvals,h(xvals,4),type='l',
  main="Truncated linear basis function with knot at x=4",
  xlab="X",
  ylab=expression((x-4)['+']),
  lwd=2,col="red",ylim=c(-1,7))

postscript(file="cubspline.ps",hori=T)
plot(xvals,h(xvals,4,pow=3),type='l',
  main="Truncated cubic basis function with knot at x=4",
  xlab="X",ylab=expression((x-4)['+']^3),
  lwd=2,col="red")

postscript(file="piecewiselin.ps",hori=T)
xvals=seq(0,10,.1)
yvals=2+xvals+3*h(xvals,2)-4*h(xvals,5)+.5*h(xvals,8)
plot(xvals,yvals,type="l",
  main="Piecewise linear spline with knots at x=2, 5, and 8",
  xlab="X",ylab="Y",
  lwd=2,col="red")

postscript(file="piecewiselin4.ps",hori=T)
par(mfrow=c(2,2), oma=c(0,0,4,0))
x=seq(0.1,9.9,l=100)
y=qnorm(x/10)+rnorm(100,0,.4)
plot(x,y,main="3 knots")
lines(x,fitted(lm(y~x+h(x,2)+h(x,5)+h(x,8))),type="l",lwd=2,col=4)
lines(x,qnorm(x/10))
points(x,y)
plot(x,y,main="6 knots")
lines(x,fitted(lm(y~x+h(x,1)+h(x,2)+h(x,3.5)+h(x,5)+h(x,6.5)+h(x,8))),col=2,lwd=2)
lines(x,qnorm(x/10))
plot(x,y,main="9 knots")
lines(x,fitted(lm(y~x+h(x,1)+h(x,2)+h(x,3)+h(x,4)+h(x,5)+h(x,6)+h(x,7)+h(x,8)+h(x,9))),col=3,lwd=2)
X=matrix(0,nrow=100,ncol=50)
for(i in (1:25)){
X[,i]=h(x,i/26*10)
}
plot(x,y,main="25 knots")
lines(x,fitted(lm(y~x+X)),col=6,lwd=2)
lines(x,qnorm(x/10))
mtext(text="Increasing the number of knots results in a
more polynomial-like fit",side=3,line=0,outer=T,cex=1.7)

postscript(file="cubspline4.ps",hori=T)
par(mfrow=c(2,2), oma=c(0,0,4,0))
x2=x^2
x3=x^3
XX3 = cbind(h(x,2,3),h(x,5,3),h(x,8,3))
XX6 = cbind(h(x,1,3),h(x,2,3),h(x,3.5,3),h(x,5,3),h(x,6.5,3),h(x,8,3))
XX9 = cbind(h(x,1,3), h(x,2,3), h(x,3,3), h(x,4,3), h(x,5,3), h(x,6,3),
  h(x,7,3), h(x,8,3), h(x,9,3)) 
plot(x,y,main="3 knots")
lines(x,fitted(lm(y~x+x2+x3+XX3)),type="l",lwd=2,col=4)
lines(x,qnorm(x/10))
plot(x,y, main="6 knots")
lines(x,fitted(lm(y~x+x2+x3+XX6)),col=2,lwd=2)
lines(x,qnorm(x/10))
plot(x,y,main="9 knots")
lines(x,qnorm(x/10))
lines(x,fitted(lm(y~x+x2+x3+XX9)),col=3,lwd=2)
X=matrix(0,nrow=100,ncol=50)
for(i in (1:25)){
X[,i]=h(x,i/26*10)^3
}
plot(x,y,main="25 knots")
lines(x,qnorm(x/10))
lines(x,fitted(lm(y~x+x2+x3+X)),col=6,lwd=2)
mtext(text="Cubic splines with increasing
numbers of knots", side=3,line=0,outer=T,cex=1.7)

library(MASS)
library(splines)
quarts = quantile(GAGurine$Age,probs=c(0.25,0.5,0.75))
lmbs1=lm(GAG~bs(Age,knots=quarts),data=GAGurine)
lmns1=lm(GAG~ns(Age,knots=quarts),data=GAGurine)
postscript(file="GAGurine1.ps",hori=T)
plot(GAGurine$Age,GAGurine$GAG,
  main="GAG in urine of children",
  ylab="GAG", xlab="Age",pch=19)

postscript(file="GAGurine2.ps",hori=T)
plot(GAGurine$Age,GAGurine$GAG,
  main="GAG in urine of children",
  ylab="GAG", xlab="Age",pch=19,col="grey")
lines(GAGurine$Age,fitted(lmbs1),lwd=3,col="red")
lines(GAGurine$Age,fitted(lmns1),lwd=3,col="blue")
legend(10, 50,
  legend=c("B-spline, knots at quartiles",
  "Natural spline, knots at quartiles"),
  lty=c(1,1),lwd=c(3,3),
  col=c("red","blue"))

# Kyphosis data
kyphosis = read.csv("kyphosis.csv")
print(summary(kyphosis))

library(gam)  # need to install this library first

kyph0.gam = gam(Kyphosis ~ 1,
  family=binomial, data=kyphosis)

kyph1.gam = gam(Kyphosis ~ s(Age) + s(Number) + s(Start),
  family=binomial, data=kyphosis)
print(summary(kyph1.gam))

print(anova(kyph0.gam,kyph1.gam,test='Chi'))

postscript(file="kyph1.ps",hori=T)
# par(pty='s',mfrow=c(2,2))
par(pty='s',mfrow=c(1,3))
plot(kyph1.gam, resid=T, rug=F, pch=19, col="red")

# remove Number, and refit gam

kyph2.gam = gam(Kyphosis ~ s(Age) + s(Start),
  family=binomial, data=kyphosis)
print(summary(kyph2.gam))

postscript(file="kyph2.ps",hori=T)
par(pty='s',mfrow=c(1,2))
plot(kyph2.gam, rug=T, se=T)

# fit corresponding GLM, and test comparison to GAM

kyph2.glm = glm(Kyphosis ~ Age + Start,
  family=binomial, data=kyphosis)
print(anova(kyph2.glm,kyph2.gam,test="Chi"))

# predictions

kyph.new = data.frame(Age = c(84,85,86), Start=c(7,8,9))
print(predict(kyph2.gam,kyph.new,type="response"))

# using GAM to transform predictors

# based on plots for kyph2.gam, seems that including
# quadratic terms of Age and Start make sense to the GLM.

kyphosis$Age.sq = (kyphosis$Age - mean(kyphosis$Age))^2
kyphosis$Start.sq = (kyphosis$Start - mean(kyphosis$Start))^2
kyph3.glm = glm(Kyphosis ~ Age + Start + Age.sq + Start.sq,
  family=binomial, data=kyphosis)

print(summary(kyph3.glm))

print(anova(kyph2.glm,kyph3.glm,test="Chi"))

