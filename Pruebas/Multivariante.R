library(datasets)
datasets()
mtcars
univ<-lm(formula = mpg~cyl+wt+am+carb, data=mtcars)
univ
fit<- univ$fitted.values
resid<- univ$residuals
summary(univ)

plot(fit,resid, col='red')
sd(mtcars$mpg) #variacion de consumo
univ.glm<-glm(formula=mpg~cyl+wt+am+carb, data=mtcars)
univ.glm

summary(univ.glm)

univ.mpg<-aov(mpg~cyl+disp+am+carb, data=mtcars)
summary(univ.mpg)

univ.hp<- aov(hp~cyl+drat+am+gear+carb, data=mtcars)
summary(univ.hp)
univ.hp

univ.wt<-aov(wt~cyl+disp+drat+am+carb, data=mtcars)
summary(univ.wt)

univ.qsec<-aov(qsec~cyl+disp+drat+vs+am+gear, data=mtcars)
summary(univ.qsec)
univ.qsec<-glm(formula = qsec~cyl+disp+drat+vs+am+gear, data=mtcars)

univ.qsec
#qsec = 26.3-0.80*cyl+0.000770*disp-0.362drat+1.05*vs-1.38*am-0.622

car.res<-cbind(univ.mpg$residuals, univ.hp$residuals, univ.wt$residuals, univ.qsec$residuals)
colnames(car.res)<-c("mpg_res", "hp_res", "wt_res", "qsec_res")
car.res<-data.frame(car.res)
print(car.res, digits=2)

pairs(car.res)


library(mvnormtest)
mshapiro.test(t(car.res))
print(prcomp(car.res, sacale=TRUE, digits=3))
biplot(prcomp(car.res, scale=TRUE, cex=c(.6,1.2),col=c(3,4),
              cex.lab=1.5, xlab="Primera componente principal",
              ylab="Segunda Componente Principal"))

              
       