library(readxl)
WM = read_excel("~/Dropbox/Matt Igo - Schaffner Lab/Dissertation/Lab Modeling Project/Results/Results.xlsx", 
                sheet = "Secondary Models")
#######Linear
Grow=as.factor(WM$Growth)
Gro=relevel(Grow,'Broth')
Inoc=as.factor(WM$Inoc)
Ino=relevel(Inoc,"Wet")

model1=lm(Rate~Temp*Aw*Gro*Ino, data=WM)
summary(model1)
aov1=aov(model1)
summary(aov1)

step(model1, direction='both', k=2)

model2=lm(formula = Rate ~ Temp + Aw + Gro + Ino + Temp:Aw + Temp:Gro + 
            Temp:Ino + Gro:Ino + Temp:Gro:Ino, data = WM)


summary(model2)
aov2=aov(model2)
summary(aov2)
AIC(model2)
TukeyHSD(aov2)

model3 = lm(Rate ~ Temp + Aw + Temp:Aw ,data=WM)
summary(model3)
aov3=aov(model3)
summary(aov3)
AIC(model3)


library(olsrr)
ols_step_best_subset(model2, IC='AIC')

model3=lm(Rate ~ Aw + Temp:Aw + Temp:Grow:Inoc,data=WM)
summary(model3)
aov3=aov(model3)
summary(aov3)


model4=lm(Rate ~ Temp + Aw + Grow + Inoc + Temp:Aw + Temp:Grow:Inoc,data=WM)
summary(model4)
aov4=aov(model4)
summary(aov4)
AIC(model4)

############WeibullDelta
Gro=as.factor(WM$Growth)
Gro=relevel(Gro, 'Broth')
Ino=as.factor(WM$Inoc)
Ino=relevel(Ino, 'Wet')

model1=lm(Delta~Temp*Aw*Gro*Ino, data=WM)
summary(model1)
aov1=aov(model1)
summary(aov1)

step(model1, direction='both', k=2)

model2=lm(Delta ~ Temp + Aw + Gro + Ino + Temp:Aw + Temp:Gro + 
            Temp:Ino + Aw:Ino + Gro:Ino + Temp:Aw:Ino + Temp:Gro:Ino,data=WM)

summary(model2)
aov2=aov(model2)
summary(aov2)
AIC(model2)

step(model2, direction='both', k=2)


ols_step_best_subset(model2, IC='AIC')


model3 = lm(Delta ~ Temp + Aw + Grow + Inoc + Aw:Inoc + Grow:Inoc+ Temp:Grow:Inoc, data = WM)
summary(model3)
aov3=aov(model3)
summary(aov3)
AIC(model3)

model3 = lm(Delta ~ Temp + Gro + Temp:Gro, data = WM)
summary(model3)
aov3=aov(model3)
summary(aov3)
AIC(model3)



#####Weiibullp

model1=lm(p~Temp + Aw + Gro + Ino + Temp:Aw + Gro:Ino + Temp:Gro + Temp:Ino + Aw:Gro + Aw:Ino + Gro:Ino + Temp:Aw:Gro + Temp:Aw:Ino + Temp:Ino:Gro + Aw:Gro:Ino, data=WM)
summary(model1)
aov1=aov(model1)
summary(aov1)

step(model1, direction='both', k=2)

model2=lm(formula = p ~ Temp + Aw + Gro + Ino + Temp:Gro + Temp:Ino + 
            Aw:Gro + Aw:Ino, data = WM)


summary(model2)
aov2=aov(model2)
summary(aov2)
AIC(model2)

##########Biphasic
###f
Grow=as.factor(WM$Growth)
Inoc=as.factor(WM$Inoc)

model1=lm(f~Temp + Aw + Grow + Inoc + Temp:Aw + Grow:Inoc + Temp:Grow + Temp:Inoc + Aw:Grow + Aw:Inoc + Grow:Inoc + Temp:Aw:Grow + Temp:Aw:Inoc + Temp:Inoc:Grow + Aw:Grow:Inoc, data=WM)
summary(model1)
aov1=aov(model1)
summary(aov1)

step(model1, direction='both', k=2)

model2=lm(f ~ Temp*Aw*Gro*Ino ,data=WM)

summary(model2)
aov2=aov(model2)
summary(aov2)
AIC(model2)

####k1
model1=lm(k1~Temp + Aw + Gro + Ino + Temp:Aw + Temp:Gro + Temp:Ino + Aw:Gro + Aw:Ino + Gro:Ino + Temp:Aw:Gro + Temp:Aw:Ino + Temp:Ino:Gro + Aw:Gro:Ino, data=WM)
summary(model1)
aov1=aov(model1)
summary(aov1)

step(model1, direction='both', k=2)

model2=lm(formula = k1 ~ Temp + Aw + Gro + Ino + Temp:Aw + Temp:Gro + 
            Aw:Gro + Gro:Ino + Temp:Aw:Gro, data = WM)


summary(model2)
aov2=aov(model2)
summary(aov2)
AIC(model2)

ols_step_best_subset(model2, IC='AIC')

model3=lm(formula = k1 ~ Temp + Aw + Grow + Temp:Aw + 
            Temp:Grow + Aw:Grow + Temp:Aw:Grow, data = WM)


summary(model3)
aov3=aov(model3)
summary(aov3)
AIC(model3)

####k2
model1=lm(k2~Temp*Aw*Gro*Ino, data=WM)
summary(model1)
aov1=aov(model1)
summary(aov1)

step(model1, direction='both', k=2)

AIC(model1)

##############Done
model1=lm(k2~Temp + Aw + Gro + Ino + Temp:Aw + Temp:Gro + Temp:Ino + Aw:Gro + Aw:Ino + Gro:Ino + Temp:Aw:Gro + Temp:Aw:Ino + Temp:Ino:Gro + Aw:Gro:Ino, data=WM)
summary(model1)
aov1=aov(model1)
summary(aov1)
step(model1, direction='both', k=2)



model2=lm(formula = k2 ~ Temp + Aw + Gro + Ino + Temp:Aw + Temp:Gro + 
            Temp:Ino + Aw:Gro + Aw:Ino + Gro:Ino + Temp:Aw:Ino + Temp:Gro:Ino + 
            Aw:Gro:Ino, data = WM)

summary(model2)
aov2=aov(model2)
summary(aov2)

step(model2, direction='both', k=2)

library(olsrr)
ols_step_best_subset(model2, IC='AIC')




model3=lm(k2 ~ Aw:Grow + Temp:Aw:Grow ,data=WM)
summary(model3)
aov3=aov(model3)
summary(aov3)

model4=lm(k2 ~ Aw + Temp+ Grow+Aw:Grow + Temp:Aw:Grow ,data=WM)
summary(model4)
aov4=aov(model4)
summary(aov4)

