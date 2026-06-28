library(readxl)
library(generalhoslem)
library(pscl)
library(car)


data <- read_excel("data/sakernas_2024.xlsx", 
    sheet = "Sheet5")
View(data)
str(data)
y1 = as.factor(data$Y1)
x1 = as.factor(data$X1)
x2 = as.factor(data$X2)
x3 = as.factor(data$X3)
x4 = as.factor(data$X4)
x5 = as.factor(data$X5)
x6 = as.factor(data$X6)
x7 = as.factor(data$X7)
x8 = as.factor(data$X8)
x9 = as.factor(data$X9)
x10 = as.factor(data$X10)
str(y1)


datalog1 = data.frame(y1,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
str(datalog1)


reglog1 = glm(y1~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, family = binomial, data = datalog1)
vif(reglog1)

#Uji Signifikansi Keseluruhan Model
pR2(reglog1)
qchisq(0.95,9)

#Uji Parsial Parameter Model
summary(reglog1)

#R square
pR2(reglog1)

#Odds Ratio
beta1 = (coef(reglog1))
beta1
OR_beta1 = exp(beta1)
OR_beta1
cbind(beta1, OR_beta1)

#Skor Wald
library(aod)
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=1) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=2) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=3) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=4) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=5) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=6) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=7) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=8) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=9) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=10)
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=11) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=12) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=13) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=14) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=15) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=16) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=17) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=18) 
wald.test(b=coef(reglog1), Sigma=vcov(reglog1), Terms=19)

#Membentuk Klasifikasi
yp_hat1 = fitted(reglog1final)
datalog1$yp_hat1 = yp_hat1
class1 = table(datalog1$y1, datalog1$yp_hat1>0.5)
class1


##Pengujian Parameter pada Masing-Masing Variabel Prediktor
#X1 klasifikasi kota atau desa
p1 = glm(y1~x1, family=binomial, data=data)
summary(p1)
install.packages("aod")
library(aod)
wald.test(b=coef(p1), Sigma=vcov(p1), Terms=1) 
wald.test(b=coef(p1), Sigma=vcov(p1), Terms=2)

#X2 usia
p2 = glm(y1~x2, family=binomial, data=data)
summary(p2)
wald.test(b=coef(p2), Sigma=vcov(p2), Terms=1) 
wald.test(b=coef(p2), Sigma=vcov(p2), Terms=2)

#X3 status perkawinan
p3 = glm(y1~x3, family=binomial, data=data)
summary(p3)
wald.test(b=coef(p3), Sigma=vcov(p3), Terms=1) 
wald.test(b=coef(p3), Sigma=vcov(p3), Terms=2)

#X4 jumlah anggota rumah tangga
p4 = glm(y1~x4, family=binomial, data=data)
summary(p4)
wald.test(b=coef(p4), Sigma=vcov(p4), Terms=1) 
wald.test(b=coef(p4), Sigma=vcov(p4), Terms=2)
wald.test(b=coef(p4), Sigma=vcov(p4), Terms=3)
wald.test(b=coef(p4), Sigma=vcov(p4), Terms=4)

#X5 jumlah anggota rumah tangga
p5 = glm(y1~x5, family=binomial, data=data)
summary(p5)
wald.test(b=coef(p5), Sigma=vcov(p5), Terms=1) 
wald.test(b=coef(p5), Sigma=vcov(p5), Terms=2)
wald.test(b=coef(p5), Sigma=vcov(p5), Terms=3)

#X6 jumlah anggota rumah tangga
p6 = glm(y1~x6, family=binomial, data=data)
summary(p6)
wald.test(b=coef(p6), Sigma=vcov(p6), Terms=1) 
wald.test(b=coef(p6), Sigma=vcov(p6), Terms=2)

#X7 jumlah anggota rumah tangga
p7 = glm(y1~x7, family=binomial, data=data)
summary(p7)
wald.test(b=coef(p7), Sigma=vcov(p7), Terms=1) 
wald.test(b=coef(p7), Sigma=vcov(p7), Terms=2)

#X8 jumlah anggota rumah tangga
p8 = glm(y1~x8, family=binomial, data=data)
summary(p8)
wald.test(b=coef(p8), Sigma=vcov(p8), Terms=1) 
wald.test(b=coef(p8), Sigma=vcov(p8), Terms=2)

#X9 jumlah anggota rumah tangga
p9 = glm(y1~x9, family=binomial, data=data)
summary(p9)
wald.test(b=coef(p9), Sigma=vcov(p9), Terms=1) 
wald.test(b=coef(p9), Sigma=vcov(p9), Terms=2)
wald.test(b=coef(p9), Sigma=vcov(p9), Terms=3)
wald.test(b=coef(p9), Sigma=vcov(p9), Terms=4)
wald.test(b=coef(p9), Sigma=vcov(p9), Terms=5)
wald.test(b=coef(p9), Sigma=vcov(p9), Terms=6)
wald.test(b=coef(p9), Sigma=vcov(p9), Terms=7)

#X10 jumlah anggota rumah tangga
p10 = glm(y1~x10, family=binomial, data=data)
summary(p10)
wald.test(b=coef(p10), Sigma=vcov(p10), Terms=1) 
wald.test(b=coef(p10), Sigma=vcov(p10), Terms=2)
wald.test(b=coef(p10), Sigma=vcov(p10), Terms=3)
wald.test(b=coef(p10), Sigma=vcov(p10), Terms=4)
wald.test(b=coef(p10), Sigma=vcov(p10), Terms=5)
wald.test(b=coef(p10), Sigma=vcov(p10), Terms=6)

##Seleksi model terbaik
step(reglog1, direction="backward", test="Chisq", data=datalog1) 
reglog1final=glm(y1~x1+x2+x4+x5+x6+x8+x9+x10, family=binomial, data=datalog1)
summary(reglog1final)
exp(reglog1final$coefficients)
exp(confint(reglog1final))

##Uji kesesuaian model
library(logitgof)
logitgof(y1,fitted(reglog1final))

library(lmtest)
lrtest(reglog1final)
lrtest(reglog2)
