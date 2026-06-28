library(readxl)
library(generalhoslem)
library(pscl)
library(car)


data <- read_excel("data/sakernas_2024.xlsx", 
                   sheet = "Sheet5")
View(data)
str(data)
y2 = as.factor(data$Y2)
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
str(y2)


datalog2 = data.frame(y2,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
str(datalog2)


reglog2 = glm(y2~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, family = binomial, data = datalog2)

#Uji Signifikansi Keseluruhan Model
pR2(reglog2)
qchisq(0.95,1)

#Uji Parsial Parameter Model
summary(reglog2)

#R square
pR2(reglog2)

#Odds Ratio
beta2 = (coef(reglog2))
beta2
OR_beta2 = exp(beta2)
OR_beta2
cbind(beta2, OR_beta2)

#Skor Wald
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=1) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=2) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=3) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=4) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=5) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=6) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=7) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=8) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=9) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=10)
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=11) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=12) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=13) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=14) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=15) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=16) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=17) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=18) 
wald.test(b=coef(reglog2), Sigma=vcov(reglog2), Terms=19) 


#Membentuk Klasifikasi
yp_hat2 = fitted(reglog2final)
datalog2$yp_hat2 = yp_hat2
class2 = table(datalog2$y2, datalog2$yp_hat2>0.5)
class2


##Pengujian Parameter pada Masing-Masing Variabel Prediktor
#X1 klasifikasi kota atau desa
q1 = glm(y2~x1, family=binomial, data=data)
summary(q1)
install.packages("aod")
library(aod)
wald.test(b=coef(q1), Sigma=vcov(q1), Terms=1) 
wald.test(b=coef(q1), Sigma=vcov(q1), Terms=2)

#X2 usia
q2 = glm(y2~x2, family=binomial, data=data)
summary(q2)
wald.test(b=coef(q2), Sigma=vcov(q2), Terms=1) 
wald.test(b=coef(q2), Sigma=vcov(q2), Terms=2)

#X3 status perkawinan
q3 = glm(y2~x3, family=binomial, data=data)
summary(q3)
wald.test(b=coef(q3), Sigma=vcov(q3), Terms=1) 
wald.test(b=coef(q3), Sigma=vcov(q3), Terms=2)

#X4 jumlah anggota rumah tangga
q4 = glm(y2~x4, family=binomial, data=data)
summary(q4)
wald.test(b=coef(q4), Sigma=vcov(q4), Terms=1) 
wald.test(b=coef(q4), Sigma=vcov(q4), Terms=2)
wald.test(b=coef(q4), Sigma=vcov(q4), Terms=3)
wald.test(b=coef(q4), Sigma=vcov(q4), Terms=4)

#X5 jumlah anggota rumah tangga
q5 = glm(y2~x5, family=binomial, data=data)
summary(q5)
wald.test(b=coef(q5), Sigma=vcov(q5), Terms=1) 
wald.test(b=coef(q5), Sigma=vcov(q5), Terms=2)
wald.test(b=coef(q5), Sigma=vcov(q5), Terms=3)

#X6 jumlah anggota rumah tangga
q6 = glm(y2~x6, family=binomial, data=data)
summary(q6)
wald.test(b=coef(q6), Sigma=vcov(q6), Terms=1) 
wald.test(b=coef(q6), Sigma=vcov(q6), Terms=2)

#X7 jumlah anggota rumah tangga
q7 = glm(y2~x7, family=binomial, data=data)
summary(q7)
wald.test(b=coef(q7), Sigma=vcov(q7), Terms=1) 
wald.test(b=coef(q7), Sigma=vcov(q7), Terms=2)

#X8 jumlah anggota rumah tangga
q8 = glm(y2~x8, family=binomial, data=data)
summary(q8)
wald.test(b=coef(q8), Sigma=vcov(q8), Terms=1) 
wald.test(b=coef(q8), Sigma=vcov(q8), Terms=2)

#X9 jumlah anggota rumah tangga
q9 = glm(y2~x9, family=binomial, data=data)
summary(q9)
wald.test(b=coef(q9), Sigma=vcov(q9), Terms=1) 
wald.test(b=coef(q9), Sigma=vcov(q9), Terms=2)
wald.test(b=coef(q9), Sigma=vcov(q9), Terms=3)
wald.test(b=coef(q9), Sigma=vcov(q9), Terms=4)
wald.test(b=coef(q9), Sigma=vcov(q9), Terms=5)
wald.test(b=coef(q9), Sigma=vcov(q9), Terms=6)
wald.test(b=coef(q9), Sigma=vcov(q9), Terms=7)

#X10 jumlah anggota rumah tangga
q10 = glm(y2~x10, family=binomial, data=data)
summary(q10)
wald.test(b=coef(q10), Sigma=vcov(q10), Terms=1) 
wald.test(b=coef(q10), Sigma=vcov(q10), Terms=2)
wald.test(b=coef(q10), Sigma=vcov(q10), Terms=3)
wald.test(b=coef(q10), Sigma=vcov(q10), Terms=4)
wald.test(b=coef(q10), Sigma=vcov(q10), Terms=5)
wald.test(b=coef(q10), Sigma=vcov(q10), Terms=6)

##Seleksi model terbaik
step(reglog2, direction="backward", test="Chisq", data=datalog2) 
reglog2final=glm(y2~x1+x2+x4+x6+x7+x9, family=binomial, data=datalog2)
summary(reglog2final)
exp(reglog2final$coefficients)
exp(confint(reglog2final))

##Uji kesesuaian model
logitgof(data$Y2,fitted(reglog2final))
