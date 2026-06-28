library(readxl)

data <- read_excel("data/sakernas_2024.xlsx", 
                   sheet = "Sheet5")
View(data)
str(data)

library("VGAM")

y1 =data$Y1
y2 = data$Y2
x1 = data$X1
x2 = data$X2
x3 = data$X3
x4 = data$X4
x5 = data$X5
x6 = data$X6
x7 = data$X7
x8 = data$X8
x9 = data$X9
x10 = data$X10


#Uji Signifikansi Keseluruhan Model
library(VGAM)

# Model penuh dengan semua prediktor
reglog <- vglm(cbind(y1, y2) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, 
               binom2.or(zero=NULL), data = data)
summary(reglog)
null_model<- vglm(cbind(y1, y2) ~ 1, binom2.or(zero=NULL), data = data)
G2 <- 2 * (logLik(reglog) - logLik(null_model))  # Statistik uji G^2
df <- length(coef(reglog)) - length(coef(null_model))  # Derajat kebebasan
p_value <- 1 - pchisq(G2, df)  # p-value
chi_critical <- qchisq(0.95, df)  # Nilai kritis chi-square (α=0,05)
G2 > chi_critical  # Apakah H0 ditolak?
if (p_value < 0.05) {
  print("Tolak H0: Semua prediktor secara simultan berpengaruh signifikan!")
} else {
  print("Gagal tolak H0: Prediktor tidak signifikan secara simultan.")
}
G2
df
qchisq(0.95,30)

#X1 atau Klasikasi desa/kota
IV=c(837,354)
III=c(1710,297)
II=c(51,2)
I=c(553,70)
X1=c(0,1)
daf1=data.frame(I,II,III,IV,X1)

fit1=vglm(cbind(I,II,III,IV)~X1,binom2.or(zero=NULL)) 
fitted(fit1)
summary(fit1)
wald.test(b=coef(fit1), Sigma=vcov(fit1), Terms=1) 
wald.test(b=coef(fit1), Sigma=vcov(fit1), Terms=2)
wald.test(b=coef(fit1), Sigma=vcov(fit1), Terms=3) 
wald.test(b=coef(fit1), Sigma=vcov(fit1), Terms=4)
wald.test(b=coef(fit1), Sigma=vcov(fit1), Terms=5) 
wald.test(b=coef(fit1), Sigma=vcov(fit1), Terms=6)


qchisq(0.95,0)


#X2 atau Jenis kelamin
IV=c(392,799)
III=c(759,1248)
II=c(19,34)
I=c(355,268)
X2=c(0,1)

daf2=data.frame(I,II,III,IV,X2)

fit2=vglm(cbind(I,II,III,IV)~X2,binom2.or(zero=NULL)) 
wald.test(b=coef(fit2), Sigma=vcov(fit2), Terms=1) 
wald.test(b=coef(fit2), Sigma=vcov(fit2), Terms=2)
wald.test(b=coef(fit2), Sigma=vcov(fit2), Terms=3) 
wald.test(b=coef(fit2), Sigma=vcov(fit2), Terms=4)
wald.test(b=coef(fit2), Sigma=vcov(fit2), Terms=5) 
wald.test(b=coef(fit2), Sigma=vcov(fit2), Terms=6)
fitted(fit2)
summary(fit2)
qchisq(0.95,0)


#X3 atau Usia
IV=c(1147,44)
III=c(1984,23)
II=c(50,3)
I=c(622,1)
X3=c(0,1)

daf3=data.frame(I,II,III,IV,X3)

fit3=vglm(cbind(I,II,III,IV)~X3,binom2.or(zero=NULL)) 
fitted(fit3)
summary(fit3)
wald.test(b=coef(fit3), Sigma=vcov(fit3), Terms=1) 
wald.test(b=coef(fit3), Sigma=vcov(fit3), Terms=2)
wald.test(b=coef(fit3), Sigma=vcov(fit3), Terms=3) 
wald.test(b=coef(fit3), Sigma=vcov(fit3), Terms=4)
wald.test(b=coef(fit3), Sigma=vcov(fit3), Terms=5) 
wald.test(b=coef(fit3), Sigma=vcov(fit3), Terms=6)
qchisq(0.95,0)

#X4 atau Status perkawinan
IV=c(152,939,45,55)
III=c(606,1279,69,53)
II=c(8,42,1,2)
I=c(137,457,15,14)
X4=c(0,1,2,3)

daf4=data.frame(I,II,III,IV,X4)

fit4=vglm(cbind(I,II,III,IV)~X4,binom2.or(zero=NULL)) 
fitted(fit4)
summary(fit4)
wald.test(b=coef(fit4), Sigma=vcov(fit4), Terms=1) 
wald.test(b=coef(fit4), Sigma=vcov(fit4), Terms=2)
wald.test(b=coef(fit4), Sigma=vcov(fit4), Terms=3) 
wald.test(b=coef(fit4), Sigma=vcov(fit4), Terms=4)
wald.test(b=coef(fit4), Sigma=vcov(fit4), Terms=5) 
wald.test(b=coef(fit4), Sigma=vcov(fit4), Terms=6)
qchisq(0.95,6)


#X5 atau jumlah anggota rt
IV=c(897,294)
III=c(1449,558)
II=c(47,6)
I=c(494,129)
X5=c(0,1)

daf5=data.frame(I,II,III,IV,X5)

fit5=vglm(cbind(I,II,III,IV)~X5,binom2.or(zero=NULL)) 
fitted(fit5)
summary(fit5)
library(aod)
wald.test(b=coef(fit5), Sigma=vcov(fit5), Terms=1) 
wald.test(b=coef(fit5), Sigma=vcov(fit5), Terms=2)
wald.test(b=coef(fit5), Sigma=vcov(fit5), Terms=3) 
wald.test(b=coef(fit5), Sigma=vcov(fit5), Terms=4)
wald.test(b=coef(fit5), Sigma=vcov(fit5), Terms=5) 
wald.test(b=coef(fit5), Sigma=vcov(fit5), Terms=6)
qchisq(0.95,0)


#X6 atau pengalaman mengikuti pelatihan
IV=c(1007,184)
III=c(1343,664)
II=c(25,28)
I=c(242,381)
X6=c(0,1)

daf6=data.frame(I,II,III,IV,X6)

fit6=vglm(cbind(I,II,III,IV)~X6,binom2.or(zero=NULL)) 
fitted(fit6)
summary(fit6)
wald.test(b=coef(fit6), Sigma=vcov(fit6), Terms=1) 
wald.test(b=coef(fit6), Sigma=vcov(fit6), Terms=2)
wald.test(b=coef(fit6), Sigma=vcov(fit6), Terms=3) 
wald.test(b=coef(fit6), Sigma=vcov(fit6), Terms=4)
wald.test(b=coef(fit6), Sigma=vcov(fit6), Terms=5) 
wald.test(b=coef(fit6), Sigma=vcov(fit6), Terms=6)
qchisq(0.95,0)


#X7 atau jml jam kerja seminggu
IV=c(531,660)
III=c(444,1563)
II=c(24,29)
I=c(214,409)
X7=c(0,1)

daf7=data.frame(I,II,III,IV,X7)

fit7=vglm(cbind(I,II,III,IV)~X7,binom2.or(zero=NULL)) 
fitted(fit7)
summary(fit7)
wald.test(b=coef(fit7), Sigma=vcov(fit7), Terms=1) 
wald.test(b=coef(fit7), Sigma=vcov(fit7), Terms=2)
wald.test(b=coef(fit7), Sigma=vcov(fit7), Terms=3) 
wald.test(b=coef(fit7), Sigma=vcov(fit7), Terms=4)
wald.test(b=coef(fit7), Sigma=vcov(fit7), Terms=5) 
wald.test(b=coef(fit7), Sigma=vcov(fit7), Terms=6)
qchisq(0.95,0)


#X8 atau penggunaan internet dlm pekerjaan
IV=c(257,934)
III=c(375,1632)
II=c(11,42)
I=c(70,553)
X8=c(0,1)

daf8=data.frame(I,II,III,IV,X8)

fit8=vglm(cbind(I,II,III,IV)~X8,binom2.or(zero=NULL)) 
fitted(fit8)
summary(fit8)
qchisq(0.95,0)
wald.test(b=coef(fit8), Sigma=vcov(fit8), Terms=1) 
wald.test(b=coef(fit8), Sigma=vcov(fit8), Terms=2)
wald.test(b=coef(fit8), Sigma=vcov(fit8), Terms=3) 
wald.test(b=coef(fit8), Sigma=vcov(fit8), Terms=4)
wald.test(b=coef(fit8), Sigma=vcov(fit8), Terms=5) 
wald.test(b=coef(fit8), Sigma=vcov(fit8), Terms=6)


#X9 atau jenis instansi
IV=c(0,1,25,1068,52,26,19)
III=c(239,58,897,729,56,14,14)
II=c(0,0,5,42,2,2,2)
I=c(313,70,185,51,1,3,0)
X9=c(0,1,2,3,4,5,6)

daf9=data.frame(I,II,III,IV,X9)

fit9=vglm(cbind(I,II,III,IV)~X9,binom2.or(zero=NULL)) 
fitted(fit9)
summary(fit9)
qchisq(0.95,15)
wald.test(b=coef(fit9), Sigma=vcov(fit9), Terms=1) 
wald.test(b=coef(fit9), Sigma=vcov(fit9), Terms=2)
wald.test(b=coef(fit9), Sigma=vcov(fit9), Terms=3) 
wald.test(b=coef(fit9), Sigma=vcov(fit9), Terms=4)
wald.test(b=coef(fit9), Sigma=vcov(fit9), Terms=5) 
wald.test(b=coef(fit9), Sigma=vcov(fit9), Terms=6)


#X10 atau moda transportasi
IV=c(1021,170)
III=c(1835,172)
II=c(46,7)
I=c(597,26)
X10=c(0,1)

daf10=data.frame(I,II,III,IV,X10)

fit10=vglm(cbind(I,II,III,IV)~X10,binom2.or(zero=NULL)) 
fitted(fit10)
summary(fit10)
wald.test(b=coef(fit10), Sigma=vcov(fit10), Terms=1) 
wald.test(b=coef(fit10), Sigma=vcov(fit10), Terms=2)
wald.test(b=coef(fit10), Sigma=vcov(fit10), Terms=3) 
wald.test(b=coef(fit10), Sigma=vcov(fit10), Terms=4)
wald.test(b=coef(fit10), Sigma=vcov(fit10), Terms=5) 
wald.test(b=coef(fit10), Sigma=vcov(fit10), Terms=6)
qchisq(0.95,0)



##Uji simultan
#X4 dan X9 (X4 4 kategori dan X9 7 kategori)
IV=c(0,0,7,131,10,3,1,0,0,17,849,39,19,15,0,0,1,40,0,3,1,0,1,0,48,3,1,2)
III=c(40,15,291,249,7,2,2,188,40,557,429,41,12,12,4,0,30,33,2,0,0,7,3,19,18,6,0,0)
II=c(0,0,1,6,1,0,0,0,0,4,33,1,2,2,0,0,0,1,0,0,0,0,0,0,2,0,0,0)
I=c(40,13,65,18,0,1,0,264,51,108,32,1,1,0,2,3,9,0,0,1,0,7,3,3,1,0,0,0)
X4=c(0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2,3)
X9=c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6)

dafyc=data.frame(I,II,III,IV,X4,X9)
library(VGAM)
fitg=vglm(cbind(I,II,III,IV)~X4+X9,binom2.or(zero=NULL)) 
fitted(fitg)
summary(fitg)
library(aod)
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=1) 
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=2)
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=3) 
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=4)
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=5) 
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=6)
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=7)
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=8) 
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=9)
wald.test(b=coef(fitg), Sigma=vcov(fitg), Terms=10)
qchisq(0.95,75)


# Buat data baru untuk g1(x) saja
# Gabungkan outcome I dan II sebagai 'Y1 = 1' dan III & IV sebagai 'Y1 = 0'
Y1_1 = IV + III
Y1_0 = I + II

# Fit model logistik biner untuk g1(x)
fit_g1 = glm(cbind(Y1_1, Y1_0) ~ X4 + X9, family = binomial, data = dafyc)
summary(fit_g1)

# Cek AIC-nya
AIC(fit_g1)


# Buat data baru untuk g2(x)
# Gabungkan outcome I dan III sebagai 'Y2 = 1' dan II & IV sebagai 'Y2 = 0'
Y2_1 = II + IV
Y2_0 = I + III

# Fit model logistik biner untuk g2(x)
fit_g2 <- glm(cbind(Y2_1, Y2_0) ~ X4 + X9, family = binomial, data = dafyc)

# Cek AIC-nya
AIC(fit_g2)

AIC(reglog1final)
AIC(reglog2final)

BIC(fit_g1)
BIC(fit_g2)
BIC(reglog1final)
BIC(reglog2final)
pR2(fit_g1)
pR2(fit_g2)
pR2(reglog1final)
pR2(reglog2final)

I=c(152,939,100,0,1,25,1068,52,26,19,69,920,32,24,1,145)
II=c(606,1279,122,239,58,897,729,56,14,14,64,1749,22,30,5,137)
III=c(8,42,3,0,0,5,42,2,2,2,7,39,0,0,0,7)
IV=c(137,457,29,313,70,185,51,1,3,0,63,526,8,6,4,16)
X4=c(0,1,2,0,1,2,3,4,5,6,0,1,2,3,4,5)
X9=c(0,0,0,1,1,1,2,2,2,3,3,4,4,5,5,6)
X10=c(0,1,2,3,4,5,6,0,1,2,3,4,5,0,1,2)

daf11=data.frame(I,II,III,IV,X4,X9,X10)

fit=vglm(cbind(I,II,III,IV)~X4+X9+X10,binom2.or(zero=NULL)) 
fitted(fit)
summary(fit)
coef(fit)
exp(coef(fit))
qchisq(0.95,36)




