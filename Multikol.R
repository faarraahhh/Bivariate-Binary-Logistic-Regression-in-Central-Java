# Install necessary packages if not installed
install.packages("readxl")    # For reading Excel files
install.packages("car")       # For VIF calculation
  # Kalau belum terinstall

# Load your dataset
data <- read_excel("data/sakernas_2024.xlsx", 
    sheet = "Sheet5")
View(data)  # Replace with your file name

# Load the libraries
library(readxl)
library(car)

str(data)

df1<-glm(Y1~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,family=binomial,data=data)
vif(df1)

df2<-glm(Y2~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,family=binomial,data=data)
vif(df1)

df2<-glm(X2~X1+X3+X4+X5+X6+X7+X8+X9+X10,family=binomial,data=data)
vif(df2)

df3<-glm(X3~X1+X2+X4+X5+X6+X7+X8+X9+X10,family=binomial,data=data)
vif(df3)

df4 <- lm(X4 ~ X1+X2+X3+X5+X6+X7+X8+X9+X10, data=data)
vif(df4)

df5<-glm(X5~X1+X2+X3+X4+X6+X7+X8+X9+X10,data=data)
vif(df5)

df6 <- glm(X6 ~ X1+X2+X3+X4+X5+X7+X8+X9+X10, data=data)
vif(df6)

df7<-glm(X7~X1+X2+X3+X4+X5+X6+X8+X9+X10,family=binomial,data=data)
vif(df7)

df8 <- glm(X8 ~ X1+X2+X3+X4+X5+X6+X7+X9+X10, family=binomial,data=data)
vif(df8)

df9 <- lm(X9 ~ X1+X2+X3+X4+X5+X6+X7+X8+X10, data=data)
vif(df9)

df10 <- lm(X10 ~ X1+X2+X3+X4+X5+X6+X7+X8+X9, data=data)
vif(df10)

