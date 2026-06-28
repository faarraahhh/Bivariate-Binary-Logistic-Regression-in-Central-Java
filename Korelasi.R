install.packages("readxl")  
library(readxl)
data <- read_excel("D:/Farah_kuliah/smt 7/proposal skripsi/data percobaan.xlsx", 
    sheet = "Sheet6")
View(data)

library(psych)

# Buat tabel kontingensi
contingency_table <- table(data$Y1, data$Y2)

# Tampilkan tabel kontingensi
print(contingency_table)

# Cek tipe data variabel
str(data$Y1)
str(data$Y2)

chi_result<-chisq.test(contingency_table)
chi_result

phi_result<-sqrt(chi_result$statistic/sum(contingency_table))
phi_result

cor(as.numeric(vr$Y1), as.numeric(vr$Y2), method="spearman")

library(ltm)

hasil <- biserial.cor(as.numeric(vr$Y1), as.numeric(vr$Y2))
print(hasil)

poly <- polychor(as.numeric(vr$Y1), as.numeric(vr$Y2))
poly
