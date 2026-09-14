library(readxl)
NTNPI.data <- read_excel("NTNPI SULSEL.xlsx")
colnames(NTNPI.data) <- c("Date","NTNPI")

# Analisis Deskriptif
summary(NTNPI.data)
writexl::write_xlsx(data.frame(Variabel = "NTNPI",
                               Rata.rata = mean(NTNPI.data$NTNPI),
                               Standar.Deviasi = sd(NTNPI.data$NTNPI),
                               Min = min(NTNPI.data$NTNPI),
                               Max = max(NTNPI.data$NTNPI)),"Analisis Deskriptif.xlsx")
library(ggplot2)
ggplot(NTNPI.data, aes(x = Date, y = NTNPI)) +
  geom_line() +
  labs(title = "NTNPI per Month", x = "Date", y = "Value")

## Tahap Analisis ##
NTNPI <- ts(NTNPI.data$NTNPI, start = c(2016,1), frequency = 12)

n <- length(NTNPI)
P <- 0.2*n
insample <- NTNPI[1:(n-P)]
n1 <- length(insample)

outsample <- NTNPI[(n-P+1):n]

# Parameter window Length (L)
# Trial and Erro
MAPE <- function(actual,prediction){
  result <- mean(abs((actual-prediction)/actual)) * 100
  return(result)
}

library(Rssa)
MAPE.values <- rep(0,(n1/2-2))
for (i in 3:(n1/2)) {
  s <- ssa(NTNPI, L = i)
  r <- reconstruct(s, groups = list(Trend1 = c(1), Season1 = c(2,3)), len = 12)
  komponen <- cbind(r$Trend1,r$Season1)
  diag.avr <- rowSums(komponen)
  MAPE.value <- MAPE(NTNPI,diag.avr)
  MAPE.values[i-2] <- MAPE.value
}
MAPE.values
Best.L <- which.min(MAPE.values)+2
MAPE.Table <- data.frame(L = c(3:(n1/2)),
                         MAPE = MAPE.values)
writexl::write_xlsx(MAPE.Table,"Window Length MAPE Values.xlsx")

L <- Best.L  # Trial and Error (L <= N/2)
K <- n1-L+1

# Dekomposisi
# Embedding
Z <- as.matrix(insample)
Z
X <- embed(Z,L)
id <- 1:L
Y <- rbind(id,X)
Y
W <- as.matrix(Y)
sort <- W[,order(-W[1,])]

THankel <- as.matrix(sort[-1,(1:L)])
Hankel <- t(THankel)
writexl::write_xlsx(data.frame(Hankel),"Matriks Lintasan.xlsx")

# Singular Value Decomposition
trajectory <- Hankel%*%t(Hankel)
dim(trajectory)
writexl::write_xlsx(data.frame(trajectory),"Matriks S.xlsx")

# Eigen Value
eigen.value <- eigen(trajectory)$values
eigen.value
eigen.sv <- data.frame(Eigen = eigen.value,
                       Singular.Value = sqrt(eigen.value))
writexl::write_xlsx(eigen.sv,"Eigen and Singular Value.xlsx")

total <- sum(eigen.value)
propv <- eigen.value/total
kumv <- cumsum(propv)
kumv # Kumulatif varian data yang bisa dijelaskan dari eigen vektor

# Penentuan grouping
eigen.vector <- eigen(trajectory)$vectors
eigen.vector
writexl::write_xlsx(data.frame(eigen.vector),"Eigen Vector.xlsx")

# Principal Component
pc <- (t(Hankel)%*%eigen.vector)/sqrt(eigen.value)
writexl::write_xlsx(data.frame(pc),"Principal Component.xlsx")

# Plot singular value
S <- ssa(insample, L = L)
S
plot(S)
plot(S, type = "vectors", plot.method = "matplot", idx = 1:3)
plot(S, type = "paired", idx = 1:2)

# Rekonstruksi
# Grouping
r <- reconstruct(S, groups = list(Trend1 = c(1), Trend2 = c(2)), len = P)
r
plot(wcor(S,groups = list(Trend1 = c(1), Trend2 = c(2)), len = P))

# Diagonal Averaging
komponen <- cbind(r$Trend1,r$Trend2)

diagonal.averaging <- rowSums(komponen)
diagonal.averaging

writexl::write_xlsx(data.frame(Waktu = c(1:length(r$Trend1)),
                               Trend1 = r$Trend1,
                               Trend2 = r$Trend2,
                               Diagonal.Averaging = diagonal.averaging), "Diagonal Averaging.xlsx")

# Forecasting
forecast <- vforecast(S, groups = list(Trend1 = c(1), Trend2 = c(2)), len = P, only.new=FALSE)
forecast

forecast.result <- as.matrix(forecast$Trend1+forecast$Trend2)
forecast.result

# Prediction Accuracy
residual <- outsample-forecast.result
residual

MAE <- mean(abs(residual))
MAE
MSE <- mean(residual^2)
MSE
MAPE.outsample <- mean(abs(residual/outsample))*100
MAPE.outsample

# Peramalan data aktual secara keseluruhan
s.complete <- ssa(NTNPI, L = L)
r.complete <- reconstruct(s.complete, groups = list(Trend1 = c(1), Trend2 = c(2)), len = 12)
komponen.complete <- cbind(r.complete$Trend1,r.complete$Trend2)
diag.avr.complete <- rowSums(komponen.complete)
writexl::write_xlsx(data.frame(Tanggal = NTNPI.data$Date,
                               Prediksi = diag.avr.complete,
                               Aktual = NTNPI,
                               Error.Percentage = (abs(diag.avr.complete-NTNPI)/NTNPI*100)), "Peramalan Data Testing.xlsx")
Total = sum(abs(diag.avr.complete-NTNPI)/NTNPI*100)
Total
Testing.MAPE <- MAPE(NTNPI,diag.avr.complete)
Testing.MAPE

forecast.complete <- vforecast(s.complete, groups = list(Trend1 = c(1), Trend2 = c(2)), len = 12, only.new=FALSE)
forecast.result.complete <- as.matrix(forecast.complete$Trend1+forecast.complete$Trend2)
writexl::write_xlsx(data.frame(Tanggal = seq(as.Date("2024-10-01"), as.Date("2025-09-01"), by = "month"),
                               Ramalan = forecast.result.complete[106:117]),"Peramalan.xlsx")

# Plot
# Data aktual utuh
data.complete <- as.matrix(NTNPI)
data.comp.kosong <- matrix(NA,12)
data.comp.gab <- rbind(data.complete,data.comp.kosong)
dim(data.comp.gab)

# Data prediksi utuh
data.pred.comp <- as.matrix(diag.avr.complete)
data.pred.kosong <- matrix(NA,12)
data.pred.gab <- rbind(data.pred.comp,data.pred.kosong)
dim(data.pred.gab)

# Data forecast dengan data aktual utuh
forecast_gab <- forecast.result.complete
dim(forecast_gab)
ts.plot(cbind(data.comp.gab, data.pred.gab, forecast_gab), main = "Data Actual, Prediction and Forecast", col = c("blue","red","orange"))
legend("topleft",c("Actual","Prediction","Forecast"), col = c("blue","red","orange"),lty = 1)

