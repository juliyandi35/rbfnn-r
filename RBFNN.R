# 1. Determining center and variance points using K-means method
library(RSNNS)
library(RBF)
library(readxl)
# Min-max scaling function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
denormalize_data <- function(x,min_data,max_data){
  x*(max_data-min_data)+min_data
}

data.awal <- read_excel("JKSE1 (1).xlsx")
data.awal$Tanggal<-as.Date(data.awal$Tanggal)
length(data.awal$Terakhir)
sd(data.awal$Terakhir)
summary(data.awal)
library(ggplot2)
ggplot(data.awal,aes(Tanggal,Terakhir))+geom_line()+ggtitle("Harga Penutupan Saham JKSE")
data <- data.awal[,-1]
data <- na.omit(data)
n <- nrow(data)
X3 <- data$Terakhir[1:(n-3)]
X2 <- data$Terakhir[2:(n-2)]
X1 <- data$Terakhir[3:(n-1)]
Y <- data$Terakhir[4:n]
X <- cbind(X1,X2,X3)
data <- data.frame(X,Y)

# Normalize the data
data.norm <- as.data.frame(lapply(data, min_max_scale))

# K-means clustering
set.seed(123)
kmeans.data <- kmeans(data.norm, centers = 3)

# Centers and variances
centers <- kmeans.data$centers
variances <- kmeans.data$withinss / kmeans.data$size

# 2. Model identification for determine how many input neuron in the RBFNN network structure. Input determination is done by check the significant lags in ACF plot
acf(data.norm[, 4])

# 3. Data splitting into 80% training data and 20% test data
set.seed(123)
ind <- sample(nrow(data.norm), nrow(data.norm) * 0.8)
train <- data.norm[ind, ]
#train.real <- data[ind,]
#length(train)
test <- data.norm[-ind, ]
#test.real <- data[-ind,]
#length(test)
train.matrix.x <- train[, -4]
test.matrix <- as.matrix(test)
test.matrix.x <- test.matrix[, -4]
data.norm.x <- data.norm[, -4]

# 4. Optimum network determining using how many hidden network and eliminated input that determined. The best model is determined using the Gaussian activation model. The best model can also determined using which model that has smallest MAPE and MSE. In this process, used Least Square Method and Global Ridge Regression
# Least Square Method
model.ls <- rbf(data.norm.x, data.norm[, 4], centers = centers, variances = variances,
                hidden = 2, activationfun = "gaussian", method = "ls")

# Global Ridge Regression
model.ridge <- rbf(data.norm.x, data.norm[, 4], centers = centers, variances = variances,
                   hidden = 2, activationfun = "gaussian", method = "ridge")

# MAPE
y.pred.ls <- predict(model.ls, test.matrix.x)
y.pred.ridge <- predict(model.ridge, test.matrix.x)
MAPE.ls <- mean(abs(as.vector(y.pred.ls) - test.matrix[, 4] / test.matrix[, 4]))*100
MAPE.ridge <- mean(abs(as.vector(y.pred.ridge) - test.matrix[, 4] / test.matrix[, 4]))*100
MAPE.ls
MAPE.ridge
# MSE
MSE.ls <- mean((as.vector(y.pred.ls) - test.matrix[, 4])^2)
MSE.ridge <- mean((as.vector(y.pred.ridge) - test.matrix[, 4])^2)
MSE.ls
MSE.ridge

# 5. Model fit test using ACF and PACF residual plot
residuals.ls <- test[,4] - predict(model.ls, data.frame(test[,-4]))
acf(residuals.ls)
pacf(residuals.ls)

residuals.ridge <- test[,4] - predict(model.ridge, data.frame(test[,-4]))
acf(residuals.ridge)
pacf(residuals.ridge)

# 6. Forecasting with calculation of output function
forecast <- predict(model.ridge, newdata = data.frame(x1 = c(0.5,0.6,0.7,0.5,0.4),x2 = c(0.5,0.7,0.7,0.4,0.4), x3 = c(0.7,0.6,0.4,0.5,0.6)))
forecast_denormalize <- denormalize_data(forecast,min(data),max(data))
forecast_denormalize

# Plotting hasil
# Buat barisan tanggal untuk data hasil forecast
start_date <- as.Date("2023-08-01")
end_date <- as.Date("2023-08-05")
daily_dates <- seq(start_date, end_date, by = "1 day")
forecasted_data <- data.frame(daily_dates,forecast_denormalize)
colnames(forecasted_data)<-c("Tanggal","Terakhir")

# Gabungkan lalu plotting
Whole_data <- rbind(forecasted_data,data.awal)
ggplot(Whole_data,aes(Tanggal,Terakhir, color = ifelse(Tanggal >= "2023-07-30", "Peramalan", "Data awal")))+
  geom_line() +  # Black line for the main data
  scale_color_manual(values = c("black", "blue")) +  # Set color values
  labs(x = "Tanggal", y = "Harga Saham JKSE", title = "Peramalan Harga Saham JKSE")
