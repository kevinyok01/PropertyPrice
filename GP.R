data<-read.csv("C:/Users/kevin/Documents/MH3511/Group Project/Housing Data.csv",header=T)
data
str(data)

head(data)
nrow(data)

# Print rows where SalePrice is equal to 0
print(sum(data[data$SalePrice == 0, ]))
median(data$SalePrice)
mean(data$SalePrice)
mean(data$SalePrice, trim=0.1)
# median<mean,right-skewed
par(mfrow = c(1, 1))

hist(data$SalePrice,main = "Histogram of Saleprice", xlab = "Sale Price", ylab = "Frequency")
shapiro.test(data$SalePrice) #not normal

data$log_SalePrice <- log(data$SalePrice)
# Draw a histogram of log(SalePrice)
hist(data$log_SalePrice, main = "Histogram of log(Saleprice)", xlab = "log(Sale Price)", ylab = "Frequency")
boxplot(data$log_SalePrice, main = "Boxplot of log(Saleprice)")
shapiro.test(data$log_SalePrice)

#log twice, not needed
data$log2_SalePrice <- log(data$log_SalePrice)
shapiro.test(data$log2_SalePrice) #p-value increases as compared to logged once, hence logging once is preferred

hist(data$log_SalePrice, main = "Histogram of log(Saleprice)", xlab = "log(Sale Price)", ylab = "Frequency")
xpt <- seq(0,14,by=0.1)
n_den <- dnorm (xpt,mean(data$log_SalePrice), sd(data$log_SalePrice))
ypt <- n_den * length(data$log_SalePrice)*0.2 
lines(xpt,ypt,col="red")

hist(data$LotArea,main = "Histogram of Lot Area", xlab = "Lot Area", ylab = "Frequency")
boxplot(data$LotArea, main = "Boxplot of Lot Area")
data$log_LotArea <- log(data$LotArea)
hist(data$log_LotArea,main = "Histogram of log (LotArea)", xlab = "log (LotArea)", ylab = "Frequency")
boxplot(data$log_LotArea, main = "Boxplot of log (LotArea)")
shapiro.test(data$LotArea)
shapiro.test(data$log_LotArea)


hist(data$GarageArea,main = "Histogram of Garage Area", xlab = "Garage Area", ylab = "Frequency")
boxplot(data$GarageArea, main = "Boxplot of Garage Area")
data$log_GarageArea <- log(data$GarageArea)
sum(data$GarageArea==0)
trunc<-log(subset(data, GarageArea!=0)$GarageArea)
hist(trunc,main = "Histogram of log (GarageArea)", xlab = "log (GarageArea)", ylab = "Frequency")
boxplot(trunc, main = "Boxplot of log (GarageArea)")
shapiro.test(data$GarageArea)
shapiro.test(trunc)

hist(data$BedroomAbvGr,main = "Histogram of No of Bedroom", xlab = "No of Bedroom", ylab = "Frequency")
boxplot(data$BedroomAbvGr, main = "Boxplot of No of Bedroom")

hist(data$Toilets,main = "Histogram of No of Toilet", xlab = "No of Toilet", ylab = "Frequency")
boxplot(data$Toilets, main = "Boxplot of No of Toilet")

hist(data$Age,main = "Histogram of Age", xlab = "Age", ylab = "Frequency")
boxplot(data$Age, main = "Boxplot of Age")

str(data)
datanum<-data[, c("log_SalePrice", "log_LotArea","log_GarageArea", "BedroomAbvGr", "Toilets", "Age")]
names(datanum)<-c("log(SalePrice)", "log(LotArea)","log(GarageArea)", "Bedroom", "Toilets", "Age")

datanum<- datanum[!is.infinite(datanum$`log(GarageArea)`) | datanum$`log(GarageArea)` != -Inf, ]


# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(datanum,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines


datanum2<-data[, c("log_SalePrice", "LotArea","GarageArea", "BedroomAbvGr", "Toilets", "Age")]
names(datanum2)<-c("log(SalePrice)", "LotArea","GarageArea", "Bedroom", "Toilets", "Age")
pairs(datanum2,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines

#multiplelinearreg

set.seed(1) # Set the seed for reproducibility
sample_size <- floor(0.8 * nrow(data))  # 80% train, 20% test
train_indices <- sample(seq_len(nrow(data)), size = sample_size)

train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

model <- lm(log_SalePrice~LotArea+GarageArea+BedroomAbvGr+
              Toilets+factor(OverallQual)+Age+factor(Remodelled)+factor(MSZoning)+factor(Exterior1st),data=train_data)

summary(model)

predicted_values <- predict(model, newdata = test_data)
actual_values <- test_data$SalePrice
exppredicted_values<-exp(predicted_values)
exppredicted_values
mse <- mean((exppredicted_values - actual_values)^2)
sqrt(mse)

median(data$SalePrice)
