setwd("C:\\Users\\it24104015\\Desktop\\New folder")
branch_data<-read.csv("Exercise.txt",header = TRUE)

str(branch_data)
names(branch_data)


##3rd
boxplot(branch_data$Sales, main="Boxplot of Sales", col="red")

##4th
summary(branch_data$Advertising)

IQR(branch_data$Advertising)

##5th
# Outlier detection function
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  outliers <- x[x < lower | x > upper]
  return(outliers)
}

# Check for outliers in Years
find_outliers(branch_data$Years)
