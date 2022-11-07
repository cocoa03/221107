# Sambongri Chl-a, collecting and preparing data
data<-read.csv("sambongri_00_22.csv", header=TRUE, na.strings = "NA")

## use normalize() for normalize data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data_norm <- as.data.frame(lapply(data, normalize))

## partition the data into a training set(75%) and a testing set(25%)
chla_train <- data_norm[1:600, ]
chla_test <- data_norm[601:799, ]

# training a model on the data
# install.packages("neuralnet")
library(neuralnet)

Chla_ann <- neuralnet(Chla ~ .,
                      data = chla_train, hidden = 5)

plot(Chla_ann)

# evaluating model performance
model_results3 <- compute(Chla_ann, chla_test[-6])
predicted_Chla <- model_results3$net.result
cor(predicted_Chla, chla_test$Chla)
plot(predicted_Chla, chla_test$Chla)
