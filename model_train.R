suppressWarnings(suppressMessages(require(here)))
suppressWarnings(suppressMessages(require(xgboost)))
suppressWarnings(suppressMessages(require(data.table)))
setwd(here())

source("functions.R")

# data preprocess
ege <- combine_all()

car_train(portion = 0.7, nround = 20000)
