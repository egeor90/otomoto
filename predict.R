suppressWarnings(suppressMessages(require(here)))
suppressWarnings(suppressMessages(require(xgboost)))
suppressWarnings(suppressMessages(require(data.table)))

setwd(here())

source("functions.R")

# data preprocess
dt <- data.table(combine_all())

# Deploy ------------------------------------------------------------------
system("clear")

# cat("Brand: ");
# brand_ <- as.character(readLines("stdin",n=1))

# cat("Model: ");
# model_ <- as.character(readLines("stdin",n=1))

# cat("Year: ");
# year_ <- as.numeric(readLines("stdin",n=1))

# cat("Km: ");
# km_ <- as.numeric(readLines("stdin",n=1))

# cat("Color: ");
# color_ <- as.character(readLines("stdin",n=1))

# cat("City: ");
# city_ <- as.character(readLines("stdin",n=1))

# cat("Old (yyyy-mm-dd): ");
# old_ <- as.Date(readLines("stdin",n=1))


# predict_car(data = dt,
#             year = year_,
#             km = km_,
#             color = color_,
#             city = city_,
#             brand = brand_,
#             model = model_,
#             ad_date = old_)

