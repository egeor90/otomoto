suppressWarnings(suppressMessages(require(here)))
suppressWarnings(suppressMessages(require(xgboost)))
suppressWarnings(suppressMessages(require(data.table)))

setwd(here())

source("functions.R")

# data preprocess
dt <- data.table(combine_all())

dt <- dt[,-c(1,2,7,10)]
dt <- dt[which(dt$km >= 5000),]
dt <- dt[which(as.numeric(as.character(dt$year)) >= 2000),]


dt$old <- as.numeric(Sys.Date()-dt$date)
dt <- dt[,-c("date")]
dt$year <- as.numeric(substr(Sys.Date(),1,4)) - as.numeric(as.character(dt$year))


# Deploy ------------------------------------------------------------------
system("clear")

cat("Brand: ");
brand_ <- as.character(readLines("stdin",n=1))

cat("Model: ");
model_ <- as.character(readLines("stdin",n=1))

cat("Year: ");
year_ <- as.numeric(readLines("stdin",n=1))

cat("Km: ");
km_ <- as.numeric(readLines("stdin",n=1))

cat("Color: ");
color_ <- as.character(readLines("stdin",n=1))

cat("City: ");
city_ <- as.character(readLines("stdin",n=1))

cat("Old (yyyy-mm-dd): ");
old_ <- as.Date(readLines("stdin",n=1))


predict_car(data = dt,
            year = year_,
            km = km_,
            color = color_,
            city = city_,
            brand = brand_,
            model = model_,
            ad_date = old_)


# predict_car(data = dt,year = 2011,km = 170000,color = "Siyah",city = "Ankara",brand = "Toyota",
#             model = "Corolla",
#             ad_date = "2020-11-01")

# pred_days <- NA
# 
# for(i in 1:90){
#   pred_days[i] <- predict_car(data = dt, year = 2011, km = 76000, color = "Siyah", city = "Ankara",
#                               brand = "Opel", model = "Corsa", ad_date = as.Date("2020-11-05")-i)
# }
# 
# plot(pred_days, type = "l")
# 
# which.max(pred_days)
# which.min(pred_days)
# mean(pred_days)