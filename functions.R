options(warn=-1)
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

pck_ <- c("tidyverse","rvest","stringr","rebus","lubridate","dplyr","here","xgboost", "data.table", "quantmod")

pck <- pck_[!(pck_ %in% installed.packages()[,"Package"])]
if(length(pck)){
  cat(paste0("Installing: ", pck, "\n"))
  install.packages(pck, repos = 'https://cran.rstudio.com/')
}

suppressWarnings(suppressMessages(invisible(lapply(pck_, require, character.only = TRUE))))
setwd(here())

require(rowr)

get_max_page <- function(html){
  max_page <- as.numeric(html %>% html_nodes('.page') %>% html_text() %>% unlist())
  max_page <- max_page[length(max_page)]
  return(max_page)
}

get_attrb <- function(html){
  attrbs <- html %>% html_nodes('.ds-params-block') %>% html_text()
  attrbs <- gsub(" ","",gsub("\n",":",attrbs))
  attrbs <- gsub(":","",gsub("::","",gsub(":::",";",attrbs)))
  attrbs <- data.table(str_split_fixed(attrbs, ";", 4))
  names(attrbs) <- c("year","km","engine","gas")
  attrbs$km <- as.numeric(substr(as.character(attrbs$km),1,nchar(as.character(attrbs$km))-2))
  attrbs$engine <- as.numeric(substr(as.character(attrbs$engine),1,nchar(as.character(attrbs$engine))-3))
  return(attrbs)
}

get_price <- function(html){
  price_ <- (html %>% html_nodes('.ds-price-number') %>% html_text() %>% str_trim() %>% unlist())
  price_ <- gsub('\\n','',price_)
  price_unit <- as.factor(substr(price_,nchar(price_)-2,nchar(price_)))
  price_ <- gsub(' ','',price_)
  price_ <- as.numeric(substr(price_,1,nchar(price_)-3))
  price_ <- data.table(price_, price_unit)
  return(price_)
}


get_location <- function(html){
  city <- (html %>% html_nodes('.ds-location-city') %>% html_text() %>% str_trim() %>% unlist())
  
  region <- (html %>% html_nodes('.ds-location-region') %>% html_text() %>% str_trim() %>% unlist())
  region <- gsub("[()]","",region)
  
  locat <- data.table(city,region)
  return(locat)
}

get_titles <- function(html){
  return(html %>% html_nodes('.offer-title__link') %>% html_text() %>% str_trim() %>% unlist())
}


get_links <- function(html){
  links <- gsub("\" title=.*","",gsub(".*href=\"","",html %>% html_nodes('.ds-photo-container')))
  links <- gsub(".html.*", ".html", links)
  return(links)
}


get_parameters <- function(html){
  for(i in 1:length(get_links(html))){
    links <- read_html(get_links(html)[i])
    params_label <- links %>% html_nodes('.offer-params__label') %>% html_text()
    params_value <- links %>% html_nodes('.offer-params__value') %>% html_text()
    params_value <- gsub(" ","",gsub("\n","",params_value))
    
    params <- data.frame(params_label,params_value, stringsAsFactors = FALSE)
  
  
    if(!exists("params_")){
      params_ <- params
    }else{
      params_ <- merge(params_, params, by="params_label", all = T)
    }
  }
  rownames(params_) <- params_[,1]
  params_ <- params_[,-1]
  params_ <- t(params_)
  rownames(params_) <- 1:nrow(params_)
  return(data.table(params_))
}

combine_all <- function(){
  suppressWarnings(suppressMessages(require(here)))
  setwd(paste0(here(),"/data"))
  
  all_cars <- lapply(list.files(pattern="*.csv"), 
                     function(x) read.csv(x,
                                          fill=TRUE, 
                                          header=TRUE, 
                                          quote="", 
                                          sep=";", 
                                          #stringsAsFactors = FALSE, 
                                          encoding="UTF-8"))
  
  for(i in 1:length(all_cars)){
    names(all_cars)[i] <- tolower(all_cars[[i]][,ncol(all_cars[[i]])][1])
  }
  
  
  dt <- rbindlist(all_cars, fill = TRUE)
  
  dt$Przebieg <- as.numeric(gsub("km","",dt$Przebieg))
  dt$Emisja.CO2 <- as.numeric(gsub("g/km","",as.character(dt$Emisja.CO2)))
  
  dt$`Opłata.początkowa` <- as.character(gsub("PLN","",as.character(dt$`Opłata.początkowa`)))
  dt$`Opłata.początkowa` <- as.numeric(gsub(",",".",dt$`Opłata.początkowa`))
  
  dt$`Miesięczna.rata` <- as.character(gsub("PLN","",as.character(dt$`Miesięczna.rata`)))
  dt$`Miesięczna.rata` <- as.numeric(gsub(",",".",dt$`Miesięczna.rata`))
  
  dt$Okres.gwarancji.producenta <- as.Date(dt$Okres.gwarancji.producenta, format = "%d/%m/%Y")
  
  dt$lub.do..przebieg.km. <- as.numeric(gsub("km","",dt$lub.do..przebieg.km.))
  
  dt$Gwarancja.dealerska..w.cenie. <- as.numeric(gsub("miesięcy","",as.character(dt$Gwarancja.dealerska..w.cenie.)))
  
  dt$`Pojemność.skokowa` <- as.numeric(gsub("cm3","",dt$`Pojemność.skokowa`))
  
  dt$Pierwsza.rejestracja <- as.Date(dt$Pierwsza.rejestracja, format = "%d/%m/%Y")
  
  dt$Numer.rejestracyjny.pojazdu <- as.character(dt$Numer.rejestracyjny.pojazdu) # can be cancelled
  
  dt$Moc <- as.numeric(gsub("KM","",dt$Moc))
  
  dt$link <- as.character(dt$link)
  
  dt$title <- as.character(dt$title)
  
  dt$`Wartość.wykupu` <- as.numeric(gsub(",",".",as.character(gsub("PLN","",as.character(dt$`Wartość.wykupu`)))))
  
  colnames(dt)[which(colnames(dt) == "price_")] <- "price"
  
  dt$price_pln <- ifelse(as.character(dt$price_unit) == "PLN", dt$price, 
         ifelse(as.character(dt$price_unit) == "EUR",dt$price * quantmod::getQuote(paste0("EUR", "PLN", "=X"))[,2],
         ifelse(as.character(dt$price_unit) == "USD",dt$price * quantmod::getQuote(paste0("USD", "PLN", "=X"))[,2],
         ifelse(as.character(dt$price_unit) == "GBP",dt$price * quantmod::getQuote(paste0("GBP", "PLN", "=X"))[,2],NA))))
  
  
  return(dt)
}


data_shape <- function(){
  ege <- data.frame(combine_all())
  
  ege <- data.table::data.table(ege[,which(!colnames(ege) %in% c("title","link","city","Wersja","price_unit","Numer.rejestracyjny.pojazdu"))])
  
  index_ <- sample(nrow(ege),replace = F)
  index_train <- index_[1:round(0.7*length(index_))]
  index_test <- index_[(round(0.7*length(index_))+1):length(index_)]
  
  ege <- ege %>% relocate(price_pln, .before = price)
  ege <- ege %>% select( everything(),-price)
  
  ege <- Filter(function(x)(length(unique(x))>1), ege)
  return(ege)
}


car_train <- function(portion = 0.7, nround = 20000, output_file = "xgb.model"){
  trainset <- as.data.frame(ege[index_train,])
  testset <- as.data.frame(ege[index_test,])
  
  xgb_train <- data.matrix(trainset)
  xgb_test <- data.matrix(testset)
  xgb_model <- xgboost(data = xgb_train[,-1], label = xgb_train[,1], nthread = 2, nrounds = 20000, 
                       early_stopping_rounds=100,
                       # objective = "reg:linear",
                       objective = "reg:squarederror")
  
  pred_xgb_train <- unlist(predict(xgb_model,xgb_train[,-1]))
  (rmse_xgb_train <- sqrt(colMeans((as.data.frame(xgb_train[,1])-pred_xgb_train)^2)))/mean(ege$price)
  plot(pred_xgb_train, type = "p", main = "Train performance", ylab = "Training set")
  lines(xgb_train[,1], type = "p", col = "red")
  
  
  pred_xgb <- unlist(predict(xgb_model,xgb_test[,-1]))
  (rmse_xgb <- sqrt(colMeans((as.data.frame(xgb_test[,1])-pred_xgb)^2)))/mean(ege$price)
  
  plot(pred_xgb, type = "p", col = "red")
  lines(xgb_test[,1], type = "p", main = "Test performance", ylab = "Test set")
  
  names <- dimnames(xgb_train[,-1])[[2]]
  importance_matrix <- xgb.importance(names, model = xgb_model)
  xgb.plot.importance(importance_matrix[1:20,])
  
  xgb.save(xgb_model, fname = output_file)
}


# predict_car <- function(data, year, km, color, city, brand, model, ad_date){
#   xgb_model <- xgb.load(paste0(here(),'/data/xgb.model'))
  
#   city <-  ifelse(city == "istanbul", "İstanbul", city)
#   deploy_values <- t(data.matrix(c(as.numeric(substr(Sys.Date(),1,4)) - as.numeric(year),
#                                    km, 
#                                    which(levels(data$color)==str_to_title(color)), 
#                                    which(levels(data$city)==str_to_title(city)), 
#                                    which(levels(data$brand)==str_to_title(brand)), 
#                                    which(levels(data$model)==str_to_title(model)), 
#                                    as.numeric(Sys.Date()-as.Date(ad_date)))))
  
  
#   #colnames(deploy_values) <- colnames(xgb_test)[-1]
#   return(predict(xgb_model,deploy_values))  
# }
