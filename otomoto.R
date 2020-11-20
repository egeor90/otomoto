start_ <- Sys.time()

pck_ <- "here"

pck <- pck_[!(pck_ %in% installed.packages()[,"Package"])]
if(length(pck)){
  cat(paste0("Installing: ", pck, "\n"))
  install.packages(pck, repos = 'http://cran.us.r-project.org')
}

suppressWarnings(suppressMessages(invisible(lapply(pck_, require, character.only = TRUE))))


setwd(here::here())
source("functions.R")

system("clear")

cat("Enter the brand: ");
brand <- readLines("stdin",n=1);

cat("Enter the model: ");
model <- readLines("stdin",n=1);

# brand <- "Volkswagen"
# model <- "Passat"


brand_ <- gsub("\ ", '-', brand, perl=T)
model_ <- gsub("\ ", '-', model, perl=T)

print("Please wait! This process may take several minutes.")

url <- paste0("https://www.otomoto.pl/osobowe/",tolower(brand_),"/",tolower(model_))
html <- read_html(url)

max_page <- get_max_page(html = html)

attrbs <- get_attrb(html)
price <- get_price(html)
locat <- get_location(html)
titles <- get_titles(html)
links <- get_links(html)
params <- get_parameters(html)



df <- data.frame(title = titles, 
                 link = links, 
                 price,
                 locat,
                 attrbs,
                 params)


df_ <- df

if(max_page <= 1 || length(max_page)==0){
  dt <- dplyr::data_frame(df_)
  dt <- dt[!duplicated(dt),]
}else if(max_page > 1){
  
  # late pages --------------------------------------------------------------
  max_page <- ifelse(max_page >= 20, 20, max_page)

  for(i in 2:max_page){
    url[i] <- paste0("https://www.otomoto.pl/osobowe/",tolower(brand_),"/",tolower(model_),"?search%5Border%5D=created_at%3Adesc&page=",i)
  }
  
  url <- url[-1]
  
  for(i in 1:length(url)){
    html <- read_html(url[i])   
    attrbs <- get_attrb(html)
    price <- get_price(html)
    locat <- get_location(html)
    titles <- get_titles(html)
    links <- get_links(html)
    params <- get_parameters(html)
    
    df <- data.frame(title = titles, 
                     link = links, 
                     price,
                     locat,
                     attrbs,
                     params)
    
    
    if(!exists("df_")){
      df_ <- df
    }else{
      df_ <- merge(df_, df, all = T)
      # df_ <- rbind(df_, df)
    }
  }
  
  
  dt <- dplyr::data_frame(df_)
  dt <- dt[!duplicated(dt),]
}


dt <- as.data.frame(dt)

dt$brand <- ifelse(tolower(brand) == "bmw", "BMW", str_to_title(brand))
dt$model <- str_to_title(model)

write.table(file=paste0("data/",tolower(brand_),"-",tolower(model_),".csv"), dt, sep = ";", quote = FALSE)

end_ <- Sys.time() 
time_ <- round(difftime(end_, start_, units='secs'),2)

system("clear")
cat(paste0("This process took ", time_, " seconds, with ",nrow(dt), " results\n\n"))
cat(paste0("Successful! Please locate the file here:\n", getwd(),"/data/",paste0(tolower(brand_),"-",tolower(model_),".csv\n\n")))
