#download new bardata

library(httr)
library(msm)

get_bar_data <- function(file, start = 1, stop = 0){
  
  listofids <- read.csv(paste0("~/Downloads/", file, ".csv"))
  numberofids <- nrow(listofids)
  if (file.exists(paste0("~/Downloads/",file, ".RDS"))){
    result <- readRDS(paste0("~/Downloads/",file, ".RDS"))
  } else {result <- vector(mode = "list", length = numberofids)}
  
  if (file.exists(paste0("~/Downloads/",file, "_status.RDS"))){
    status <- readRDS(paste0("~/Downloads/",file, "_status.RDS"))
  } else {status <- data.frame("id" = listofids$id, "status" = rep(as.character(NA), numberofids))}
  
  for (i in start:(ifelse(stop == 0, numberofids, stop))){
    
    url <- paste0("https://api.bar-rts.com/replays/", listofids[i, "id"])
    print(i)
    print(url)
    response <- httr::GET(url)
    print(status_code(response))
    stopifnot(status_code(response) == 200)
    result[[i]]<- content(response)
    status[i,"status"] <- status_code(response)
    sleeptime <- rtnorm(1, 8, 2, 5, 15)
    if (status_code(response) != 200) {
      print("Aborting run!")
      break}
    print(paste0("Sleeping for ", sleeptime, " seconds."))
    Sys.sleep(sleeptime)
  }

  saveRDS(result, paste0("~/Downloads/",file, ".RDS"))
  saveRDS(status, paste0("~/Downloads/",file, "_status.RDS"))
result
}

