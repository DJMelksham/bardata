library(stringr)

library(tidyverse)

#bardata <- jsonlite::read_json("~/Downloads/replay_detailed_data_2022-01-01_2022-08-31.json")

bardata <- readRDS("~/Downloads/replays20220101_20221231.RDS")


tabular_players <- read.csv("~/Downloads/tabularplayers20220101_20221231.csv")
tabular_matches <- read.csv("~/Downloads/tabularmatches20220101_20221231.csv")

tabular_matches$year <- substr(tabular_matches$starttime,1,4)
tabular_matches$month <- substr(tabular_matches$starttime,6, 7)
tabular_matches$day <- substr(tabular_matches$starttime,9, 10)
tabular_matches$hour <- substr(tabular_matches$starttime,12, 13)
tabular_matches$minute <- substr(tabular_matches$starttime,15, 16)
tabular_matches$second <- substr(tabular_matches$starttime,18, 19)

tabular_matches$standardised_mapname <- str_replace(tabular_matches$mapname, "\\s[^ ]+$", "")
tabular_matches$standardised_mapname <- toupper(str_replace(tabular_matches$standardised_mapname, "_[^_]+$", ""))
tabular_matches$standardised_mapname <- str_replace(tabular_matches$standardised_mapname, "_|\\-"," ")
tabular_matches$standardised_mapname <- str_replace(tabular_matches$standardised_mapname, "_|\\-"," ")

tabular_matches$newmapheight <- (tabular_matches$mapheight/2) * 1024
tabular_matches$newmapwidth <- (tabular_matches$mapwidth/2) * 1024


get_calcnumteams <- function(x){length(x[["AllyTeams"]])}
get_calcnumplayers <- function(x){ 
  sum <- 0
  numteams <- get_calcnumteams(x)
  if (numteams < 1) return(NA)
  for (i in 1:numteams){
    sum <- sum + length(get_fromteam(x, i, "Players"))
  }
  sum}


get_econdestroyedaward <- function(x){x[["awards"]][["econDestroyed"]][[1]][["teamId"]]}
get_fightingunitsdestroyedaward <- function(x){}
get_resourceefficiencyaward  <- function(x){}
get_cowaward  <- function(x){}
get_mostresourcesaward <- function(x){}
get_mostdamagetakenaward <- function(x){}
get_sleepaward <- function(x){}

library(jsonlite)
filename <- "replays20230201_20230214"
test <- jsonlite::read_json(paste0("~/Downloads/", filename, ".json"))
testdata <- test[["data"]]
get_id <- function(x){x$id[1]}
testids <- data.frame('id' = sapply(testdata, get_id))
write.csv(testids, paste0("~/Downloads/", filename, ".csv"))
source("~/get_new_bar_data.R")

#data1 <- readRDS(paste0("~/Downloads/", filename, ".csv"))
get_bar_data(filename)