#install.packages("jsonlite")
#install.packages("rjson")

library(jsonlite)
library(tidyverse)

#bardata <- jsonlite::read_json("~/Downloads/replays20230201_20230214.json")

#bardata <- readRDS("~/Downloads/replays20220101_20221231.RDS")



# First putting together a basic extract of a matches dataset

get_id <- function(x){x$id[1]}
matches <- data.frame("id" = sapply(bardata, get_id))

get_filename <- function(x){x[["fileName"]]}
get_starttime <- function(x){x[["startTime"]]}
get_duration <- function(x){as.numeric(x[["durationMs"]])/1000}
get_fullduration <- function(x){as.numeric(x[["fullDurationMs"]])/1000}

matches$filename <- sapply(bardata, get_filename)
matches$starttime <- sapply(bardata, get_starttime)
matches$duration <- sapply(bardata, get_duration)
matches$fullduration <- sapply(bardata, get_fullduration)

get_engineversion <- function(x){x[["engineVersion"]]}
get_gameversion <- function(x){x[["gameVersion"]]}
matches$engineversion <- sapply(bardata, get_engineversion)
matches$gameversion <- sapply(bardata, get_gameversion)

get_mapname <- function(x){x[["hostSettings"]][["mapname"]]}
matches$mapname <- sapply(bardata, get_mapname)

get_hsnumplayers <- function(x){x[["hostSettings"]][["numplayers"]]}
get_startpostype <- function(x){x[["hostSettings"]][["startpostype"]]}
get_numrestrictions <- function(x){x[["hostSettings"]][["numrestrictions"]]}
get_hsnumallyteams <- function(x){x[["hostSettings"]][["numallyteams"]]}
get_hsnumteams <- function(x){x[["hostSettings"]][["numteams"]]}

matches$numtotalviewers <- as.numeric(sapply(bardata, get_hsnumplayers))
matches$startpostype <- sapply(bardata, get_startpostype)
matches$numrestrictions <- sapply(bardata, get_numrestrictions)
matches$hsnumallyteams <- sapply(bardata, get_hsnumallyteams)
matches$hsnumteams <- sapply(bardata, get_hsnumteams)

get_coop <- function(x){x[["gameSettings"]][["coop"]]}
matches$coop <- sapply(bardata, get_coop)

get_autobalance <- function(x){x[["spadsSettings"]][["autobalance"]]}
get_skillmode <- function(x){x[["spadsSettings"]][["skillmode"]]}
get_rankmode <- function(x){x[["spadsSettings"]][["rankmode"]]}
get_balancemode <- function(x){x[["spadsSettings"]][["balancemode"]]}
get_autoblockbalance <- function(x){x[["spadsSettings"]][["autoblockbalance"]]}
get_teamsize <- function(x){x[["spadsSettings"]][["teamsize"]]}
get_numteams <- function(x){x[["spadsSettings"]][["nbteams"]]}

matches$autobalance <- sapply(bardata, get_autobalance)
matches$skillmode <- sapply(bardata, get_skillmode)
matches$rankmode <- sapply(bardata, get_rankmode)
matches$balancemode <- sapply(bardata, get_balancemode)
matches$autoblockbalance <- sapply(bardata, get_autoblockbalance)
matches$teamsize <- sapply(bardata, get_teamsize)
matches$numteams <- sapply(bardata, get_numteams)

get_gameendednormally <- function(x){x[["gameEndedNormally"]]}
get_hasbots <- function(x){x[["hasBots"]]}
get_preset <- function(x){x[["preset"]]}
get_reported <- function(x){x[["reported"]]}
matches$gameendednormally <- sapply(bardata, get_gameendednormally)
matches$hasbots <- sapply(bardata, get_hasbots)
matches$preset <- sapply(bardata, get_preset)
matches$reported <- sapply(bardata, get_reported)

get_mapid <- function(x){x[["Map"]][["id"]]}
get_mapscriptName <- function(x){x[["Map"]][["scriptName"]]}
get_mapfileName <- function(x){if(is.null(x[["Map"]][["fileName"]])){NA} else {x[["Map"]][["fileName"]]}}
get_mapwidth <- function(x){if(is.null(x[["Map"]][["width"]])){
  NA} else {as.numeric(x[["Map"]][["width"]])}}
get_mapheight <- function(x){if(is.null(x[["Map"]][["height"]])){
  NA} else {as.numeric(x[["Map"]][["height"]])}}
matches$mapid <- sapply(bardata, get_mapid)
matches$mapscriptname <- sapply(bardata, get_mapscriptName)
matches$mapfilename <- sapply(bardata, get_mapfileName)
matches$mapwidth <- sapply(bardata, get_mapwidth)
matches$mapheight <- sapply(bardata, get_mapheight)
                         
get_numofais <- function(x){
  teams <- length(x[["AllyTeams"]])
  aisum <- 0
  i <- 0
  while (i < teams){
    aisum <- aisum + length(x[["AllyTeams"]][[1]][["AIs"]])
    i <- i + 1
  }
  aisum}

matches$numofais <- sapply(bardata, get_numofais)

get_fromteam <- function(x, tnum, prop){
  if (tnum <= length(x[["AllyTeams"]]) && prop %in% names(x[["AllyTeams"]][[tnum]])) {
         x[["AllyTeams"]][[tnum]][[prop]]} else {NA}}

get_fromplayer <- function(x, tnum, pnum, prop){
  if (tnum <= length(x[["AllyTeams"]]) && pnum <= length(x[["AllyTeams"]][[tnum]][["Players"]]) && prop %in% names(x[["AllyTeams"]][[tnum]][["Players"]][[pnum]])){
    result <- x[["AllyTeams"]][[tnum]][["Players"]][[pnum]][[prop]]
    if (is.null(result)) {return(NA)} else {return(result)}} else {
    NA}}

get_calcnumteams <- function(x){length(x[["AllyTeams"]])}
get_calcnumplayers <- function(x){ 
  sum <- 0
  numteams <- get_calcnumteams(x)
  if (numteams < 1) return(NA)
  for (i in 1:numteams){
    sum <- sum + length(get_fromteam(x, i, "Players"))
  }
  sum}

matches$calcnumteams <- sapply(bardata, get_calcnumteams)
matches$calcnumplayers <- sapply(bardata, get_calcnumplayers)

num_player_obs <- sum(matches$calcnumplayers)

matches$year <- substr(matches$starttime,1,4)
matches$month <- substr(matches$starttime,6, 7)
matches$day <- substr(matches$starttime,9, 10)
matches$hour <- substr(matches$starttime,12, 13)
matches$minute <- substr(matches$starttime,15, 16)
matches$second <- substr(matches$starttime,18, 19)

matches$standardised_mapname <- str_replace(matches$mapname, "\\s[^ ]+$", "")
matches$standardised_mapname <- toupper(str_replace(matches$standardised_mapname, "_[^_]+$", ""))
matches$standardised_mapname <- str_replace(matches$standardised_mapname, "_|\\-"," ")
matches$standardised_mapname <- str_replace(matches$standardised_mapname, "_|\\-"," ")

matches$newmapheight <- (matches$mapheight/2) * 1024
matches$newmapwidth <- (matches$mapwidth/2) * 1024

#Starting work on the players dataset

players <- data.frame("id" = rep("", num_player_obs),
                      "gameid" = rep("", num_player_obs),
                      "playerid" = rep("", num_player_obs),
                      "name" = rep("", num_player_obs),
                      "teamid" = rep("", num_player_obs),
                      "teamidentifier" = rep("", num_player_obs), #the real team identifier?
                      "handicap" = rep("", num_player_obs),
                      "winningteam"= rep(as.logical(NA), num_player_obs),
                      "faction" = rep("", num_player_obs),
                      "countrycode" = rep("", num_player_obs),
                      "rgbred" = rep(as.numeric(NA), num_player_obs),
                      "rgbblue" = rep(as.numeric(NA), num_player_obs),
                      "rgbgreen" = rep(as.numeric(NA), num_player_obs),
                      "rank" = rep(as.numeric(NA), num_player_obs),
                      "skilluncertainty" = rep(as.numeric(NA), num_player_obs),
                      "skill" = rep(as.numeric(NA), num_player_obs),
                      "trueskill" = rep(as.numeric(NA), num_player_obs),
                      "startposx" = rep(as.numeric(NA), num_player_obs),
                      "startposy" = rep(as.numeric(NA), num_player_obs),
                      "startposz" = rep(as.numeric(NA), num_player_obs),
                      "clanid" = rep("", num_player_obs),
                      "trueskillmubefore" = rep(as.numeric(NA), num_player_obs),
                      "trueskillmuafter" = rep(as.numeric(NA), num_player_obs),
                      "trueskillsigmabefore" = rep(as.numeric(NA), num_player_obs),
                      "trueskillsigmaafter" = rep(as.numeric(NA), num_player_obs),
                      "createdat" = rep("", num_player_obs),
                      "updatedat" = rep("", num_player_obs),
                      "allyteamid" = rep("", num_player_obs),
                      "userid" = rep("", num_player_obs),
                      "econdestroyedaward" = rep(as.logical(NA), num_player_obs),
                      "econdestroyedawardteamid" = rep(as.numeric(NA), num_player_obs),
                      "fightingunitsdestroyedaward" = rep(as.logical(NA), num_player_obs),
                      "fightingunitsdestroyedawardteamid" = rep(as.numeric(NA), num_player_obs),
                      "resourceefficiencyaward" = rep(as.logical(NA), num_player_obs),
                      "resourceefficiencyawardteamid" = rep(as.numeric(NA), num_player_obs),
                      "cowaward" = rep(as.logical(NA), num_player_obs),
                      "cowawardteamid" = rep(as.numeric(NA), num_player_obs),
                      "mostresourcesproducedaward" = rep(as.logical(NA), num_player_obs),
                      "mostresourcesproducedawardteamid" = rep(as.numeric(NA), num_player_obs),
                      "mostdamagetakenaward" = rep(as.logical(NA), num_player_obs),
                      "mostdamagetakenawardteamid" = rep(as.numeric(NA), num_player_obs),
                      "sleepaward" = rep(as.logical(NA), num_player_obs),
                      "sleepawardteamid" = rep(as.numeric(NA), num_player_obs))

get_econdestroyedaward <- function(x){x[["awards"]][["econDestroyed"]][[1]][["teamId"]]}
get_fightingunitsdestroyedaward <- function(x){x[["awards"]][["fightingUnitsDestroyed"]][[1]][["teamId"]]}
get_resourceefficiencyaward  <- function(x){x[["awards"]][["resourceEfficiency"]][[1]][["teamId"]]}
get_cowaward  <- function(x){x[["awards"]][["cow"]][["teamId"]]}
get_mostresourcesproducedaward <- function(x){x[["awards"]][["mostResourcesProduced"]][["teamId"]]}
get_mostdamagetakenaward <- function(x){x[["awards"]][["mostDamageTaken"]][["teamId"]]}
get_sleepaward <- function(x){x[["awards"]][["sleep"]][["teamId"]]}

ob_number <- 1                     
for (i in 1:length(bardata)){
#for (i in 1:10){
  if (i %% 500 == 0){print(paste0("Up to match ", as.character(i), " of ", length(bardata)," ..."))}
  match <- bardata[[i]]
  teams <- get_calcnumteams(match)
  id <- get_id(match)
  for (j in 1:teams){
    numplayers <- length(get_fromteam(match, j, "Players"))
    if (numplayers > 0){
      for (k in 1:(numplayers)){
        players[ob_number, "id"] <- id
        players[ob_number, "gameid"] <- get_fromplayer(match,j,k,"id")
        players[ob_number, "playerid"] <- get_fromplayer(match,j,k,"playerId")
        players[ob_number, "name"] <- get_fromplayer(match,j,k,"name")
        players[ob_number, "teamid"] <- get_fromplayer(match,j,k,"teamId")
        players[ob_number, "teamidentifier"] <- get_fromteam(match, j, "allyTeamId")
        players[ob_number, "handicap"] <- get_fromplayer(match,j,k,"handicap")
        players[ob_number, "winningteam"] <- get_fromteam(match,j,"winningTeam")
        players[ob_number, "faction"] <- get_fromplayer(match,j,k,"faction")
        players[ob_number, "countrycode"] <- get_fromplayer(match,j,k,"countryCode")
        players[ob_number, "rgbred"] <- get_fromplayer(match,j,k,"rgbColor")[["r"]]
        players[ob_number, "rgbgreen"] <- get_fromplayer(match,j,k,"rgbColor")[["g"]]
        players[ob_number, "rgbblue"] <- get_fromplayer(match,j,k,"rgbColor")[["b"]]
        players[ob_number, "rank"] <- get_fromplayer(match,j,k,"rank")
        players[ob_number, "skilluncertainty"] <- get_fromplayer(match,j,k,"skillUncertainty")
        players[ob_number, "skill"] <- ifelse(is.null(get_fromplayer(match,j,k,"skill")),NA, as.numeric(gsub("\\[|\\]|\\~|\\#|\\(|\\)", "", get_fromplayer(match,j,k,"skill"))))
        players[ob_number, "trueskill"] <- ifelse(is.null(get_fromplayer(match,j,k,"trueSkill")), NA, get_fromplayer(match,j,k,"trueSkill"))
        players[ob_number, "startposx"] <- get_fromplayer(match,j,k,"startPos")[["x"]]
        players[ob_number, "startposy"] <- get_fromplayer(match,j,k,"startPos")[["y"]]
        players[ob_number, "startposz"] <- get_fromplayer(match,j,k,"startPos")[["z"]]
        players[ob_number, "clanid"] <- get_fromplayer(match,j,k,"clanId")
        players[ob_number, "trueskillmubefore"] <- get_fromplayer(match, j, k, "trueSkillMuBefore")
        players[ob_number, "trueskillmuafter"] <- get_fromplayer(match, j, k, "trueSkillMuAfter")
        players[ob_number, "trueskillsigmabefore"] <- get_fromplayer(match, j, k, "trueSkillSigmaBefore")
        players[ob_number, "trueskillsigmaafter"] <- get_fromplayer(match, j, k, "trueSkillSigmaAfter")
        players[ob_number, "createdat"] <- get_fromplayer(match,j,k,"createdAt")
        players[ob_number, "updatedat"] <- get_fromplayer(match,j,k,"updatedAt")
        players[ob_number, "allyteamid"] <- get_fromplayer(match,j,k,"allyTeamId")
        players[ob_number, "userid"] <- get_fromplayer(match,j,k,"userId")
        
        
        
        if (players[ob_number, "teamid"] == get_econdestroyedaward(match)){
          players[ob_number, "econdestroyedaward"] <- TRUE
        } else {players[ob_number, "econdestroyedaward"] <- FALSE}
        players[ob_number, "econdestroyedawardteamid"] <- match[["awards"]][["econDestroyed"]][[1]][["teamId"]]
        
        if (players[ob_number, "teamid"] == get_fightingunitsdestroyedaward(match)){
          players[ob_number, "fightingunitsdestroyedaward"] <- TRUE
        } else {players[ob_number, "fightingunitsdestroyedaward"] <- FALSE}
        players[ob_number, "fightingunitsdestroyedawardteamid"] <- match[["awards"]][["fightingUnitsDestroyed"]][[1]][["teamId"]]
        
        if (players[ob_number, "teamid"] == get_resourceefficiencyaward(match)){
          players[ob_number, "resourceefficiencyaward"] <- TRUE
        } else {players[ob_number, "resourceefficiencyaward"] <- FALSE}
        players[ob_number, "resourceefficiencyawardteamid"] <- match[["awards"]][["resourceEfficiency"]][[1]][["teamId"]]
        
        if (players[ob_number, "teamid"] == get_cowaward(match)){
          players[ob_number, "cowaward"] <- TRUE
        } else {players[ob_number, "cowaward"] <- FALSE}
        players[ob_number, "cowawardteamid"] <- match[["awards"]][["cow"]][["teamId"]]
        
        if (players[ob_number, "teamid"] == get_mostresourcesproducedaward(match)){
          players[ob_number, "mostresourcesproducedaward"] <- TRUE
        } else {players[ob_number, "mostresourcesproducedaward"] <- FALSE}
        players[ob_number, "mostresourcesproducedawardteamid"] <- match[["awards"]][["mostResourcesProduced"]][["teamId"]]
        
        if (players[ob_number, "teamid"] == get_mostdamagetakenaward(match)){
          players[ob_number, "mostdamagetakenaward"] <- TRUE
        } else {players[ob_number, "mostdamagetakenaward"] <- FALSE}
        players[ob_number, "mostdamagetakenawardteamid"] <- match[["awards"]][["mostDamageTaken"]][["teamId"]]
        
        if (players[ob_number, "teamid"] == get_sleepaward(match)){
          players[ob_number, "sleepaward"] <- TRUE
        } else {players[ob_number, "sleepaward"] <- FALSE}
        players[ob_number, "sleepawardteamid"] <- match[["awards"]][["sleep"]][["teamId"]]
        
        
        ob_number <- ob_number + 1
        
        
      }
    }
  }
}
                      
tabular_players <- right_join(matches, players)                      

write.csv(tabular_players, "~/Downloads/tabularplayers20230101_20230131.csv")
write.csv(matches, "~/Downloads/tabularmatches20230101_20230131.csv")
