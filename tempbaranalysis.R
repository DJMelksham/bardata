#install.packages("data.table")
library(data.table)
library(dplyr)
library(sqldf)

tabular_players <- read.csv("~/Downloads/tabularplayers20220101_20221231.csv")
tabular_matches <- read.csv("~/Downloads/tabularmatches20220101_20221231.csv")

test <- arrange(tabular_players, name, month, day, hour, minute, second)
test2 <- sqldf("select * from test where calcnumteams = 2 AND calcnumplayers =  16")


test3 <- transform(test, counter = ave(winningteam, rleid(name, winningteam), FUN = seq_along))
test3 <- select(test3, id, name, winningteam, counter, year, month, day, hour, minute, second, skill, mapname, preset, calcnumteams, calcnumplayers)

test4 <- transform(test2, counter = ave(winningteam, rleid(name, winningteam), FUN = seq_along))
test4 <- select(test4, id, name, winningteam, counter, year, month, day, hour, minute, second, skill, mapname, preset, calcnumteams, calcnumplayers)
test5 <- sqldf("select * from test3 where counter <= 50 order by counter desc")
