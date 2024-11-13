install.packages("dplyr")
library(dplyr)


Player_15 <- read.csv('../fifa2020/players_15.csv', sep=",")
Player_16 <- read.csv('../fifa2020/players_16.csv', sep=",")
Player_17 <- read.csv('../fifa2020/players_17.csv', sep=",")
Player_18 <- read.csv('../fifa2020/players_18.csv', sep=",")
Player_19 <- read.csv('../fifa2020/players_19.csv', sep=",")
Player_20 <- read.csv('../fifa2020/players_20.csv', sep=",")
Teams_and_leagues <- read.csv('../fifa2020/teams_and_leagues.csv', sep=",")



Players  <- rbind( Player_15,Player_16,Player_17,Player_18,Player_19,Player_20)
View(Players)
View(Teams_and_leagues)

Players_NA <- colSums(is.na(Players))
Teams_and_leagues_NA <- colSums(is.na(Teams_and_leagues))

View(Players_NA)
View(Teams_and_leagues_NA)
