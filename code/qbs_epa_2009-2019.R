library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(nflscrapR)
library(bannerCommenter)
library(teamcolors)
library(ggrepel)
library(tictoc)
library(ggthemes)
library(ggthemr)
library(gt)
source("~/nfl/code/helpers.R")

##################################################################
##              Look at QBs Over Past 10 Years EPA              ##
##################################################################


##################################################################
##           first look at 2018 and Follow Setup Code           ##
##################################################################
library(tictoc)
tic("data generation")
#pbp_18 <- scrape_season_play_by_play(2018) this takes a long time
toc()

tic("data generation")
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
toc()

pbp %>% select(posteam, defteam, desc, play_type) %>% head
pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp %>% select(posteam, desc, play_type) %>% head
pbp_rp %>% filter(play_type=="no_play") %>% select(desc, rush_attempt, pass_attempt)  %>% head

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp %>% filter(play_type=="no_play") %>% select(pass, rush, desc)  %>% head

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)


#lots of NAs
passer_test <- pbp_rp %>%
  filter(pass == 1) %>%
  group_by(passer_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), plays=n()) %>%
  arrange(desc(plays))

pbp_players <- pbp_rp %>% 
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name)
  )

qbs <- pbp_players %>% 
  mutate(
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    rusher = rusher_player_name,
    receiver = receiver_player_name,
    play = 1
  ) %>%
  group_by(name, posteam) %>%
  summarize (
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = sum(play),
    epa_per_play = sum(epa)/n_plays,
    success_per_play =sum(success)/n_plays
  ) %>%
  filter(n_dropbacks>=100)
#works for 2018


##################################################################
##                   Get seasons 2009 to 2019                   ##
##################################################################

first <- 2009 #first season to grab. min available=2009
last <- 2019 # most recent season

#drop all the fumble information - doesn't matter and causes errors
datalist = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards, -blocked_player_id, -fumble_recovery_2_player_id)
  datalist[[yr]] <- pbp # add it to your list
}

pbp_all <- dplyr::bind_rows(datalist)

#################################################################
##                    clean up player names                    ##
#################################################################


pbp_all <- pbp_all %>%
  filter(!is_na(epa), !is_na(posteam), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0),
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
    play=1
  ) %>%
  filter(pass==1 | rush==1)




qb_min <- 150


pbp_all_test <- pbp_all %>%
  filter(season >= 2006) %>%
  apply_completion_probability() %>%
  fix_fumbles()

qbs <- pbp_all_test %>%
  group_by(name, season, posteam) %>%
  mutate(
    unadjusted_epa = epa,
    epa = ifelse(epa < -4.5, -4.5, epa)) %>%
  summarize (
    n_plays = sum(play),
    n_db = sum(complete_pass) + sum(incomplete_pass),
    cpoe = mean(100*(complete_pass - cp), na.rm = TRUE),
    epa = mean(epa),
    unadjusted_epa = mean(unadjusted_epa),
    success = mean(success),
    index = 0.248070 * epa + 0.006686 * cpoe + 0.080851
  ) %>%
  filter(n_db > 50 & n_plays >= qb_min) %>% ungroup() %>%
  group_by(name) %>%
  mutate(
    lcpoe = lag(cpoe, n = 1, order_by = season),
    lepa = lag(epa, n = 1, order_by = season),
    lindex = lag(index, n = 1, order_by = season)
  )

qbs[qbs == "Alex Smith"] <- "A.Smith" #for some reason alex smith appears once. 
qbs[qbs == "G.Minshew II"] <- "G.Minshew"

#################################################################
##                    Break it up by Season                    ##
#################################################################




  q<- qbs %>% ungroup() %>% 
    filter(season == 2019) %>%
    select(name,posteam, n_plays,index, epa,unadjusted_epa, success,cpoe) %>%
    mutate(
      ranke = rank(-epa),
      ranki = rank(-index),
      rankeu = rank(-unadjusted_epa),
      ranks = rank(-success),
      rankc = rank(-cpoe),    
      i = paste0(round(index, 2)," (",ranki,")"),
      e = paste0(round(epa, 2)," (",ranke,")"),
      eu = paste0(round(unadjusted_epa, 2)," (",rankeu,")"),
      s = paste0(round(success, 2)," (",ranks,")"),
      c = paste0(round(cpoe, 1)," (",rankc,")"))


q %>%
  gt()
    
