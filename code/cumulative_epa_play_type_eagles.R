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

#################################################################
##         Cumulative EPA by play type for 2019 Eagles         ##
#################################################################

tic("data generation")
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))
toc()

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp %>% filter(play_type=="no_play") %>% select(pass, rush, desc)  %>% head
pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp_eagles <- pbp_rp %>%
  filter(posteam == "PHI") 

pass_pbp_eagles <- pbp_rp_eagles %>%
  filter(pass == 1)

rush_pbp_eagles <- pbp_rp_eagles %>%
  filter(rush == 1)


pass_pbp_eagles$count <- seq(1:718)
rush_pbp_eagles$count <- seq(1:426)


pass_pbp_eagles_filter <- pass_pbp_eagles %>%
  select(posteam, epa, count)

rush_pbp_eagles_filter <- rush_pbp_eagles %>%
  select(posteam, epa, count)

pass_eagles_cumsum <- within(pass_pbp_eagles_filter, epa_sum <- cumsum(pass_pbp_eagles_filter$epa))
rush_eagles_cumsum <- within(rush_pbp_eagles_filter, epa_sum <- cumsum(rush_pbp_eagles_filter$epa))


eagles_epa_total <- pass_eagles_cumsum %>%
  mutate(Type = "Pass") %>%
  bind_rows(rush_eagles_cumsum %>%
              mutate(Type = "Rush"))

ggthemr("fresh")
ggplot(eagles_epa_total, aes(x = count, y= epa_sum, color = Type)) +
  geom_line(size = 1) +
  geom_segment(x = 96, xend = 96, y = - 25, yend = 50, color = "red", linetype = 2) + 
  geom_segment(x = 142, xend = 142, y = - 25, yend = 50, color = "red", linetype = 2) +
  xlab("Number of Plays") +
  ylab("Total EPA") +
  labs(caption = "Data From @nflscrapR") +
  labs(title = "Philadelphia Eagles: Rush vs Pass EPA for 2019 Season") +
  labs(color = "Play Type") +
  scale_colour_manual(values = c("forest green", "black"))








  
  
  

##################################################################
##                          DO NOT USE                          ##
##################################################################
pbp_rp_eagles$count <- seq(1:1144)

pbp_rp_eagles_filter <- pbp_rp_eagles %>%
  select(posteam, pass, rush, epa, count)

pbp_rp_eagles_cumsum <- within(pbp_rp_eagles_filter, epa_sum <- cumsum(pbp_rp_eagles_filter$epa))


pass_pbp_eagles <- pbp_rp_eagles_cumsum %>%
  filter(pass == 1)

run_pbp_eagles <- pbp_rp_eagles_cumsum %>%
  filter(rush == 1)

ggplot(pbp_rp_eagles_cumsum, aes(x= count, y=epa_sum)) +
  geom_line() 

ggplot(pass_pbp_eagles, aes(x = count, y = epa_sum)) + 
  geom_line()


ggplot(run_pbp_eagles, aes(x = count, y = epa_sum)) + 
  geom_line()

ggplot(pass_eagles_cumsum, aes(x=count, y=epa_sum)) +
  geom_line()

ggplot(rush_eagles_cumsum, aes(x=count, y = epa_sum)) + 
  geom_line()

ggplot() +
  geom_line(data = rush_eagles_cumsum, aes(x = count, y = epa_sum, color = "Rush"), colour = "gray") + 
  geom_line(data = pass_eagles_cumsum, aes(x=count, y=epa_sum, color = "Pass")) +
  








