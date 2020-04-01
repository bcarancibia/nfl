library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(nflscrapR)
library(bannerCommenter)
library(teamcolors)

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                    REVIEW EXISTING CODE EXAMPLES                    ###
###                                                                     ###
###########################################################################
###########################################################################



##=============================================================
##  nflscrapR - https://github.com/maksimhorowitz/nflscrapR   =
##=============================================================

week_2_games <- scrape_game_ids(2018, weeks = 2)

# Now generate the play-by-play dataset for the game:
kc_vs_pit_pbp <- week_2_games %>%
  filter(home_team == "PIT") %>%
  pull(game_id) %>%
  scrape_json_play_by_play()


# Pull out the Steelers and Chief colors:
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
pit_color <- nfl_teamcolors %>%
  filter(name == "Pittsburgh Steelers") %>%
  pull(primary)
kc_color <- nfl_teamcolors %>%
  filter(name == "Kansas City Chiefs") %>%
  pull(primary)

kc_vs_pit_pbp %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("KC", "PIT"),
                     values = c(kc_color, pit_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = "KC", color = kc_color, size = 8) + 
  annotate("text", x = 3000, y = .25, label = "PIT", color = pit_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed") + 
  geom_vline(xintercept = 1800, linetype = "dashed") + 
  geom_vline(xintercept = 2700, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = "Week 2 Win Probability Chart",
    subtitle = "Kansas City Chiefs vs. Pittsburgh Steelers",
    caption = "Data from nflscrapR"
  ) + theme_bw()

##=========================================================================================
##  Ben Baldwin Code: https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701   =
##=========================================================================================

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

pbp_rp %>%
  filter(posteam == "LA", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

schotty <- pbp_rp %>%
  filter(wp>.20 & wp<.80 & down<=2 & qtr<=2 & half_seconds_remaining>120) %>%
  group_by(posteam) %>%
  summarize(mean_pass=mean(pass), plays=n()) %>%
  arrange(mean_pass)
schotty

ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))


chart_data <- pbp_rp %>%
  filter(pass==1) %>%
  group_by(posteam) %>%
  summarise(
    num_db = n(),
    epa_per_db = sum(epa) / num_db,
    success_rate = sum(epa > 0) / num_db
  )

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
  ggplot(aes(x = success_rate, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "Dropback success rate & EPA/play",
       subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))


pbp_rp %>% filter(play_type=="no_play") %>% 
  select(desc, pass, passer_player_name, rusher_player_name, receiver_player_name) %>% head()

