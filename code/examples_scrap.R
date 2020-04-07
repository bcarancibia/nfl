library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(nflscrapR)
library(bannerCommenter)
library(teamcolors)
library(ggrepel)
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



qbs %>%
  ggplot(aes(x = success_per_play, y = epa_per_play)) +
  geom_hline(yintercept = mean(qbs$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(qbs$success_per_play), color = "red", linetype = "dashed") +
  geom_point(color = ifelse(qbs$posteam == "SF", "red", "black"), cex=qbs$n_plays/60, alpha=1/4) +
  geom_text_repel(aes(label=name),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "QB success rate and EPA/play",
       subtitle = "2018, min 100 pass attempts, includes all QB's rush and pass plays") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))




first <- 2009 #first season to grab. min available=2009
last <- 2019 # most recent season

datalist = list()
for (yr in first:last) {
  pbp <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
  pbp <- pbp %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards)
  datalist[[yr]] <- pbp # add it to your list
}

#pbp_all <- dplyr::bind_rows(datalist) doesn't work

pbp_all <- do.call(rbind.data.frame, datalist)

pbp_all %>% group_by(home_team) %>%summarize(n=n(), seasons=n_distinct(season), minyr=min(season), maxyr=max(season)) %>% 
  arrange(seasons)


pbp_all <- pbp_all %>% 
  mutate_at(vars(home_team, away_team, posteam, defteam), funs(case_when(
    . %in% "JAX" ~ "JAC",
    . %in% "STL" ~ "LA",
    . %in% "SD" ~ "LAC",
    TRUE ~ .
  ))) 


saveRDS(pbp_all, file="FILENAME.rds")
pbp_all <- readRDS("FILENAME.rds")

pbp_all_rp <- pbp_all %>%
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
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		      (up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name),
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    yards_gained=ifelse(play_type=="no_play",NA,yards_gained),
    play=1
  ) %>%
  filter(pass==1 | rush==1)

pbp_all_rp %>% filter(pass==1 & !is.na(passer_player_name))%>% mutate(
  arod = if_else(posteam=="GB"&passer_player_name=="A.Rodgers",1,0),
  early = if_else(season<=2014,1,0)
) %>%
  group_by(arod,early) %>%
  summarize(mean_epa=mean(epa), ypp=mean(yards_gained, na.rm = TRUE)) %>% arrange(-early)




##################################################################
##       Lee Sharp - https://github.com/leesharpe/nfldata       ##
##################################################################

standings <- read_csv("http://www.habitatring.com/standings.csv")
standings %>% head()

standings %>% 
  filter(playoff == "WonSB") %>% 
  group_by(seed) %>% 
  summarize(count=n())

ggplot(standings,aes(x=scored,y=allowed)) +
  theme_minimal() +
  geom_point(aes(color=playoff)) +
  xlab("Points Scored") +
  ylab("Points Allowed") +
  labs(title="Points Scored vs. Points Allowed")

games <- read_csv("http://www.habitatring.com/games.csv")
games %>% 
  select(season,week,location,away_team,away_score,home_team,home_score,result) %>% 
  head()

home_games <- games %>% filter(location == "Home")
summary(home_games$result)







