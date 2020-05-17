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
library(nflfastR)
source("~/nfl/code/helpers.R")

##################################################################
##                           QB Divas                           ##
##################################################################

# gameId <- 2019111100
# 
# a<-fast_scraper(gameId, source = "gc") %>%
#   clean_pbp()
# 
# 
# getWikiNFLDraft <- function(x, method = 'df') {
#   if (method == 'copy') {
#     paste0('https://en.wikipedia.org/wiki/', x, '_NFL_Draft') %>%
#       read_html %>%
#       html_table(fill = TRUE) %>%
#       .[[5]] %>%
#       select(-1, -Notes, -Conf.) %>%
#       setNames(c('round', 'pick', 'team', 'name', 'position', 'school')) %>%
#       mutate(
#         name = sub(" \\†", "", name),
#         name = sub(" [*]", "", name),
#         name = sub(" Jr.", "", name),
#         name = sub(" III", "", name),
#         pick = sub("[*]", "", pick),
#         round = sub("[*]", "", round)
#       ) %>%
#       filter(!grepl("forfeit", name)) %>%
#       mutate_all(~ str_squish(.)) %>%
#       select(round, pick, name, position, school, team) %>%
#       write.table(
#         .,
#         "clipboard-20000",
#         sep = "\t",
#         row.names = FALSE,
#         col.names = FALSE
#       )
#   } else{
#     paste0('https://en.wikipedia.org/wiki/', x, '_NFL_Draft') %>%
#       read_html %>%
#       html_table(fill = TRUE) %>%
#       .[[5]] %>%
#       select(-1, -Notes, -Conf.) %>%
#       setNames(c('round', 'pick', 'team', 'name', 'position', 'school')) %>%
#       mutate(
#         name = sub(" \\†", "", name),
#         name = sub(" [*]", "", name),
#         name = sub(" Jr.", "", name),
#         name = sub(" III", "", name),
#         pick = sub("[*]", "", pick),
#         round = sub("[*]", "", round)
#       ) %>%
#       filter(!grepl("forfeit", name)) %>%
#       mutate_all(~ str_squish(.)) %>%
#       select(round, pick, name, position, school, team)
#   }
# }
# 
# a <- as.data.frame(getWikiNFLDraft(2017))


##################################################################
##                Found CSV Data from Lee Sharpe                ##
##################################################################

#load data
draft_picks <- read_csv("~/nfldata/data/draft_picks.csv")

rosters <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds')) %>%
  select(team.season, teamPlayers.gsisId, teamPlayers.displayName, team.abbr, teamPlayers.position) %>%
  as_tibble()


pbp <- 
  bind_rows(
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2000.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2001.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2002.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2003.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2004.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2005.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2006.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2007.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2008.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2009.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2010.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2011.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2012.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2013.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2014.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2015.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2016.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2017.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds')),
    readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
  ) %>%
  filter(season_type == 'REG') %>%
  clean_pbp() %>%
  filter(!is.na(posteam) & !is.na(epa) & (rush == 1 | pass == 1))

joined <- pbp %>% 
  filter(!is.na(receiver_id)) %>%
  filter(pass_touchdown == 1) %>%
  select(posteam, season, desc, passer_id, passer, receiver, receiver_id, epa) %>%
  left_join(rosters, by = c('receiver_id' = 'teamPlayers.gsisId', 'season' = 'team.season'))


joined_filter <- joined %>%
  filter(passer %in% c("A.Rodgers", "D.Brees", "T.Brady", "P.Manning", "P.Rivers", "E.Manning", "B.Roethlisberger", "M.Ryan", 'C.Palmer', "B.Favre"))


joined_picks <- joined_filter %>%
  inner_join(draft_picks, by = c('teamPlayers.displayName' = 'full_name', 'teamPlayers.position' = 'position'))


qb_pass_round <- joined_picks %>%
  group_by(passer) %>%
  count(round)


#################################################################
##                          Round One                          ##
#################################################################


round_one <- as.data.frame(qb_pass_round %>%
  filter(round == 1))

round_one %>%
  gt() %>%
  cols_align(align = "center") %>%
  tab_header(
    title = md("QB TD Passes to First Round Picks"),
    subtitle = md("Aaron Rodgers is Sad") 
    )%>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = passer == "A.Rodgers")
  ) %>%
  cols_label(
    passer = md("**Quarterback**"),
    round = md("**Round TD Receiver Drafted**"),
    n = md("**Number of Times Recieved TD**")) %>%
  tab_source_note("Table: @bcarancibia | PBP and Roster Data: @nflfastR | Draft Pick Data: @LeeSharpeNFL") %>%
  gtsave("firstround.png", expand = 10)
      

ggplot(round_one, aes(x= passer, y = n)) +
  geom_col(fill = "#1F5D25") +
  labs(title = "Number of TDs QB has Passed to First Round Picks " , x = "Quarterback", y = "Number of TD Passes to First Round Picks") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggsave("roundone.png", dpi = 1000)

  


#################################################################
##                          Round Two                          ##
#################################################################

round_two <- as.data.frame(qb_pass_round %>%
  filter(round == 2))

round_two %>%
  gt() %>%
  cols_align(align = "center") %>%
  tab_header(
    title = md("QB TD Passes to Second Round Picks"),
    subtitle = md("Aaron Rodgers is Less Sad")
  )%>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = passer == "A.Rodgers")
  ) %>%
  cols_label(
    passer = md("**Quarterback**"),
    round = md("**Round TD Receiver Drafted**"),
    n = md("**Number of Times Recieved TD**")) %>%
  tab_source_note("Table: @bcarancibia | PBP and Roster Data: @nflfastR | Draft PickData: @LeeSharpeNFL" ) %>%
  gtsave("secondround.png", expand = 10)


ggplot(round_two, aes(x= passer, y = n)) +
  geom_col(fill = "#1F5D25") +
  labs(title = "Number of TDs QB has Passed to Second Round Picks " , x = "Quarterback", y = "Number of TD Passes to Second Round Picks") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggsave("roundtwo.png", dpi = 1000)
  





