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
##         Cumulative EPA by play type for NFC EAST            ##
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


##################################################################
##                            EAGLES                            ##
##################################################################

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
p1 <- ggplot(eagles_epa_total, aes(x = count, y= epa_sum, color = Type)) +
  geom_line(size = 1) +
  xlab("Number of Plays") +
  ylab("Total EPA") +
  labs(caption = "Data From @nflscrapR") +
  labs(title = "Philadelphia Eagles: Rush vs Pass EPA for 2019 Season") +
  labs(color = "Play Type") +
  scale_colour_manual(values = c("forest green", "black"))

#################################################################
##                           Cowboys                           ##
#################################################################

pbp_rp_cowboys <- pbp_rp %>%
  filter(posteam == "DAL") 

pass_pbp_cowboys <- pbp_rp_cowboys %>%
  filter(pass == 1)

rush_pbp_cowboys <- pbp_rp_cowboys %>%
  filter(rush == 1)

pass_pbp_cowboys$count <- seq(1:673)
rush_pbp_cowboys$count <- seq(1:430)

pass_pbp_cowboys_filter <- pass_pbp_cowboys %>%
  select(posteam, epa, count)

rush_pbp_cowboys_filter <- rush_pbp_cowboys %>%
  select(posteam, epa, count)

pass_cowboys_cumsum <- within(pass_pbp_cowboys_filter, epa_sum <- cumsum(pass_pbp_cowboys_filter$epa))
rush_cowboys_cumsum <- within(rush_pbp_cowboys_filter, epa_sum <- cumsum(rush_pbp_cowboys_filter$epa))

cowboys_epa_total <- pass_cowboys_cumsum %>%
  mutate(Type = "Pass") %>%
  bind_rows(rush_cowboys_cumsum %>%
              mutate(Type = "Rush"))

ggthemr("fresh")
p2 <- ggplot(cowboys_epa_total, aes(x = count, y= epa_sum, color = Type)) +
  geom_line(size = 1) +
  xlab("Number of Plays") +
  ylab("Total EPA") +
  labs(caption = "Data From @nflscrapR") +
  labs(title = "Dallas Cowboys: Rush vs Pass EPA for 2019 Season") +
  labs(color = "Play Type") +
  scale_colour_manual(values = c("navy", "gray"))

##################################################################
##                            Giants                            ##
##################################################################

pbp_rp_giants <- pbp_rp %>%
  filter(posteam == "NYG") 

pass_pbp_giants <- pbp_rp_giants %>%
  filter(pass == 1)

rush_pbp_giants <- pbp_rp_giants %>%
  filter(rush == 1)

pass_pbp_giants$count <- seq(1:718)
rush_pbp_giants$count <- seq(1:335) 

pass_pbp_giants_filter <- pass_pbp_giants %>%
  select(posteam, epa, count)

rush_pbp_giants_filter <- rush_pbp_giants %>%
  select(posteam, epa, count)

pass_giants_cumsum <- within(pass_pbp_giants_filter, epa_sum <- cumsum(pass_pbp_giants_filter$epa))
rush_giants_cumsum <- within(rush_pbp_giants_filter, epa_sum <- cumsum(rush_pbp_giants_filter$epa))

giants_epa_total <- pass_giants_cumsum %>%
  mutate(Type = "Pass") %>%
  bind_rows(rush_giants_cumsum %>%
              mutate(Type = "Rush"))

ggthemr("fresh")
p3 <- ggplot(giants_epa_total, aes(x = count, y= epa_sum, color = Type)) +
  geom_line(size = 1) +
  xlab("Number of Plays") +
  ylab("Total EPA") +
  labs(caption = "Data From @nflscrapR") +
  labs(title = "New York Giants: Rush vs Pass EPA for 2019 Season") +
  labs(color = "Play Type") +
  scale_colour_manual(values = c("blue", "red"))


##################################################################
##                           Redskins                           ##
##################################################################


pbp_rp_redskins <- pbp_rp %>%
  filter(posteam == "WAS") 

pass_pbp_redskins <- pbp_rp_redskins %>%
  filter(pass == 1)

rush_pbp_redskins <- pbp_rp_redskins %>%
  filter(rush == 1)

pass_pbp_redskins$count <- seq(1:577)
rush_pbp_redskins$count <- seq(1:341) 

pass_pbp_redskins_filter <- pass_pbp_redskins %>%
  select(posteam, epa, count)

rush_pbp_redskins_filter <- rush_pbp_redskins %>%
  select(posteam, epa, count)

pass_redskins_cumsum <- within(pass_pbp_redskins_filter, epa_sum <- cumsum(pass_pbp_redskins_filter$epa))
rush_redskins_cumsum <- within(rush_pbp_redskins_filter, epa_sum <- cumsum(rush_pbp_redskins_filter$epa))

redskins_epa_total <- pass_redskins_cumsum %>%
  mutate(Type = "Pass") %>%
  bind_rows(rush_redskins_cumsum %>%
              mutate(Type = "Rush"))

ggthemr("fresh")
p4 <- ggplot(redskins_epa_total, aes(x = count, y= epa_sum, color = Type)) +
  geom_line(size = 1) +
  xlab("Number of Plays") +
  ylab("Total EPA") +
  labs(caption = "Data From @nflscrapR") +
  labs(title = "Washington Redskins: Rush vs Pass EPA for 2019 Season") +
  labs(color = "Play Type") +
  scale_colour_manual(values = c("maroon", "gold"))



##################################################################
##                     All Plotted Together                     ##
##################################################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1, p2, p3, p4, cols=2)
ggsave('nfc_east_2019.png', dpi=1000)
