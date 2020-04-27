library(datasets)
library(tidyverse)
library(harrypotter)
library(paletteer)
library(bannerCommenter)


#################################################################
##                  testing scales and colors                  ##
#################################################################


ggplot(chickwts) +
  geom_bar(aes(x = feed, y = weight, fill = feed), 
           position = "dodge", stat = "summary", fun.y = "mean") +
  labs(
    y = "Average Weight",
    x = "Feed",
    fill = NULL,
    title = "Average Chicken Weight by Feed Type",
    subtitle = "What's the unit of measure? Who knows!",
    caption = "From the chickwts dataset"
  )  +
  scale_fill_manual(values = my_colors_always) +
  theme_bw()


ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = my_colors_green)



ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = my_colors_green)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = my_colors_red5 )

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = my_colors_raven)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = my_colors_luna)




