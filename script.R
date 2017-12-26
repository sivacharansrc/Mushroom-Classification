
rm(list = ls())
options(scipen = 999)
library(dplyr)

### READING SOURCE FILE ####

data <- read.csv("~/R Projects/Mushroom-Classification/src/mushrooms.csv")

names(data)


df <- select(data,
             classifcation = class,
             cap.color,
             odor,
             habitat)

case.cap.color <- list(
  !! cap.color == "n" ~ "brown",
  !! cap.color == "y" ~ "yellow",
  !! cap.color == "w" ~ "white",
  !! cap.color == "g" ~ "gray",
  !! cap.color == "e" ~ "red",
  !! cap.color == "p" ~ "pink",
  !! cap.color == "b" ~ "buff",
  !! cap.color == "u" ~ "purple",
  !! cap.color == "c" ~ "cinnamon",
  TRUE ~ "green")

case.odor <- list(
  !! odor == "a" ~ "almond",
  !! odor == "l" ~ "anise",
  !! odor == "c" ~ "creosote",
  !! odor == "y" ~ "fishy",
  !! odor == "f" ~ "foul",
  !! odor == "m" ~ "musty",
  !! odor == "n" ~ "none",
  !! odor == "p" ~ "pungent",
  TRUE ~ "spicy")

case.habitat <- list(
  !! habitat == "g" ~ "grasses",
  !! habitat == "l" ~ "leaves",
  !! habitat == "m" ~ "meadows",
  !! habitat == "p" ~ "paths",
  !! habitat == "u" ~ "urban",
  !! habitat == "w" ~ "waste",
  TRUE ~ "woods")

case.class <- list(
   classification == "e" ~ "Edible",
  TRUE ~ "Poisonous")

df <- mutate(df,
 #            class = case_when(!!! case.class),
 #            cap.color = case_when(!!! case.cap.color),
 #            odor = case_when(!!! case.odor),
             habitat1 = case_when(!!! case.habitat))

df <- mutate(df, class = case_when(!!! case.class))
head(df)



unique(df$cap.color)
