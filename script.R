####### SETTING UP THE ENVIRONMENT
rm(list = ls())
options(scipen = 999)
library(dplyr)

### READING SOURCE FILE ####

dataSource <- read.csv("~/R Projects/Mushroom-Classification/src/mushrooms.csv",
                 stringsAsFactors = F)

df <- dataSource %>% rename(classification = class)


case.class <- list(
  !! classification == "e" ~ "edible",
  TRUE ~ "poisonous")


case.cap.shape <- list(
  !! cap.shape == "b" ~ "bell",
  !! cap.shape == "c" ~ "conical",
  !! cap.shape == "x" ~ "convex",
  !! cap.shape == "f" ~ "flat",
  !! cap.shape == "k" ~ "knobbed",
  TRUE ~ "sunken")

case.cap.surface <- list(
  !! cap.surface == "f" ~ "fibrous",
  !! cap.surface == "g" ~ "grooves",
  !! cap.surface == "y" ~ "scaly",
  TRUE ~ "fibrous")

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

case.bruises <- list(
  !! bruises == "t" ~ "bruises",
  TRUE ~ "no")

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

case.gill.attachment <- list(
  !! gill.attachment == "a" ~ "attached",
  !! gill.attachment == "d" ~ "descending",
  !! gill.attachment == "f" ~ "free",
  TRUE ~ "notched")

case.gill.spacing <- list(
  !! gill.spacing == "c" ~ "close",
  !! gill.spacing == "w" ~ "crowded",
  TRUE ~ "distant")

case.gill.size <- list(
  !! gill.size == "b" ~ "broad",
  TRUE ~ "narrow")

case.gill.color <- list(
  !! gill.color == "n" ~ "brown",
  !! gill.color == "y" ~ "yellow",
  !! gill.color == "w" ~ "white",
  !! gill.color == "g" ~ "gray",
  !! gill.color == "e" ~ "red",
  !! gill.color == "p" ~ "pink",
  !! gill.color == "b" ~ "buff",
  !! gill.color == "u" ~ "purple",
  !! gill.color == "c" ~ "cinnamon",
  !! gill.color == "k" ~ "black",
  !! gill.color == "h" ~ "chocolate",
  !! gill.color == "o" ~ "orange",
  TRUE ~ "green")

case.stalk.shape <- list(
  !! stalk.shape == "e" ~ "enlarging",
  TRUE ~ "tapering")

case.stalk.root <- list(
  !! stalk.root == "b" ~ "bulbous",
  !! stalk.root == "c" ~ "club",
  !! stalk.root == "u" ~ "cup",
  !! stalk.root == "e" ~ "equal",
  !! stalk.root == "z" ~ "rhizomorphs",
  !! stalk.root == "r" ~ "rooted",
  TRUE ~ "missing")

case.stalk.surface.above.ring <- list(
  !! stalk.surface.above.ring == "f" ~ "fibrous",
  !! stalk.surface.above.ring == "y" ~ "scaly",
  !! stalk.surface.above.ring == "k" ~ "silky",
  TRUE ~ "smooth")

case.stalk.surface.below.ring <- list(
  !! stalk.surface.below.ring == "f" ~ "fibrous",
  !! stalk.surface.below.ring == "y" ~ "scaly",
  !! stalk.surface.below.ring == "k" ~ "silky",
  TRUE ~ "smooth")

case.stalk.color.above.ring <- list(
  !! stalk.color.above.ring == "n" ~ "brown",
  !! stalk.color.above.ring == "y" ~ "yellow",
  !! stalk.color.above.ring == "w" ~ "white",
  !! stalk.color.above.ring == "g" ~ "gray",
  !! stalk.color.above.ring == "e" ~ "red",
  !! stalk.color.above.ring == "p" ~ "pink",
  !! stalk.color.above.ring == "b" ~ "buff",
  !! stalk.color.above.ring == "c" ~ "cinnamon",
  TRUE ~ "orange")

case.stalk.color.below.ring <- list(
  !! stalk.color.below.ring == "n" ~ "brown",
  !! stalk.color.below.ring == "y" ~ "yellow",
  !! stalk.color.below.ring == "w" ~ "white",
  !! stalk.color.below.ring == "g" ~ "gray",
  !! stalk.color.below.ring == "e" ~ "red",
  !! stalk.color.below.ring == "p" ~ "pink",
  !! stalk.color.below.ring == "b" ~ "buff",
  !! stalk.color.below.ring == "c" ~ "cinnamon",
  TRUE ~ "orange")

case.veil.type <- list(
  !! veil.type == "p" ~ "partial",
  TRUE ~ "universal")


case.veil.color <- list(
  !! veil.color == "n" ~ "brown",
  !! veil.color == "o" ~ "orange",
  !! veil.color == "w" ~ "white",
  TRUE ~ "yellow")

case.ring.number <- list(
  !! ring.number == "n" ~ "none",
  !! ring.number == "o" ~ "one",
  TRUE ~ "two")

case.ring.type <- list(
  !! ring.type == "c" ~ "cobwebby",
  !! ring.type == "e" ~ "evanescent",
  !! ring.type == "f" ~ "flaring",
  !! ring.type == "l" ~ "large",
  !! ring.type == "n" ~ "none",
  !! ring.type == "p" ~ "pendant",
  !! ring.type == "s" ~ "sheathing",
  TRUE ~ "zone")

case.spore.print.color <- list(
  !! spore.print.color == "n" ~ "brown",
  !! spore.print.color == "y" ~ "yellow",
  !! spore.print.color == "w" ~ "white",
  !! spore.print.color == "h" ~ "chocolate",
  !! spore.print.color == "r" ~ "green",
  !! spore.print.color == "u" ~ "purple",
  !! spore.print.color == "b" ~ "buff",
  !! spore.print.color == "k" ~ "black",
  TRUE ~ "orange")

case.population <- list(
  !! population == "a" ~ "abundant",
  !! population == "c" ~ "clustered",
  !! population == "n" ~ "numerous",
  !! population == "s" ~ "scattered",
  !! population == "v" ~ "several",
  TRUE ~ "solitary")

case.habitat <- list(
  !! habitat == "g" ~ "grasses",
  !! habitat == "l" ~ "leaves",
  !! habitat == "m" ~ "meadows",
  !! habitat == "p" ~ "paths",
  !! habitat == "u" ~ "urban",
  !! habitat == "w" ~ "waste",
  TRUE ~ "woods")


df <- mutate(df,
             classification = case_when(!!! case.class),
             cap.shape = case_when(!!! case.cap.shape),
             cap.surface = case_when(!!! case.cap.surface),
             cap.color = case_when(!!! case.cap.color),
             bruises = case_when(!!! case.bruises),
             odor = case_when(!!! case.odor),
             gill.attachment = case_when(!!! case.gill.attachment),
             gill.spacing = case_when(!!! case.gill.spacing),
             gill.size = case_when(!!! case.gill.size),
             gill.color = case_when(!!! case.gill.color),
             stalk.shape = case_when(!!! case.stalk.shape),
             stalk.root = case_when(!!! case.stalk.root),
             stalk.surface.above.ring = case_when(!!! case.stalk.surface.above.ring),
             stalk.surface.below.ring = case_when(!!! case.stalk.surface.below.ring),
             stalk.color.above.ring = case_when(!!! case.stalk.color.above.ring),
             stalk.color.below.ring = case_when(!!! case.stalk.color.below.ring),
             veil.type = case_when(!!! case.veil.type),
             veil.color = case_when(!!! case.veil.color),
             ring.number = case_when(!!! case.ring.number),
             ring.type = case_when(!!! case.ring.type),
             spore.print.color = case_when(!!! case.spore.print.color),
             population = case_when(!!! case.population),
             habitat = case_when(!!! case.habitat))



