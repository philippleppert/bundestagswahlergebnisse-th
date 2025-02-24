library(shiny)
library(dplyr)
library(ggplot2)
library(dqshiny) # for autocomplete_input
library(fontawesome)

# data
datenbank <- readRDS("database.rds")

# for shiny app
autocomplete_gemeinden <- 
  datenbank %>%
  filter(dataset == "Gemeindeergebnis") %>%
  group_by(Jahr) %>%
  distinct(Name)

autocomplete_ot <- 
  datenbank %>%
  filter(dataset == "Stadt-/Ortsteilergebnisse") %>%
  group_by(Jahr) %>%
  distinct(Name)

# Farbskala
levels(factor(datenbank$Partei))

partei_farben <- c(
  "#A6CEE3",
  # AfD
  "#0099CC",
  # Basis
  "#00FFFF",
  # BGE,
  "#7F00FF",
  # BSW
  "#ffc0cb",
  # Buendnis Deutschland
  "#000000",
  # CDU
  "#990033",
  # Die Partei
  "#3399FF",
  # DM
  "#CCCCCC",
  # Einzelbewerber
  "#CCCCCC",
  # Einzelbewerber
  "#CCCCCC",
  # Einzelbewerber
  "#E6AB02",
  # FDP
  "#FF7F00",
  # FREIE WÄHLER
  "#666666",
  # GRAUE
  "#33A02C",
  # GRÜNE
  "#FFCCFF",
  # Humanisten
  "#B30000",
  # LINKE
  "#FF9933",
  # Menschliche Welt
  "#CC0000",
  # MLPD
  "#993300",
  # NPD
  "#FF9900",
  # ÖDP
  "#FF6600",
  # Piraten
  "#0066CC",
  # REP
  "#FF0000",
  # SPD
  "#FF0033",
  # Team Todenhöfer
  "#00CCCC",
  # Tierschutzpartei
  "#6600CC",
  # VOLT
  "#CCFF00"  # VPartei
)

# set names
names(partei_farben) <- levels(factor(datenbank$Partei))