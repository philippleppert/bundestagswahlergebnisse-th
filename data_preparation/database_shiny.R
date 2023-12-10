# --- Wahljahre einlesen/aufbereiten
source("daten_2005.R", encoding = "utf-8")
source("daten_2009.R", encoding = "utf-8")
source("daten_2013.R", encoding = "utf-8")
source("daten_2017.R", encoding = "utf-8")
source("daten_2021.R", encoding = "utf-8")

# --- Wahljahre Zusammenführen
datenbank <- bind_rows(daten_2005,
                       daten_2009, 
                       daten_2013, 
                       daten_2017,
                       daten_2021)

# --- Checks
table(datenbank$Jahr)
table(datenbank$Jahr, datenbank$dataset)

# --- Speichern
saveRDS(datenbank, file = "shiny/database.rds")

