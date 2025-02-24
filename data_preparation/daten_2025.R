library(tidyverse)

key_kreise <- read_delim("data_preparation/mod_data/keys_lk.csv", delim = ";",
                                 col_types = "cc")

data_raw <- read_delim("data_preparation/mod_data/th_bw_2025.csv", delim = ";", col_names = F,
                       na = c("", "-")) %>%
  `colnames<-`(c("Stand",
                 "Satzart",
                 "Wahlkreisnr.",
                 "Kreisnr.",
                 "Gemeindenr.",
                 "Wahlbezirksnr.",
                 "Name",
                 "Gemeinde mit  eigenem  Briefwahlbezirk",
                 "abgebend	nach Gemeinde/Wahlbezirk",
                 "aufnehmend von	Gemeinde/	Wahlbezirk", 
                 "Anzahl Wahlbezirke insgesamt",
                 "Anzahl Wahlbezirke erfasst",
                 "Wahlberechtigte",
                 "Wähler",
                 "Wahlbeteiligung",
                 "Erststimmen ungültige",
                 "Erststimmen gültige",
                 "Zweitstimmen ungültige",
                 "Zweitstimmen gültige",
                 "Erststimme_absolut_AfD",
                 "Erststimme_prozent_AfD",
                 "Zweitstimme_absolut_AfD",
                 "Zweitstimme_prozent_AfD",
                 "Erststimme_absolut_SPD",
                 "Erststimme_prozent_SPD",
                 "Zweitstimme_absolut_SPD",
                 "Zweitstimme_prozent_SPD",
                 "Erststimme_absolut_CDU",
                 "Erststimme_prozent_CDU",
                 "Zweitstimme_absolut_CDU",
                 "Zweitstimme_prozent_CDU",
                 "Erststimme_absolut_LINKE",
                 "Erststimme_prozent_LINKE",
                 "Zweitstimme_absolut_LINKE",
                 "Zweitstimme_prozent_LINKE",
                 "Erststimme_absolut_FDP",
                 "Erststimme_prozent_FDP",
                 "Zweitstimme_absolut_FDP",
                 "Zweitstimme_prozent_FDP",
                 "Erststimme_absolut_GRÜNE",
                 "Erststimme_prozent_GRÜNE",
                 "Zweitstimme_absolut_GRÜNE",
                 "Zweitstimme_prozent_GRÜNE",
                 "Erststimme_absolut_FREIE_WÄHLER",
                 "Erststimme_prozent_FREIE_WÄHLER",
                 "Zweitstimme_absolut_FREIE_WÄHLER",
                 "Zweitstimme_prozent_FREIE_WÄHLER",
                 "Erststimme_absolut_VOLT",
                 "Erststimme_prozent_VOLT",
                 "Zweitstimme_absolut_VOLT",
                 "Zweitstimme_prozent_VOLT",
                 "Erststimme_absolut_MLPD",
                 "Erststimme_prozent_MLPD",
                 "Zweitstimme_absolut_MLPD",
                 "Zweitstimme_prozent_MLPD",
                 "Erststimme_absolut_Buendnis_Deutschland",
                 "Erststimme_prozent_Buendnis_Deutschland",
                 "Zweitstimme_absolut_Buendnis_Deutschland",
                 "Zweitstimme_prozent_Buendnis_Deutschland",
                 "Erststimme_absolut_BSW",
                 "Erststimme_prozent_BSW",
                 "Zweitstimme_absolut_BSW",
                 "Zweitstimme_prozent_BSW",
                 "Erststimme_absolut_Einzelbewerber1",
                 "Erststimme_prozent_Einzelbewerber1",
                 "Erststimme_absolut_Einzelbewerber2",
                 "Erststimme_prozent_Einzelbewerber2"
                 )
               ) %>%
  mutate(Id = str_c(Wahlkreisnr., Kreisnr., Gemeindenr., Wahlbezirksnr.),
         Jahr = 2025) %>%
  select(Jahr, Id, everything())

# Daten: L
l <- 
  data_raw %>% 
  filter(Satzart == "L")

l_analyse <- 
  l %>% # Allgemeine Infos
  mutate(Wahlbeteiligung = str_replace(Wahlbeteiligung, ",","\\." ) %>% as.numeric(),
         Erststimme_Quote = `Erststimmen gültige` / Wähler,
         Zweitstimme_Quote = `Zweitstimmen gültige` / Wähler) %>%
  select(Jahr, Id, Name, Wahlberechtigte,
         Wähler, Wahlbeteiligung, Erststimme_Quote, Zweitstimme_Quote) %>%
  left_join(l %>% # Erststimmen
  select(Name, starts_with("Erststimme_absolut")) %>%
  pivot_longer(cols = -c(Name),
               names_to = "Partei",
               names_pattern = "Erststimme_absolut_(.*)",
               values_to = "Erststimmen_absolut") %>%
  group_by(Name, Partei) %>%
  summarise(Erststimmen_absolut = sum(Erststimmen_absolut, na.rm = T)) %>%
  ungroup(), by = "Name") %>%
  left_join(l %>% # Zweitstimmen
              select(Name, starts_with("Zweitstimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Zweitstimme_absolut_(.*)",
                           values_to = "Zweitstimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Zweitstimmen_absolut = sum(Zweitstimmen_absolut, na.rm = T)), 
            by = c("Name", "Partei")) %>%
  ungroup() %>%
  mutate(dataset = "Landesergebnis")

l_analyse <-
  l_analyse %>%
  left_join( l_analyse %>%
               group_by(Name) %>%
               summarise(Zweitstimmen_Summe = sum(Zweitstimmen_absolut, na.rm = T),
                         Erststimmen_Summe = sum(Erststimmen_absolut, na.rm = T)),
             by = c("Name")
             ) %>%
  mutate(Zweitstimmen_relativ = Zweitstimmen_absolut/Zweitstimmen_Summe,
         Erststimmen_relativ = Erststimmen_absolut/Erststimmen_Summe)


# Daten: K
k <- 
  data_raw %>% filter(Satzart == "K")

k_analyse  <- 
  k %>% # Allgemeine Infos
  mutate(Wahlbeteiligung = str_replace(Wahlbeteiligung, ",","\\." ) %>% as.numeric(),
         Erststimme_Quote = `Erststimmen gültige` / Wähler,
         Zweitstimme_Quote = `Zweitstimmen gültige` / Wähler) %>%
  select(Jahr, Id, Name, Wahlberechtigte,
         Wähler, Wahlbeteiligung, Erststimme_Quote, Zweitstimme_Quote) %>%
  left_join(k %>% # Erststimmen
              select(Name, starts_with("Erststimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Erststimme_absolut_(.*)",
                           values_to = "Erststimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Erststimmen_absolut = sum(Erststimmen_absolut, na.rm = T)) %>%
              ungroup(), by = "Name") %>%
  left_join(k %>% # Zweitstimmen
              select(Name, starts_with("Zweitstimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Zweitstimme_absolut_(.*)",
                           values_to = "Zweitstimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Zweitstimmen_absolut = sum(Zweitstimmen_absolut, na.rm = T)), 
            by = c("Name", "Partei")) %>%
  ungroup() %>%
  mutate(dataset = "Wahlkreisergebnis")
   

k_analyse <-
  k_analyse %>%
  left_join(k_analyse %>%
              group_by(Name) %>%
              summarise(Zweitstimmen_Summe = sum(Zweitstimmen_absolut, na.rm = T),
                        Erststimmen_Summe = sum(Erststimmen_absolut, na.rm = T)),
            by = c("Name")
            ) %>%
  mutate(Zweitstimmen_relativ = Zweitstimmen_absolut/Zweitstimmen_Summe,
         Erststimmen_relativ = Erststimmen_absolut/Erststimmen_Summe)

# Daten: G
g <- 
  data_raw %>%
  filter((Satzart == "G") | 
           (Gemeindenr. == "000" & grepl("^9", Wahlbezirksnr.) & !grepl("^5", Kreisnr.))) %>%
  left_join(key_kreise, by = "Kreisnr.")


g_analyse  <- 
  g %>% # Allgemeine Infos
  mutate(Wahlbeteiligung = str_replace(Wahlbeteiligung, ",","\\." ) %>% as.numeric(),
         Erststimme_Quote = `Erststimmen gültige` / Wähler,
         Zweitstimme_Quote = `Zweitstimmen gültige` / Wähler) %>%
  select(Jahr, Id, Name, Wahlberechtigte,
         Wähler, Wahlbeteiligung, Erststimme_Quote, Zweitstimme_Quote) %>%
  left_join(g %>% # Erststimmen
              select(Name, starts_with("Erststimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Erststimme_absolut_(.*)",
                           values_to = "Erststimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Erststimmen_absolut = sum(Erststimmen_absolut, na.rm = T)) %>%
              ungroup(), by = "Name") %>%
  left_join(g %>% # Zweitstimmen
              select(Name, starts_with("Zweitstimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Zweitstimme_absolut_(.*)",
                           values_to = "Zweitstimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Zweitstimmen_absolut = sum(Zweitstimmen_absolut, na.rm = T)), 
            by = c("Name", "Partei")) %>%
  ungroup() %>%
  mutate(dataset = "Gemeindeergebnis") 

g_analyse <-
  g_analyse %>%
  left_join(g_analyse %>%
              group_by(Name) %>%
              summarise(Zweitstimmen_Summe = sum(Zweitstimmen_absolut, na.rm = T),
                        Erststimmen_Summe = sum(Erststimmen_absolut, na.rm = T)),
            by = c("Name")
            ) %>%
  mutate(Zweitstimmen_relativ = Zweitstimmen_absolut/Zweitstimmen_Summe,
         Erststimmen_relativ = Erststimmen_absolut/Erststimmen_Summe)

# Daten: LK

lk_analyse <-
  g %>% # Allgemeine Infos
  mutate(Wahlbeteiligung = str_replace(Wahlbeteiligung, ",","\\." ) %>% as.numeric(),
         Erststimme_Quote = `Erststimmen gültige` / Wähler,
         Zweitstimme_Quote = `Zweitstimmen gültige` / Wähler) %>%
  select(Jahr, Id, Name, Landkreis,
         Wähler, Erststimme_Quote, Zweitstimme_Quote) %>%
  left_join(g %>% 
              mutate(Wahlbeteiligung = str_replace(Wahlbeteiligung, ",","\\." ) %>% as.numeric()) %>%
              group_by(Landkreis) %>%
              summarise(Wahlbeteiligung = round(mean(Wahlbeteiligung, na.rm = T), digits = 1),
                        Wahlberechtigte = sum(Wahlberechtigte, na.rm = T)),
            by = "Landkreis") %>%
  left_join(g %>% # Erststimmen
              select(Name, starts_with("Erststimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Erststimme_absolut_(.*)",
                           values_to = "Erststimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Erststimmen_absolut = sum(Erststimmen_absolut, na.rm = T)) %>%
              ungroup(), by = "Name") %>%
  left_join(g %>% # Zweitstimmen
              select(Name, starts_with("Zweitstimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Zweitstimme_absolut_(.*)",
                           values_to = "Zweitstimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Zweitstimmen_absolut = sum(Zweitstimmen_absolut, na.rm = T)), 
            by = c("Name", "Partei")) %>%
  ungroup() %>%
  mutate(dataset = "Landkreisergebnis") 

lk_analyse <-
  lk_analyse %>%
  group_by(Landkreis, Partei) %>%
  select(-Erststimmen_absolut, - Zweitstimmen_absolut, -Name) %>%
  slice(1) %>%
  left_join(lk_analyse %>%
              group_by(Landkreis, Partei) %>%
              summarise(Erststimmen_absolut = sum(Erststimmen_absolut, na.rm = T),
                        Zweitstimmen_absolut = sum(Zweitstimmen_absolut, na.rm = T)
                        ), by = c("Landkreis","Partei")) %>%
  left_join(lk_analyse %>%
              group_by(Landkreis) %>%
              summarise(Zweitstimmen_Summe = sum(Zweitstimmen_absolut, na.rm = T),
                        Erststimmen_Summe = sum(Erststimmen_absolut, na.rm = T)),
            by = c("Landkreis")
            ) %>%
  mutate(Zweitstimmen_relativ = Zweitstimmen_absolut/Zweitstimmen_Summe,
         Erststimmen_relativ = Erststimmen_absolut/Erststimmen_Summe)

# Daten: OT
ot <-
  data_raw %>%
  filter(is.na(Satzart)) 
  
ot_analyse  <- 
  ot %>% # Allgemeine Infos
  mutate(Wahlbeteiligung = str_replace(Wahlbeteiligung, ",","\\." ) %>% as.numeric(),
         Erststimme_Quote = `Erststimmen gültige` / Wähler,
         Zweitstimme_Quote = `Zweitstimmen gültige` / Wähler) %>%
  select(Jahr, Id, Name, Wahlberechtigte,
         Wähler, Wahlbeteiligung, Erststimme_Quote, Zweitstimme_Quote) %>%
  left_join(ot %>% # Erststimmen
              select(Name, starts_with("Erststimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Erststimme_absolut_(.*)",
                           values_to = "Erststimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Erststimmen_absolut = sum(Erststimmen_absolut, na.rm = T)) %>%
              ungroup(), by = "Name") %>%
  left_join(ot %>% # Zweitstimmen
              select(Name, starts_with("Zweitstimme_absolut")) %>%
              pivot_longer(cols = -c(Name),
                           names_to = "Partei",
                           names_pattern = "Zweitstimme_absolut_(.*)",
                           values_to = "Zweitstimmen_absolut") %>%
              group_by(Name, Partei) %>%
              summarise(Zweitstimmen_absolut = sum(Zweitstimmen_absolut, na.rm = T)), 
            by = c("Name", "Partei")) %>%
  ungroup() %>%
  mutate(dataset = "Stadt-/Ortsteilergebnisse") 

ot_analyse <-
  ot_analyse %>%
  left_join(ot_analyse %>%
              group_by(Name) %>%
              summarise(Zweitstimmen_Summe = sum(Zweitstimmen_absolut, na.rm = T),
                        Erststimmen_Summe = sum(Erststimmen_absolut, na.rm = T)),
            by = c("Name")
            ) %>%
  mutate(Zweitstimmen_relativ = Zweitstimmen_absolut/Zweitstimmen_Summe,
         Erststimmen_relativ = Erststimmen_absolut/Erststimmen_Summe)

daten_2025 <- bind_rows(l_analyse, k_analyse, lk_analyse, g_analyse, ot_analyse)
