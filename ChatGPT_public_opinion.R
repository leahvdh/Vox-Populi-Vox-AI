# -----------------------------------------------------------------------------------------------------------------------------
# Analysis code for:
# Artificial Intelligence, Unbiased Opinions? Assessing GPTs suitability for estimating public opinion in multi-party systems
# -----------------------------------------------------------------------------------------------------------------------------

# Set working directory -------------------------------------------------------------------------------------------------------
setwd("C:/Users/Alexander/OneDrive/OneDrive - uni-mannheim.de/Dokumente/1 Research/1 In progress/Co-authored/ChatGPT public opinion/data")

# Load packages ---------------------------------------------------------------------------------------------------------------
library(haven)
library(dplyr)

# Load data -------------------------------------------------------------------------------------------------------------------
# GLES Nachwahl-Querschnitt 2017 stored in GESIS Data Archive for the Social Sciences: https://doi.org/10.4232/1.13235
GLES2017_original <- read_dta("ZA6801_de_v4-0-1.dta")

# GLES Nachwahl-Querschnitt 2021 stored in GESIS Data Archive for the Social Sciences: https://doi.org/10.4232/1.14074

# Recode variables ------------------------------------------------------------------------------------------------------------
GLES2017 <- GLES2017_original %>%
  filter(q2d == 1) %>% # restrict to eligible voters
  mutate(
    age = 2017 - q2c,
    female = ifelse(q1 == 2, 1,
                    ifelse(q1 == 1, 0, NA)),
    edu = ifelse(q136m == 1 | q136l == 1 | q136k == 1 | q136j == 1, 5,
                 ifelse(q135 == 4 | q135 == 5, 4,
                        ifelse(q135 == 3 | q135 == 6, 3,
                               ifelse(q135 == 2, 2,
                                      ifelse(q135 == 1 | q135 == 9, 1, NA))))),
    emp = ifelse(q137 == 1 | q137 == 2 | q137 == 8 | q137 == 11, 1,
                 ifelse(q137 == 7 | q137 == 10 | q137 == 12, 2,
                        ifelse(q137 == 3 | q137 == 4 | q137 == 5 | q137 == 6 | q137 == 9, 3, NA))),
    hhincome = ifelse(q192 > 0, q192, NA),
    hhincome = ifelse(hhincome <= 5, 1,
                      ifelse(hhincome >= 6 & hhincome <= 10, 2,
                             ifelse(hhincome >= 11, 3, NA))),
    east = ifelse(ostwest2 == 0, 1,
                  ifelse(ostwest2 == 1, 0, NA)),
    religious = ifelse(q170 > 0, q170, NA),
    leftright = ifelse(q32 == 1 | q32 == 2, 1,
                       ifelse(q32 == 3 | q32 == 4, 2,
                              ifelse(q32 == 5 | q32 == 6 | q32 == 7, 3,
                                     ifelse(q32 == 8 | q32 == 9, 4,
                                            ifelse(q32 == 10 | q32 == 11, 5, NA))))),
    partyid = ifelse(q125a == 1 | q125a == 2 | q125a == 3, 1,
                     ifelse(q125a == 4, 2,
                            ifelse(q125a == 5, 3,
                                   ifelse(q125a == 6, 4,
                                          ifelse(q125a == 7, 5,
                                                 ifelse(q125a == 322, 6,
                                                        ifelse(q125a == 801, 7,
                                                               ifelse(q125a == 808, 8, NA)))))))),
    partyid_degree = ifelse(q126 == -97, 6,
                            ifelse(q126 > 0, q126, NA)),
    inequality = ifelse(q66d == 1 | q66d == 2, 1,
                        ifelse(q66d == 3, 2,
                               ifelse(q66d == 4 | q66d == 5, 3, NA))),
    immigration = ifelse(q79 > 0, q79, NA),
    immigration = ifelse(immigration <= 5, 1,
                         ifelse(immigration == 6, 2,
                                ifelse(immigration >= 7, 3, NA))),
    vote = ifelse(q19ba == 1, 1,
                  ifelse(q19ba == 4, 2,
                         ifelse(q19ba == 6, 3,
                                ifelse(q19ba == 5, 4,
                                       ifelse(q19ba == 7, 5,
                                              ifelse(q19ba == 322, 6,
                                                     ifelse(q19ba == 801, 7,
                                                            ifelse(q19ba == -83, 8,
                                                                   ifelse(q17 == 2, 9, NA))))))))),
    female = factor(female, levels = c(0, 1), labels = c("männlich", "weiblich")),
    edu = factor(edu, levels = c(1, 2, 3, 4, 5), 
                 labels = c("keinen Schulabschluss", "einen Hauptschulabschluss", "einen Realschulabschluss", "Abitur", "einen Hochschulabschluss")),
    emp = factor(emp, levels = c(1, 2, 3), labels = c("berufstätig", "nicht berufstätig", "in Ausbildung")),
    hhincome = factor(hhincome, levels = c(1, 2, 3), 
                      labels = c("niedriges", "mittleres", "hohes")),
    east = factor(east, levels = c(0, 1), labels = c("Westdeutschland", "Ostdeutschland")),
    religious = factor(religious, levels = c(1, 2, 3, 4), 
                       labels = c("überhaupt nicht religiös", "nicht sehr religiös", "etwas religiös", "sehr religiös")),
    leftright = factor(leftright, levels = c(1, 2, 3, 4, 5),
                       labels = c("stark links", "mäßig links", "in der Mitte", "mäßig rechts", "stark rechts")),
    partyid = factor(partyid, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c("mit der Partei CDU/CSU", "mit der Partei SPD", "mit der Partei FDP", "mit der Partei Bündnis 90/Die Grünen", "mit der Partei Die Linke", "mit der Partei AfD", "mit einer Kleinpartei", "mit keiner Partei")),
    partyid_degree = factor(partyid_degree, levels = c(1, 2, 3, 4, 5, 6),
                            labels = c("sehr stark", "ziemlich stark", "mäßig", "ziemlich schwach", "sehr schwach", "")),
    party = paste(partyid_degree, partyid),
    
    inequality = factor(inequality, levels = c(1, 2, 3),
                        labels = c("Maßnahmen ergreifen", "neutral", "keine Maßnahmen ergreifen")),
    immigration = factor(immigration, levels = c(1, 2, 3), 
                         labels = c("erleichtern", "weder erleichtern noch einschränken", "einschränken")),
    vote = factor(vote, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                  labels = c("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "FDP", "Die Linke", "AfD", "Andere Partei", "Ungültig gewählt", "Nicht gewählt"))
  ) %>%
  select(
    lfdn,
    age,
    female,
    edu,
    emp,
    hhincome,
    east,
    religious,
    leftright,
    partyid,
    partyid_degree,
    party,
    inequality,
    immigration,
    vote
  ) %>%
  na.omit() %>%
  mutate(
    prompt = paste0("Ich bin ", age, 
                    " Jahre alt und ", female, 
                    ". Ich habe ", edu, 
                    ", ein ", hhincome, " monatliches Haushalts-Nettoeinkommen",
                    " und ich bin ", emp,
                    ". Ich bin ", religious,
                    ". Politisch-ideologisch ordne ich mich ", leftright, " ein.",
                    " Ich identifiziere mich ", party,
                    ". Ich lebe in ", east,
                    ". Ich finde, die Regierung sollte die Einwanderung ", immigration,
                    " und ", inequality, ", um die Einkommensunterschiede zu verringern.",
                    " Habe ich bei der Bundestagswahl 2017 gewählt und wenn ja, welcher Partei habe ich meine Zweitstimme gegeben? Ich [INSERT]")
  )

# Save data
save(GLES2017, file = "GLES2017.Rdata")
