############################################################
# stat_kennwerte.R
# Teil Person 1: Mittelwert, Median, Modus
############################################################

# Pakete laden -------------------------------------------------------------

library(readxl)
library(dplyr)

# 1) Daten einlesen --------------------------------------------------------

# Passe den Pfad bei Bedarf an (z.B. "./data/...")
df <- read_excel("GUESSS_SWITZERLAND_Final.xlsx",
                 sheet = "GUESSS_SWITZERLAND_Final_2")

# 2) Likert-Variablen in numerisch umkodieren ------------------------------
# '.' soll als Missing (NA) behandelt werden

likert_cols <- c(
  "Q4.1.1_1","Q4.1.1_2","Q4.1.1_3",
  "Q4.1.1_4","Q4.1.1_5","Q4.1.1_6", # Intention
  "Q6.2_1"                          # Soziale Norm: Familie
)

df <- df %>%
  mutate(across(all_of(likert_cols),
                ~ as.numeric(na_if(as.character(.x), "."))))

# 3) Skala EI_mean bilden --------------------------------------------------

ei_items <- c("Q4.1.1_1","Q4.1.1_2","Q4.1.1_3",
              "Q4.1.1_4","Q4.1.1_5","Q4.1.1_6")

ei_mat <- as.matrix(df[, ei_items])

# Mittelwert über die 6 Items, na.rm = TRUE
df$EI_mean <- rowMeans(ei_mat, na.rm = TRUE)

# Optional: EI_mean auf NA setzen, wenn weniger als 3 gültige Antworten
valid_counts <- rowSums(!is.na(ei_mat))
df$EI_mean[valid_counts < 3] <- NA

# 4) Nominale Variablen sicherstellen --------------------------------------

# Falls sie nicht schon numerisch 0/1 sind:
df$Q2.2 <- as.numeric(df$Q2.2)
df$Q2.3 <- as.numeric(df$Q2.3)

# 5) Funktion für statistischen Modus --------------------------------------

# Achtung: base::mode() ist etwas anderes, deshalb eigene Funktion
stat_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 6) Funktion für Kennwerte: Mittelwert, Median, Modus ---------------------

kennwerte <- function(x, name) {
  x_clean <- x[!is.na(x)]
  
  data.frame(
    variable = name,
    N       = length(x_clean),
    mean    = mean(x_clean),
    median  = median(x_clean),
    mode    = stat_mode(x_clean)
  )
}

# 7) Kennwerte für ausgewählte Variablen -----------------------------------

results_mmmo <- bind_rows(
  kennwerte(df$EI_mean,   "EI_mean"),
  kennwerte(df$Q6.2_1,    "Q6.2_1_family"),
  kennwerte(df$Q2.2,      "Q2.2_in_process"),
  kennwerte(df$Q2.3,      "Q2.3_self_employed")
)

print(results_mmmo)

# Person 2 kann hier später Varianz und Standardabweichung ergänzen,
# z.B. direkt in der Funktion 'kennwerte()'.

