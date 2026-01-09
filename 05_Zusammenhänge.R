# =========================================================
# 05_Zusammenhänge.R
# Zusammenhänge (Korrelation / Regression)
# GUESSS 2018 – Schweiz
# =========================================================
# Ziel:
# Prüfen, ob soziale Normen mit (a) Gründungsintention und
# (b) tatsächlicher Gründungsaktivität zusammenhängen.
# =========================================================

library(readxl)
library(dplyr)
library(readr)

# ---------------------------
# 1) Daten laden
# ---------------------------
data <- read_excel(
  "GUESSS_SWITZERLAND_Final.xlsx",
  sheet = "GUESSS_SWITZERLAND_Final_2"
)

# ---------------------------
# 2) Behavior (aktiv vs. nicht aktiv)
# ---------------------------
data <- data %>%
  mutate(
    aktiv = case_when(
      Q2.2 == 1 | Q2.3 == 1 ~ 1,
      Q2.2 == 0 & Q2.3 == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ---------------------------
# 3) Soziale Normen bereinigen + Skala bilden
# ---------------------------
norm_vars <- c("Q6.2_1", "Q6.2_2", "Q6.2_3")

data <- data %>%
  mutate(across(all_of(norm_vars), ~ parse_number(as.character(.)))) %>%
  mutate(
    socnorm_mean = rowMeans(select(., all_of(norm_vars)), na.rm = TRUE)
  )

# Quick check
summary(data$socnorm_mean)

# ---------------------------
# 4) Gründungsintention (falls vorhanden) als Skala
# ---------------------------
# In unserem Projekt waren das:
intent_vars <- c("Q4.1.1_1","Q4.1.1_2","Q4.1.1_3","Q4.1.1_4","Q4.1.1_5","Q4.1.1_6")

# robust: nur berechnen, wenn die Spalten existieren
if (all(intent_vars %in% names(data))) {
  data <- data %>%
    mutate(across(all_of(intent_vars), ~ parse_number(as.character(.)))) %>%
    mutate(
      intention_mean = rowMeans(select(., all_of(intent_vars)), na.rm = TRUE)
    )
} else {
  data$intention_mean <- NA_real_
}

summary(data$intention_mean)

# =========================================================
# 5) TEST 1: Spearman-Korrelation
#    soziale Normen ↔ Intention
# =========================================================
# Spearman ist sinnvoll bei ordinal/pseudo-metrisch (Likert).
# (siehe Sitzung 7, Wahl des Tests nach niedrigstem Skalenniveau)
# -------------------------------------------------------------

cor_socnorm_intent <- cor.test(
  data$socnorm_mean,
  data$intention_mean,
  method = "spearman",
  use = "complete.obs"
)
cor_socnorm_intent

# =========================================================
# 6) TEST 2: Spearman-Korrelation
#    soziale Normen ↔ Aktivität (0/1)
# =========================================================
cor_socnorm_aktiv <- cor.test(
  data$socnorm_mean,
  data$aktiv,
  method = "spearman",
  use = "complete.obs"
)
cor_socnorm_aktiv

# =========================================================
# 7) TEST 3 (optional): Lineare Regression
#    Intention ~ soziale Normen
# =========================================================
# Regression: AV/UV definiert, AV metrisch/pseudo-metrisch
# (Sitzung 7: einfache/multiple Regression)
# ---------------------------------------------------------
lm_model <- lm(intention_mean ~ socnorm_mean, data = data)
summary(lm_model)