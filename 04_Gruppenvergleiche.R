# =========================================================
# 04_Gruppenvergleiche.R
# Gruppenvergleiche mittels t-Test
# GUESSS 2018 – Schweiz
# =========================================================
# Ziel:
# Vergleich der wahrgenommenen sozialen Normen zwischen
# Studierenden mit und ohne tatsächliche Gründungsaktivität
#
# Methode:
# Unabhängige t-Tests (Welch)
#
# Autor: Ermin Zoronjic, Salih Hasicic
# Datum: 19.12.2025
# =========================================================

# ---------------------------
# 0) Libraries
# ---------------------------
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
# 2) Gruppenvariable:
#    Entrepreneurial Behavior
# ---------------------------
# Aktiv = im Gründungsprozess ODER bereits gegründet
# Keine Aktivität = keine unternehmerische Aktivität

data <- data %>%
  mutate(
    aktiv = case_when(
      Q2.2 == 1 | Q2.3 == 1 ~ 1,
      Q2.2 == 0 & Q2.3 == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    aktiv = factor(
      aktiv,
      levels = c(0, 1),
      labels = c("Keine Aktivität", "Aktiv")
    )
  )

# Check Gruppengrössen
table(data$aktiv, useNA = "ifany")

# ---------------------------
# 3) Soziale Normen (Likert 1–7)
# ---------------------------
# Items:
# Q6.2_1, Q6.2_2, Q6.2_3
# -> als Text/Faktor eingelesen, daher numerisch bereinigen

norm_vars <- c("Q6.2_1", "Q6.2_2", "Q6.2_3")

data <- data %>%
  mutate(
    across(all_of(norm_vars),
           ~ parse_number(as.character(.)))
  )

# Kontrolle (optional)
# summary(data$Q6.2_1)

# ---------------------------
# 4) Deskriptive Statistik
# ---------------------------
desc <- data %>%
  group_by(aktiv) %>%
  summarise(
    across(
      all_of(norm_vars),
      list(
        n = ~ sum(!is.na(.)),
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

print(desc)

# ---------------------------
# 5) t-Tests (Welch)
# ---------------------------
tests <- lapply(norm_vars, function(v) {
  t.test(
    as.formula(paste(v, "~ aktiv")),
    data = data,
    var.equal = FALSE
  )
})

names(tests) <- norm_vars
tests

# ---------------------------
# 6) Multiple-Testing-Korrektur
# ---------------------------
pvals <- sapply(tests, function(x) x$p.value)
p.adjust(pvals, method = "bonferroni")
