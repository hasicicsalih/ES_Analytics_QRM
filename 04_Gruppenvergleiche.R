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

# ---------------------------
# Interpretation
# ---------------------------
# Es wurden unabhängige Welch-t-Tests durchgeführt, um zu prüfen, 
# ob sich die wahrgenommenen sozialen Normen zwischen Studierenden 
# mit tatsächlicher Gründungsaktivität (im Gründungsprozess oder bereits gegründet) 
# und Studierenden ohne Aktivität unterscheiden. In allen drei Norm-Items
# liegen die Mittelwerte der aktiven Gruppe signifikant höher als
# jene der nicht-aktiven Gruppe (alle p < .001). Auch nach Bonferroni-Korrektur
# zur Kontrolle multipler Tests bleiben die Unterschiede statistisch signifikant.
# Dies deutet darauf hin, dass Studierende, die tatsächlich entrepreneurial aktiv
# sind, im Durchschnitt stärkere wahrgenommene Unterstützung bzw. 
# positivere Reaktionen aus ihrem sozialen Umfeld berichten. Diese Ergebnisse
# stützen die Annahme, dass soziale Normen mit dem Übergang von Intention zu 
# tatsächlichem Verhalten in Zusammenhang stehen.

# =========================================================
# 7) Wilcoxon-Rangsummen-Tests
# =========================================================

wilcox_tests <- lapply(norm_vars, function(v) {
  wilcox.test(
    as.formula(paste(v, "~ aktiv")),
    data = data,
    exact = FALSE   # wichtig bei grossen Stichproben
  )
})

names(wilcox_tests) <- norm_vars
wilcox_tests

# p-Werte extrahieren & Bonferroni-Korrektur
wilcox_pvals <- sapply(wilcox_tests, function(x) x$p.value)
p.adjust(wilcox_pvals, method = "bonferroni")

# ---------------------------
# Interpretation (Wilcoxon-Rangsummen-Test)
# ---------------------------
# Zusätzlich zu den parametrischen Welch-t-Tests wurden Wilcoxon-
# Rangsummen-Tests durchgeführt, um die Robustheit der Ergebnisse
# zu überprüfen. Der Wilcoxon-Test ist ein nicht-parametrisches
# Verfahren und erfordert keine Annahmen über die Normalverteilung
# der Daten, was insbesondere bei ordinalskalierten Likert-Skalen
# sinnvoll ist.
#
# Die Ergebnisse zeigen für alle drei Items zu wahrgenommenen
# sozialen Normen signifikante Unterschiede zwischen Studierenden
# mit tatsächlicher Gründungsaktivität (im Gründungsprozess oder
# bereits gegründet) und Studierenden ohne Aktivität (alle p < .001).
# Auch nach Bonferroni-Korrektur zur Kontrolle multipler Tests
# bleiben alle Unterschiede hochsignifikant.
#
# Studierende mit unternehmerischer Aktivität weisen durchgehend
# höhere Ränge in den Norm-Items auf als Studierende ohne Aktivität.
# Dies deutet darauf hin, dass stärkere wahrgenommene soziale
# Unterstützung bzw. positivere erwartete Reaktionen aus dem
# sozialen Umfeld mit tatsächlichem Gründungsverhalten zusammenhängen.
#
# Die Übereinstimmung der Ergebnisse aus den parametrischen t-Tests
# und den nicht-parametrischen Wilcoxon-Tests erhöht die Robustheit
# der Befunde und zeigt, dass die beobachteten Unterschiede nicht
# von Verteilungsannahmen abhängen.

