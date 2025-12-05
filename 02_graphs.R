############################################################
# 02_graphs.R
# Graphische Darstellung von Merkmalsausprägungen
# Variablen: EI_mean, Q6.2_1 (Familie), Q2.2, Q2.3
############################################################

# 1) Paket laden ----------------------------------------------------------

# Falls readxl noch nicht installiert ist:
# install.packages("readxl")

library(readxl)

# 2) Daten einlesen -------------------------------------------------------

# Pfad ggf. anpassen
df <- read_excel("GUESSS_SWITZERLAND_Final.xlsx",
                 sheet = "GUESSS_SWITZERLAND_Final_2")

# 3) Likert-Variablen in numerisch umkodieren -----------------------------
# '.' soll als Missing (NA) behandelt werden

likert_cols <- c(
  "Q4.1.1_1","Q4.1.1_2","Q4.1.1_3",
  "Q4.1.1_4","Q4.1.1_5","Q4.1.1_6", # Intention
  "Q6.2_1"                          # Soziale Norm: Familie
)

for (col in likert_cols) {
  x <- df[[col]]
  x <- as.character(x)
  x[x == "."] <- NA
  df[[col]] <- as.numeric(x)
}

# 4) Skala EI_mean bilden -------------------------------------------------

ei_items <- c("Q4.1.1_1","Q4.1.1_2","Q4.1.1_3",
              "Q4.1.1_4","Q4.1.1_5","Q4.1.1_6")

ei_mat <- as.matrix(df[, ei_items])

df$EI_mean <- rowMeans(ei_mat, na.rm = TRUE)

valid_counts <- rowSums(!is.na(ei_mat))
df$EI_mean[valid_counts < 3] <- NA

# 5) Nominale Variablen vorbereiten ---------------------------------------

df$Q2.2 <- as.numeric(df$Q2.2)  # 0/1: im Gründungsprozess
df$Q2.3 <- as.numeric(df$Q2.3)  # 0/1: bereits selbständig

# Für Labels in Plots:
df$Q2.2_factor <- factor(df$Q2.2,
                         levels = c(0, 1),
                         labels = c("nein", "ja"))

df$Q2.3_factor <- factor(df$Q2.3,
                         levels = c(0, 1),
                         labels = c("nein", "ja"))

############################################################
# 6) Histogramm: Gründungsintention (EI_mean)
############################################################

# Einfaches Histogramm
hist(df$EI_mean,
     main = "Histogramm der Gründungsintention (EI_mean)",
     xlab = "Gründungsintention (Mittelwert 1–7)",
     ylab = "Häufigkeit",
     breaks = 20)

# Interpretation (für euch im Bericht, nicht im Code):
# - Liegen viele Werte eher im unteren Bereich (z.B. 1–3) oder gleichmässig verteilt?
# - Sieht man eine Schiefe (z.B. rechtssteil, also viele niedrige Intentionen, wenige hohe)?

############################################################
# 7) Boxplot: EI_mean nach Gründungsprozess (Q2.2)
############################################################

boxplot(EI_mean ~ Q2.2_factor,
        data = df,
        main = "Gründungsintention nach Gründungsprozess",
        xlab = "Im Gründungsprozess?",
        ylab = "Gründungsintention (EI_mean)")

# Interpretation:
# - Ist der Median der Intention bei denjenigen im Gründungsprozess höher?
# - Sind die Boxen unterschiedlich hoch (Streuung)?

############################################################
# 8) Histogramm: Soziale Norm Familie (Q6.2_1)
############################################################

# Da es eine Skala 1–7 ist, macht es Sinn, die Klassen passend zu wählen:
hist(df$Q6.2_1,
     main = "Reaktion der Familie auf Unternehmertum (Q6.2_1)",
     xlab = "Erwartete Reaktion Familie (1 = sehr negativ, 7 = sehr positiv)",
     ylab = "Häufigkeit",
     breaks = seq(0.5, 7.5, by = 1))

# Interpretation:
# - Häufen sich die Werte im Bereich 5–7? (=> eher positive Unterstützung)
# - Gibt es überhaupt viele sehr negative Einschätzungen (1–2)?

############################################################
# 9) Balkendiagramm: Gründungsprozess (Q2.2)
############################################################

tab_q22 <- table(df$Q2.2_factor)

barplot(tab_q22,
        main = "Im Gründungsprozess (Q2.2)",
        xlab = "Antwort",
        ylab = "Anzahl Personen")

# Optional: relative Häufigkeiten
prop_q22 <- prop.table(tab_q22)
barplot(prop_q22,
        main = "Im Gründungsprozess (relative Häufigkeit)",
        xlab = "Antwort",
        ylab = "Anteil")

############################################################
# 10) Balkendiagramm: Bereits selbständig (Q2.3)
############################################################

tab_q23 <- table(df$Q2.3_factor)

barplot(tab_q23,
        main = "Bereits selbständig (Q2.3)",
        xlab = "Antwort",
        ylab = "Anzahl Personen")

prop_q23 <- prop.table(tab_q23)
barplot(prop_q23,
        main = "Bereits selbständig (relative Häufigkeit)",
        xlab = "Antwort",
        ylab = "Anteil")

############################################################
# 11) Scatterplot: EI_mean vs. Soziale Norm Familie (Q6.2_1)
############################################################

# Streudiagramm mit Regressionsgerade
plot(df$Q6.2_1, df$EI_mean,
     main = "Gründungsintention und erwartete Familienreaktion",
     xlab = "Erwartete Reaktion Familie (Q6.2_1; 1 = sehr negativ, 7 = sehr positiv)",
     ylab = "Gründungsintention (EI_mean, 1–7)",
     pch = 16,      # gefüllte Punkte
     col = "grey50") 

# Lineare Regressionsgerade hinzufügen
model_ei_family <- lm(EI_mean ~ Q6.2_1, data = df)
abline(model_ei_family, col = "red", lwd = 2)

# Optional: Korrelationskoeffizient ausgeben
cor_ei_family <- cor(df$Q6.2_1, df$EI_mean, use = "complete.obs")
cor_ei_family

