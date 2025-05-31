# Charger les librairies
library(tidyverse)
library(moments)
library(ggplot2)
library(dplyr)
library(tidyr)
library(randtests)
# Importer le CSV
# renommer pour plus de clarté


df <- read_csv("indices_close.csv") %>%
  rename(
    SP500      = `^GSPC`,
    EUSTOXX600 = `^STOXX`
  ) %>%
  mutate(Date = as.Date(Date))

# Calculer les rendements simples journaliers
df_ret <- df %>% 
  arrange(Date) %>% 
  mutate(
    ret_SP500       = (SP500 - lag(SP500)) / lag(SP500),
    ret_EUROSTOXX600 = (EUSTOXX600 - lag(EUSTOXX600)) / lag(EUSTOXX600)
  ) %>% 
  drop_na()

# Statistiques descriptives de base
summary_stats <- df_ret %>% 
  summarise(
    mean_SP500   = mean(ret_SP500),
    med_SP500    = median(ret_SP500),
    sd_SP500     = sd(ret_SP500),
    skew_SP500   = skewness(ret_SP500),
    kurt_SP500   = kurtosis(ret_SP500),
    mean_EU      = mean(ret_EUROSTOXX600),
    med_EU       = median(ret_EUROSTOXX600),
    sd_EU        = sd(ret_EUROSTOXX600),
    skew_EU      = skewness(ret_EUROSTOXX600),
    kurt_EU      = kurtosis(ret_EUROSTOXX600)
  )

print(summary_stats)

# Visualisations initiales
# Crée le graphique et le stocke dans un objet
plot_histos <- df_ret %>%
  pivot_longer(c(ret_SP500, ret_EUROSTOXX600), 
               names_to = "Indice", values_to = "Rendement") %>%
  ggplot(aes(x = Rendement)) +
  geom_histogram(bins = 50, alpha = 0.6) +
  facet_wrap(~ Indice, scales = "free") +
  labs(
    title = "Histogrammes des rendements journaliers",
    x = "Rendement",
    y = "Effectif"
  )

png("histogrammes_rendements.png", width = 1000, height = 600)
print(plot_histos)
dev.off()

# Densités comparées

df_ret %>% 
  ggplot(aes(x = ret_SP500)) +
  geom_density() +
  geom_density(aes(x = ret_EUROSTOXX600), linetype = "dashed") +
  labs(title = "Densités comparées (SP500 plein vs EURO STOXX 600 pointillé)",
       x = "Rendement", y = "Densité")

png("densites_SP500_EUROSTOXX600.png", width = 800, height = 600)
print(plot_densites)
dev.off()


# Évolution cumulative d’une mise de 1€
# Création du graphique et assignation à 'p'
p <- df_ret %>%
  mutate(
    cum_SP500 = cumprod(1 + ret_SP500),
    cum_EU    = cumprod(1 + ret_EUROSTOXX600)
  ) %>%
  select(Date, cum_SP500, cum_EU) %>%
  pivot_longer(-Date, names_to = "Indice", values_to = "Valeur") %>%
  ggplot(aes(x = Date, y = Valeur, color = Indice)) +
  geom_line() +
  labs(
    title = "Évolution cumulative d’un investissement de 1€",
    y     = "Capital",
    x     = "Date"
  )

ggsave(
  filename = "graphique_performance_cumulee.png",
  plot     = p,
  width    = 10,
  height   = 6,
  units    = "in",
  dpi      = 300,
  device   = "png"
)

# Sauvegarde des graphiques ACF côte-à-côte
png(filename = "acf_indices.png", width = 800, height = 400)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
acf(df_ret$ret_SP500,    main = "ACF ret_SP500")
acf(df_ret$ret_EUROSTOXX600, main = "ACF ret_EUROSTOXX600")
dev.off()

# Sauvegarde des graphiques ACF des rendements au carré côte-à-côte
png(filename = "acf_sq_indices.png", width = 800, height = 400)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
acf(df_ret$ret_SP500^2,    main = "ACF ret_SP500^2")
acf(df_ret$ret_EUROSTOXX600^2, main = "ACF ret_EUROSTOXX600^2")
dev.off()

# Sauvegarde de la corrélation glissante (60 jours)
png(filename = "rolling_correlation.png", width = 800, height = 400)
plot(df_ret$Date, df_ret$roll_cor60, type = 'l', col = 'steelblue', 
     main = 'Corrélation glissante (60 jours)', xlab = 'Date', ylab = 'Corrélation')
dev.off()

runs_sp500 <- runs.test(df_ret$ret_SP500)
runs_eu    <- runs.test(df_ret$ret_EUROSTOXX600)
print(runs_sp500)
print(runs_eu)



# Charger les librairies nécessaires
library(tidyverse)
library(tseries)       # pour jarque.bera.test()
library(moments)       # pour skewness, kurtosis
library(car)           # pour qqPlot (optionnel)

# Tests non-paramétriques de comparaison de distribution
# Mann–Whitney U (Wilcoxon non apparié)
wilcox_res <- wilcox.test(
  df_ret$ret_SP500,
  df_ret$ret_EUROSTOXX600,
  alternative = "two.sided"
)
print(wilcox_res)

# Kolmogorov–Smirnov
ks_res <- ks.test(
  df_ret$ret_SP500,
  df_ret$ret_EUROSTOXX600
)
print(ks_res)


# QQ‐plots et tests de normalité
png(filename = "qqplots.png", width = 800, height = 400)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
# QQ-plot SP500 vs normale
qqnorm(df_ret$ret_SP500, main = "QQ-plot ret_SP500")
qqline(df_ret$ret_SP500)

# QQ-plot EUSTOXX600 vs normale
qqnorm(df_ret$ret_EUROSTOXX600, main = "QQ-plot ret_EUROSTOXX600")
qqline(df_ret$ret_EUROSTOXX600)
dev.off()


# Shapiro–Wilk
shapiro_sp <- shapiro.test(df_ret$ret_SP500)
shapiro_eu <- shapiro.test(df_ret$ret_EUROSTOXX600)
print(shapiro_sp)
print(shapiro_eu)

# Jarque–Bera
jb_sp <- jarque.bera.test(df_ret$ret_SP500)
jb_eu <- jarque.bera.test(df_ret$ret_EUROSTOXX600)
print(jb_sp)
print(jb_eu)

# Spearman
spearman_res <- cor.test(
  df_ret$ret_SP500,
  df_ret$ret_EUROSTOXX600,
  method = "spearman"
)
print(spearman_res)

# Kendall
kendall_res <- cor.test(
  df_ret$ret_SP500,
  df_ret$ret_EUROSTOXX600,
  method = "kendall"
)
print(kendall_res)



# Modèle LOESS
loess_mod <- loess(
  ret_SP500 ~ ret_EUROSTOXX600,
  data = df_ret,
  span = 0.3
)

# Création d'une grille pour la prédiction
grid <- seq(
  min(df_ret$ret_EUROSTOXX600, na.rm = TRUE),
  max(df_ret$ret_EUROSTOXX600, na.rm = TRUE),
  length.out = 200
)
pred_df <- data.frame(
  ret_EUROSTOXX600 = grid,
  pred_SP500       = predict(loess_mod, newdata = data.frame(ret_EUROSTOXX600 = grid))
)

# Tracé nuage + courbe LOESS sur data et pred_df
png(filename = "loess_regression.png",
    width    = 800,     
    height   = 600,     
    res      = 100)   
ggplot(df_ret, aes(x = ret_EUROSTOXX600, y = ret_SP500)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_line(
    data = pred_df,
    aes(x = ret_EUROSTOXX600, y = pred_SP500),
    color = "blue", size = 1
  ) +
  labs(
    title = "Régression non-paramétrique LOESS",
    x     = "ret_EUROSTOXX600",
    y     = "ret_SP500"
  ) +
  theme_minimal()
dev.off()

# Régression Nadaraya–Watson
# Préparation des données
x <- df_ret$ret_EUROSTOXX600
y <- df_ret$ret_SP500

# Nadaraya–Watson via ksmooth
bw <- 0.1  
nw_ks <- ksmooth(x, y, kernel = "normal", bandwidth = bw)

png(filename = "nw_vs_loess.png",
    width    = 800,    
    height   = 600,    
    res      = 100)    

# Tracé comparatif
plot(x, y, pch = 20, cex = 0.5,
     xlab = "ret_EUROSTOXX600", ylab = "ret_SP500",
     main = "NW (ksmooth) vs LOESS")

# Ajouter NW (ksmooth)
lines(nw_ks, col = "black", lwd = 2)

# Ajouter LOESS
lo_mod <- loess(y ~ x, span = 0.3)
grid <- seq(min(x), max(x), length.out = 200)
lines(grid, predict(lo_mod, newdata = data.frame(x = grid)),
      col = "blue", lwd = 2, lty = 2)

legend("topleft",
       legend = c("NW (ksmooth)", "LOESS"),
       col    = c("black",     "blue"),
       lty    = c(1,           2),
       lwd    = c(2,           2))
dev.off()












