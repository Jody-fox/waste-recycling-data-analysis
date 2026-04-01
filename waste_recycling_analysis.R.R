install.packages("readxl")

#avvio i pacchetti che mi servono 
library(readxl)
library(corrplot)
library(ggplot2)
library(scales)
library(boot)
library(leaps)
library(correlation)
library(tidyverse)
library(forcats) 
library(broom)



#leggo i dati dal foglio excel in questione
dati <- read_excel("C:\\Users\\Jody\\Desktop\\emilia_romagna_report.xlsx")
View(dati)
head(dati, 10)



#RIMOZIONE CELLE VUOTE
dim(dati)
dati <- na.omit(dati)
dim(dati)  

names(dati)





#pulizia variaibli categoriche e con anno sbagliato
#Sono state eliminate le variabili categoriche e identificative e rimosse le misure
#di superficie ridondanti o puramente dimensionali, mantenendo solo quelle più interpretabili e
#informative ai fini del modello di regressione.
dati_num2 <- dati[ , -c(1, 2, 3, 4, 8, 11, 13, 14, 15, 20)] 
names(dati_num2)
dim(dati_num2)


names(dati_num2)


#statistiche sul dataset
summary(dati_num2) 













#grafici per data vis -> PRESENTAZIONE INIZIALE REPORT!!!


#Istogrammi frequenza-differenziata
ggplot(dati, aes(x = Differenziata_2022)) +
  geom_histogram(bins = 25, fill="#81c784", color="white", alpha=.8) +
  geom_density(color="#1b5e20", linewidth=1.2) +
  theme_minimal(base_size = 13) +
  labs(title="Distribuzione della raccolta differenziata (2022)",
       x="Raccolta differenziata (%)",
       y="Frequenza")

#Boxplot provinciale 
ggplot(dati, aes(x = Provincia, y = Differenziata_2022)) +
  geom_boxplot(fill="#90caf9") +
  theme_minimal() +
  labs(title="Distribuzione della raccolta differenziata per provincia")


#Grafico a barre: Media della RD per provincia
dati %>%
  group_by(Provincia) %>%
  summarise(media_RD = mean(Differenziata_2022)) %>%
  ggplot(aes(x = reorder(Provincia, -media_RD), y = media_RD)) +
  geom_col(fill="#4fc3f7") +
  geom_text(aes(label=sprintf("%.1f%%", media_RD*100)),
            vjust=-0.3, size=3.5) +
  theme_minimal(base_size = 13) +
  labs(title = "Media della raccolta differenziata per provincia",
       x = "Provincia", y = "Media RD (%)")










#grafico correlazioni dopo aver tolto le colonne categoriche
cor(dati_num2)
cor <- cor(dati_num2)
corrplot(cor, type = "upper", tl.col = "black", tl.srt = 45 )
#correlazione positive -> Partecipazione_elettorale 0.41168671 /// REDDITO 0.30404106 
#correlazione negative -> supforestale  -0.73811788  ///  digitdivide -0.60955601  ///  auto1000ab  -0.44145803 









set.seed(1)
train <- sample(nrow(dati_num2), 262)


dati_train <- dati_num2[train, ]
dati_test  <- dati_num2[-train, ]



#REGRESSIONE LINEARE con tutte le variabili dentro che chiamo con estensione "TT"
lm.fit_tt <- lm(Differenziata_2022 ~ ., data = dati_train)  
summary(lm.fit_tt)
pred_test <- predict(lm.fit_tt, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
#Multiple R-squared:  0.6137,	Adjusted R-squared:  0.5935 
#MSE -> 0.01087973
#variabili significative (3 e 2 stelline) -> AUTO1000  - SUPFORESTALE - DIGITDIVIDE


##Creiamo una variabile "digi" --> usando un ciclo if
#SERVE PER SMISTARE I COMUNI DICHIARATI CON UN PROBLEMA COL DIGIT_DIVIDE
dati$digi <- ifelse(dati$Digitaldivide_2011 >= 30, "not digital", "digital")


#GRAFICO CHE CI DICE CHE I COMUNI NON DIGITALI SONO LA MINORANZA QUASI AL 30%
# prepariamo i dati con percentuali
dati_pie <- dati %>%
  count(digi) %>%
  mutate(
    perc = n / sum(n),
    label = percent(perc, accuracy = 1)
  )

ggplot(dati_pie, aes(x = 2, y = n, fill = digi)) +
  geom_col(width = 1, color = "white", linewidth = 0.8) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +   # crea il "buco" centrale (donut soft)
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 6,        # <<< PERCENTUALI PIÙ GRANDI
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "digital" = "#66bb6a",
      "not digital" = "#ef5350"
    ),
    labels = c("Digital", "Digital divide elevato")
  ) +
  labs(
    title = "Comuni per livello di digitalizzazione",
    subtitle = "I comuni con digital divide elevato rappresentano il 28%",
    fill = ""
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 11,
      color = "grey40"
    ),
    legend.position = "right",
    legend.text = element_text(size = 11)
  )


#data visualisation in seguito a modello full che registra come variabili a 3stelle proprio le correlate positive
ggplot(
  dati,
  aes(
    x = Supforestale_2020,
    y = Differenziata_2022,
    color = Auto1000ab_2016
  )
) +
  geom_point(alpha = .7, size = 2.4) +
  geom_smooth(se = FALSE, linewidth = 1) +
  facet_wrap(~ digi) +
  scale_color_gradientn(
    colours = rev(c("darkred", "orange", "yellow", "green")),
    name = "Auto per 1000 ab."
  ) +
  labs(
    title = "Relazione tra Superficie Forestale e Raccolta Differenziata",
    subtitle = "Distinti per digital divide",
    x = "Superficie forestale",
    y = "Raccolta differenziata 2022",
    caption = "Fonte: dataset Emilia-Romagna"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )












# 1 PUNTO
### SUBSET ###


##BEST SUBSET METHOD
set.seed(5)

regfit.full <- regsubsets(Differenziata_2022 ~ ., dati_train, nvmax = 13 )
reg.summary <- summary(regfit.full)
reg.summary
names(reg.summary)   

reg.summary$adjr2 

##Visualizziamo i risultati adj_R**2
par(mfrow = c(2, 2))
plot(reg.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)  
points(8, reg.summary$adjr2[8], col = "red", cex = 2, pch = 20)   

##facciamo la stessa cosa anche con Cp e BIC
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(7, reg.summary$cp[7], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(5, reg.summary$bic[5], col = "red", cex = 2,  pch = 20)

#veidamo quali sono le variabili prese in considerazione
dev.off()
plot(regfit.full , scale = "r2")
plot(regfit.full , scale = "adjr2")
plot(regfit.full , scale = "Cp")
plot(regfit.full , scale = "bic")





#PROVIAMO I 2 MODELLI SEGUENDO LE INFO CHE CI DA ADJ-R**2 E BIC

#adj R**2
coef(regfit.full ,8)

#bic
coef(regfit.full ,5) 




#REG LINEARE best_subset_ADJ-R**2
best.model_full_adj <- lm(Differenziata_2022 ~ CO2eq_2016 +
                            Auto1000ab_2016 +
                            `Stazioni ferroviarie_2021`+
                            Digitaldivide_2011 +
                            Reddito_2020 +
                            Spesacomunale_2017 + 
                            Supforestale_2020 +
                            Superficieagricola_2022, data = dati_train)
summary(best.model_full_adj)
pred_test <- predict(best.model_full_adj, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
#Multiple R-squared:  0.6124,	Adjusted R-squared:  0.6002 
#MSE -> 0.01107656


#REG LINEARE best_subset_BIC
best.model_full_bic <- lm(Differenziata_2022 ~ Supforestale_2020 +
                            `Stazioni ferroviarie_2021` +
                            Reddito_2020 +
                            Digitaldivide_2011 +
                            Auto1000ab_2016, data = dati_train) 
summary(best.model_full_bic)
pred_test <- predict(best.model_full_bic, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
#Multiple R-squared:  0.5991,	Adjusted R-squared:  0.5913 
#MSE ->  0.01068958













##FORWARD E BACKWARD STEPWISE SELECTION

#foreward lo chiamerò così -> fwd

regfit.fwd <- regsubsets(Differenziata_2022 ~ ., data = dati_train, nvmax = 13, method = "forward")
summary(regfit.fwd)
reg_fwd.summary <- summary(regfit.fwd)
reg_fwd.summary


par(mfrow = c(2, 2))
plot(reg_fwd.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_fwd.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

which.max(reg_fwd.summary$adjr2)  

points(8, reg_fwd.summary$adjr2[8], col = "red", cex = 2, pch = 20)    


plot(reg_fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg_fwd.summary$cp)
points(7, reg_fwd.summary$cp[7], col = "red", cex = 2, pch = 20)
which.min(reg_fwd.summary$bic)
plot(reg_fwd.summary$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(5, reg_fwd.summary$bic[5], col = "red", cex = 2,  pch = 20)


dev.off()  
plot(regfit.fwd , scale = "r2")
plot(regfit.fwd , scale = "adjr2")
plot(regfit.fwd , scale = "Cp")
plot(regfit.fwd , scale = "bic")


coef(regfit.fwd ,8) 
coef(regfit.fwd ,5) 




# fwd adjR**2 = best_sub_adjR**2
#fwd_bic = best_sub_bic !!!!











#BWD
regfit.bwd <- regsubsets(Differenziata_2022 ~ ., data = dati_train, nvmax = 13, method = "backward")
summary(regfit.bwd)
reg_bwd.summary <- summary(regfit.bwd)
reg_bwd.summary


par(mfrow = c(2, 2))
plot(reg_bwd.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_bwd.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

which.max(reg_bwd.summary$adjr2)  

points(8, reg_bwd.summary$adjr2[8], col = "red", cex = 2, pch = 20)    



plot(reg_bwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg_bwd.summary$cp)
points(7, reg_bwd.summary$cp[7], col = "red", cex = 2, pch = 20)
which.min(reg_bwd.summary$bic)
plot(reg_bwd.summary$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(5, reg_bwd.summary$bic[5], col = "red", cex = 2,  pch = 20)



dev.off()  
plot(regfit.bwd , scale = "r2")
plot(regfit.bwd , scale = "adjr2")
plot(regfit.bwd , scale = "Cp")
plot(regfit.bwd , scale = "bic")


coef(regfit.bwd ,8) 
coef(regfit.bwd ,5) 



# bwd_adjR**2 = best_sub_adjR**2
#bwd_bic = best_sub_bic !!!!














#RICAPITOLANDO ABBIAMO QUESTI 3 MODELLI A CONFRONTO


# - 1
#REGRESSIONE LINEARE con tutte le variabili dentro 
lm.fit_tt <- lm(Differenziata_2022 ~ ., data = dati_train)  
summary(lm.fit_tt)
pred_test <- predict(lm.fit_tt, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
#Multiple R-squared:  0.6137,	Adjusted R-squared:  0.5935 
#MSE -> 0.01087973


# - 2
#REG LINEARE best_subset_ADJ-R**2
best.model_full_adj <- lm(Differenziata_2022 ~ CO2eq_2016 +
                            Auto1000ab_2016 +
                            `Stazioni ferroviarie_2021`+
                            Digitaldivide_2011 +
                            Reddito_2020 +
                            Spesacomunale_2017 + 
                            Supforestale_2020 +
                            Superficieagricola_2022, data = dati_train)
summary(best.model_full_adj)
pred_test <- predict(best.model_full_adj, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
#Multiple R-squared:  0.6124,	Adjusted R-squared:  0.6002 
#MSE -> 0.01107656


# - 3
#REG LINEARE best_subset_BIC
best.model_full_bic <- lm(Differenziata_2022 ~ Supforestale_2020 +
                            `Stazioni ferroviarie_2021` +
                            Reddito_2020 +
                            Digitaldivide_2011 +
                            Auto1000ab_2016, data = dati_train)
summary(best.model_full_bic)
pred_test <- predict(best.model_full_bic, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
#Multiple R-squared:  0.5991,	Adjusted R-squared:  0.5913 
#MSE ->  0.01068958














AIC(lm.fit_tt, best.model_full_adj, best.model_full_bic)
BIC(lm.fit_tt, best.model_full_adj, best.model_full_bic)





#RISULTATI


# 1 - modello "tutte variabili"
#AIC -> -352.1563
#BIC -> -298.6311
#Multiple R-squared:  0.6137,	Adjusted R-squared:  0.5935
#MSE -> 0.01087973


# 2 - modello "best_subset ADJ-R**2"
#AIC -> -361.2610
#BIC -> -325.5776
#Multiple R-squared:  0.6124,	Adjusted R-squared:  0.6002 
#MSE -> 0.01107656


# 3 - modello "best_subset BIC"
#AIC -> -358.4432
#BIC -> -333.4648
#ultiple R-squared:  0.5991,	Adjusted R-squared:  0.5913 
#MSE -> 0.01068958






# --- SOLO 3 MODELLI (tabella aggiornata) ---
results <- tibble::tribble(
  ~Modello,                    ~R2,     ~Adj_R2, ~MSE,        ~AIC,        ~BIC,
  "Tutte le variabili",         0.6137,  0.5935, 0.01087973, -352.1563,  -298.6311,
  "Best subset (Adj-R²)",       0.6124,  0.6002, 0.01107656, -361.2610,  -325.5776,
  "Best subset (BIC)",          0.5991,  0.5913, 0.01068958, -358.4432,  -333.4648
)

# 1) formato lungo (solo da results)
long <- results %>%
  pivot_longer(
    cols = c(R2, Adj_R2, MSE, AIC, BIC),
    names_to = "Metrica",
    values_to = "Valore"
  )

# 2) normalizzo e inverto dove serve (MSE/AIC/BIC: più basso = meglio)
long2 <- long %>%
  group_by(Metrica) %>%
  mutate(
    norm  = (Valore - min(Valore)) / (max(Valore) - min(Valore)),
    score = if_else(Metrica %in% c("MSE", "AIC", "BIC"), 1 - norm, norm)
  ) %>%
  ungroup()

# 3) ordine modelli = quello della tabella, ma al contrario (come avevi tu)
ordine_modelli <- rev(results$Modello)

# 4) ordine metriche
ordine_metriche <- c("Adj_R2", "R2", "MSE", "AIC", "BIC")

long2 <- long2 %>%
  mutate(
    Modello = factor(Modello, levels = ordine_modelli),
    Metrica = factor(Metrica, levels = ordine_metriche)
  )

# 5) plot
ggplot(long2, aes(x = Metrica, y = Modello, fill = score)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(
    data = subset(long2, round(score, 2) != 1),
    aes(label = sprintf("%.2f", score)),
    size = 4,
    color = "black"
  ) +
  geom_text(
    data = subset(long2, round(score, 2) == 1),
    aes(label = sprintf("%.2f", score)),
    size = 6,
    fontface = "bold",
    color = "white"
  ) +
  labs(
    title = "Confronto modelli (score normalizzato 0–1)",
    subtitle = "1.00 = migliore performance per metrica (MSE/AIC/BIC invertiti)",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "grey35")
  )










#REG LINEARE best_subset_ADJ-R**2 è il modello che spiega meglio con un Adjusted R-squared:  0.6002
#VARIABILI SIGNIFICATIVE -> auto 3s - digitdivide 2s - reddito 2s - supforestale 3s




#REG LINEARE best_subset_BIC è il modello che predice meglio con un MSE:  0.01068958
#VARIABILI SIGNIFICATIVE -> auto 3s - stazioni_ferroviarie 3s - digitdivide 2s - reddito 2s - supforestale 3s




















#punto 2 -> RICAMPIONAMENTO modello che scegliamo per spiegare




#LOOCV best_subset_adj_R**2

set.seed(2) 

lm.fit_loocv <- glm(Differenziata_2022 ~ CO2eq_2016 +
                      Auto1000ab_2016 +
                      `Stazioni ferroviarie_2021`+
                      Spesacomunale_2017 +
                      Digitaldivide_2011 +
                      Reddito_2020 +
                      Supforestale_2020 +
                      Superficieagricola_2022, data = dati_train)
coef(lm.fit_loocv)

cv.err <- cv.glm(dati_train, lm.fit_loocv)  

cv.err$delta  

#dopo LOOCV -> 0.01502180 // MSE prima -> 0.01107656 -> PREDIZIONE ROBUSTA



#BOOTSTRAP -> controllo coefficienti


# 0) Formula del modello finale (UNA VOLTA SOLA)
form_finale <- Differenziata_2022 ~ CO2eq_2016 +
  Auto1000ab_2016 +
  `Stazioni ferroviarie_2021` +
  Spesacomunale_2017 +
  Digitaldivide_2011 +
  Reddito_2020 +
  Supforestale_2020 +
  Superficieagricola_2022

# 1) Funzione bootstrap (CORRETTA: prima ricampiona le righe, poi stima)
boot.fn <- function(data, index) {
  data_boot <- data[index, ]                 # ricampionamento con rimpiazzo
  mod <- lm(form_finale, data = data_boot)   # fit sul campione bootstrap
  coef(mod)
}

# 2) Bootstrap sul training set (coerente con la tua validazione)
set.seed(100)
boot_res <- boot(data = dati_train, statistic = boot.fn, R = 2000)

# 3) Nomi coefficienti nell'ordine corretto
coef_names <- names(boot_res$t0)

# 4) CI percentile 95% per ogni coefficiente
get_ci_perc <- function(i) {
  ci <- boot.ci(boot_res, type = "perc", index = i)
  c(lower = ci$percent[4], upper = ci$percent[5])
}
ci_mat <- t(sapply(seq_along(coef_names), get_ci_perc))

# 5) Tabella completa bootstrap
tab_boot <- data.frame(
  Coeff    = coef_names,
  Original = as.numeric(boot_res$t0),
  SE_boot  = apply(boot_res$t, 2, sd),
  CI_low   = ci_mat[, "lower"],
  CI_high  = ci_mat[, "upper"]
) %>%
  mutate(
    includes_0 = (CI_low <= 0 & CI_high >= 0),
    robust_txt = ifelse(includes_0, "NO (include 0)", "YES (exclude 0)")
  )

tab_boot

# 6) Tabella pulita da report (SOLO 8 variabili, NO intercetta)
tab_clean <- tab_boot %>%
  filter(Coeff != "(Intercept)") %>%
  mutate(
    Beta   = Original,
    CI95   = paste0("[", signif(CI_low, 3), ", ", signif(CI_high, 3), "]"),
    Robust = ifelse(includes_0, "NO", "YES"),
    Segno  = ifelse(Beta > 0, "+", "-")
  ) %>%
  select(Variabile = Coeff, Segno, Beta, SE_boot, CI95, Robust) %>%
  mutate(across(c(Beta, SE_boot), ~ signif(.x, 4))) %>%
  arrange(desc(Robust), desc(abs(Beta)))

tab_clean

# 7) Forest plot (coef + CI 95% bootstrap) SOLO 8 variabili
tab_plot <- tab_boot %>%
  filter(Coeff != "(Intercept)") %>%
  mutate(
    Beta = Original,
    Robust = ifelse(includes_0, "NO", "YES"),
    Robust_ord = ifelse(Robust == "YES", 1, 0),
    lab = sprintf("%+.3g", Beta),
    
    # posizionamento etichette in base al segno
    hjust_lab = ifelse(Beta >= 0, 0, 1),
    nudge_lab = ifelse(Beta >= 0, 0.03, -0.03)
  ) %>%
  arrange(desc(Robust_ord), desc(abs(Beta))) %>%
  mutate(Coeff = factor(Coeff, levels = rev(Coeff)))

ggplot(tab_plot,
       aes(x = Beta, y = Coeff, xmin = CI_low, xmax = CI_high, color = Robust)) +
  geom_point(size = 2) +
  geom_errorbarh(height = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(
    data = tab_plot,
    aes(x = Beta + nudge_lab, label = lab, hjust = hjust_lab),
    size = 4,
    show.legend = FALSE
  ) +
  labs(
    title = "Coefficienti del modello finale con IC 95% bootstrap",
    subtitle = "Robustezza: IC bootstrap 95% esclude lo zero",
    x = "Coefficiente stimato",
    y = ""
  ) +
  theme_minimal(base_size = 13)



#VARIABILI SIGNIFICATIVE -> auto 3s - digitdivide 2s - reddito 2s - supforestale 3s
#fortemente  robuste dopo la conferma bootstrap



















#proviamo a migliorare la predizione del modello best_subset_BIC perche era il modello con MSE più basso.


ggplot(dati_train, aes(x = Auto1000ab_2016, y = Differenziata_2022)) + geom_point(alpha = .85, size = 1.8) + geom_smooth()
ggplot(dati_train, aes(x = `Stazioni ferroviarie_2021`, y = Differenziata_2022)) + geom_point(alpha = .85, size = 1.8) + geom_smooth(method = "lm")
ggplot(dati_train, aes(x = Digitaldivide_2011, y = Differenziata_2022)) + geom_point(alpha = .85, size = 1.8) + geom_smooth(method = "lm") 
ggplot(dati_train, aes(x = Reddito_2020, y = Differenziata_2022)) + geom_point(alpha = .85, size = 1.8) + geom_smooth()
ggplot(dati_train, aes(x = Supforestale_2020, y = Differenziata_2022)) + geom_point(alpha = .85, size = 1.8) + geom_smooth(method = "lm")





#tentativi
best.model_full_bic_poly <- lm(Differenziata_2022 ~ poly(Supforestale_2020, 2) +
                                 `Stazioni ferroviarie_2021` +
                                 Reddito_2020 +
                                 Digitaldivide_2011 +
                                 Auto1000ab_2016, data = dati_train)
summary(best.model_full_bic_poly)
pred_test <- predict(best.model_full_bic_poly, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
# mse -> 0.01064501






best.model_full_bic_poly2 <- lm(Differenziata_2022 ~ Supforestale_2020 +
                                 poly(`Stazioni ferroviarie_2021`, 2) +
                                 Reddito_2020 +
                                 Digitaldivide_2011 +
                                 Auto1000ab_2016, data = dati_train)
summary(best.model_full_bic_poly2)
pred_test <- predict(best.model_full_bic_poly2, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
# mse -> 0.01063069






best.model_full_bic_poly3 <- lm(Differenziata_2022 ~ Supforestale_2020 +
                                 `Stazioni ferroviarie_2021` +
                                 poly(Reddito_2020, 2) +
                                 Digitaldivide_2011 +
                                 Auto1000ab_2016, data = dati_train)
summary(best.model_full_bic_poly3)
pred_test <- predict(best.model_full_bic_poly3, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
# mse -> 0.0104967






best.model_full_bic_poly4 <- lm(Differenziata_2022 ~ Supforestale_2020 +
                                 `Stazioni ferroviarie_2021` +
                                 Reddito_2020 +
                                 poly(Digitaldivide_2011, 2) +
                                 Auto1000ab_2016, data = dati_train)
summary(best.model_full_bic_poly4)
pred_test <- predict(best.model_full_bic_poly4, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
# mse -> 0.01078364 -> non migliora








best.model_full_bic_poly5 <- lm(Differenziata_2022 ~ Supforestale_2020 +
                                 `Stazioni ferroviarie_2021` +
                                 Reddito_2020 +
                                 Digitaldivide_2011 +
                                 poly(Auto1000ab_2016, 2), data = dati_train)
summary(best.model_full_bic_poly5)
pred_test <- predict(best.model_full_bic_poly5, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
# mse -> 0.01081788-> non migliora








best.model_full_bic_poly6 <- lm(Differenziata_2022 ~ poly(Supforestale_2020, 2) +
                                  `Stazioni ferroviarie_2021` +
                                  poly(Reddito_2020, 2) +
                                  Digitaldivide_2011 +
                                  Auto1000ab_2016, data = dati_train)
summary(best.model_full_bic_poly6)
pred_test <- predict(best.model_full_bic_poly6, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
# mse -> 0.01047458




best.model_full_bic_poly7 <- lm(Differenziata_2022 ~ poly(Supforestale_2020, 2) +
                                  poly(`Stazioni ferroviarie_2021`,2) +
                                  poly(Reddito_2020, 2) +
                                  Digitaldivide_2011 +
                                  Auto1000ab_2016, data = dati_train)
summary(best.model_full_bic_poly7)
pred_test <- predict(best.model_full_bic_poly7, newdata = dati_test)
mean((dati_test$Differenziata_2022 - pred_test)^2)
# mse ->  0.01045385








#verifica  con LOOCV del mdello best_subset_bic

lm.fit_loocv_bic <- glm(Differenziata_2022 ~ Supforestale_2020 +
                          `Stazioni ferroviarie_2021` +
                          Reddito_2020 +
                          Digitaldivide_2011 +
                          Auto1000ab_2016, data = dati_train)
coef(lm.fit_loocv_bic)

cv.err <- cv.glm(dati_train, lm.fit_loocv_bic)  

cv.err$delta 
#mse con LOOCV -> 0.01489617   -> prima era 0.01068958






#verifica  con LOOCV del mdello best.model_full_bic_poly7
lm.fit_loocv_bic_poly <- glm(Differenziata_2022 ~ poly(Supforestale_2020, 2) +
                               poly(`Stazioni ferroviarie_2021`,2) +
                               poly(Reddito_2020, 2) +
                               Digitaldivide_2011 +
                               Auto1000ab_2016, data = dati_train)
coef(lm.fit_loocv_bic_poly)

cv.err <- cv.glm(dati_train, lm.fit_loocv_bic_poly)  

cv.err$delta  
#mse con LOOCV -> 0.01528543  -> prima era 0.01068958






#verifica  con LOOCV del mdello best.model_full_bic_poly3
lm.fit_loocv_bic_poly2 <- glm(Differenziata_2022 ~ Supforestale_2020 +
                               `Stazioni ferroviarie_2021` +
                               poly(Reddito_2020, 2) +
                               Digitaldivide_2011 +
                               Auto1000ab_2016, data = dati_train)
coef(lm.fit_loocv_bic_poly2)

cv.err <- cv.glm(dati_train, lm.fit_loocv_bic_poly2) 
cv.err$delta 
#mse con LOOCV -> 0.01506447  -> prima era 0.01068958


#Abbiamo testato estensioni polinomiali. Sebbene alcuni modelli riducano
#leggermente l’MSE sul test, la LOOCV aumenta, suggerendo che il guadagno non è robusto. 
#Per questo si preferisce il modello lineare BIC





mse_comp <- tibble::tribble(
  ~Modello,                 ~MSE_test,     ~MSE_LOOCV,
  "BIC lineare",             0.01068958,   0.01489617,
  "BIC poly (tutte 2°)",     0.01045385,   0.01528543,
  "BIC poly(solo Reddi 2°)", 0.01049670,   0.01506447
)

mse_comp

mse_long <- mse_comp %>%
  pivot_longer(cols = c(MSE_test, MSE_LOOCV),
               names_to = "Tipo",
               values_to = "MSE") %>%
  mutate(
    Tipo = recode(Tipo,
                  MSE_test = "MSE test",
                  MSE_LOOCV = "MSE LOOCV"),
    Tipo = factor(Tipo, levels = c("MSE test", "MSE LOOCV")),
    Modello = factor(Modello, levels = rev(mse_comp$Modello))
  )

ggplot(mse_long, aes(x = Modello, y = MSE, fill = Tipo)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = sprintf("%.5f", MSE)),
    position = position_dodge(width = 0.75),
    vjust = -0.4,
    size = 3.8
  ) +
  labs(
    title = "Confronto MSE: test vs LOOCV (modelli BIC)",
    subtitle = "Le polinomiali migliorano leggermente sul test set, ma peggiorano in LOOCV",
    x = "",
    y = "MSE",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )



















pred_test <- predict(best.model_full_bic, newdata = dati_test)

df_pred <- data.frame(
  Osservato = dati_test$Differenziata_2022,
  Predetto  = pred_test
)



ggplot(df_pred, aes(x = Osservato, y = Predetto)) +
  geom_point(alpha = 0.7, size = 3, color = "#2C7FB8") +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  ) +
  labs(
    title = "Valori osservati vs predetti – modello BIC",
    subtitle = paste0(
      "MSE test = ",
      round(mean((df_pred$Osservato - df_pred$Predetto)^2), 5)
    ),
    x = "Raccolta differenziata osservata (%)",
    y = "Raccolta differenziata predetta (%)"
  ) +
  theme_minimal(base_size = 13)




#---------------------------------------------






