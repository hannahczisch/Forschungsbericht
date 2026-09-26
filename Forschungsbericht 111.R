#Benevolenter Sexismus 

#Daten
personen <- read.csv2(file.choose())

# install.packages(c("pwr", "psych", "car", "effectsize"))
library(pwr)
library(psych)
library(car)
library(effectsize)


# 1. a priori Poweranalyse


power_apriori <- pwr.t.test(d = 0.39,          
                            sig.level = 0.05,
                            power = 0.80,
                            type = "two.sample",
                            alternative = "greater") 
power_apriori

ceiling(power_apriori$n)      
2 * ceiling(power_apriori$n)   

plot(power_apriori)

#Gesamtstichprobe: 164


# 2.Ausschlusskriterien
nrow(personen)

unvollstaendig <- personen$Aufgabe_vollstaendig == 0
zuschnell <- personen$ASI_Dauer_min < 2
zulangsam <- personen$ASI_Dauer_min > 20

sum(unvollstaendig)
sum(zuschnell)
sum(zulangsam)

ausschliessen <- unvollstaendig | zulangsam | zuschnell
daten <- personen[!ausschliessen, ]
daten$Gruppe <- factor(daten$Gruppe, levels = c("hetero", "queer"))

nrow(daten)
table(daten$Gruppe)

# 3.deskriptive Daten der Stichprobe

mean(daten$Alter)
sd(daten$Alter)
range(daten$Alter)

tapply(daten$Alter, daten$Gruppe, mean)
tapply(daten$Alter, daten$Gruppe, sd)


# 4.Reliabilität ASI und invertieren 

itemsASI <- daten[, c("ASI_01", "ASI_02", "ASI_03", "ASI_04", "ASI_05",
                      "ASI_06", "ASI_07", "ASI_08", "ASI_09", "ASI_10",
                      "ASI_11", "ASI_12", "ASI_13", "ASI_14", "ASI_15",
                      "ASI_16", "ASI_17", "ASI_18", "ASI_19", "ASI_20",
                      "ASI_21", "ASI_22")]

invertiert <- c(3, 6, 7, 13, 18, 21)
itemsASI[, invertiert] <- 5 - itemsASI[, invertiert]



ItemsHostilerSexismus <- c(2, 4, 5, 7, 10, 11, 14, 15, 16, 18, 21)



ItemsBenevolenterSexismus <- c(1, 3, 6, 8, 9, 12, 13, 17, 19, 20, 22) 

psych::alpha(itemsASI)
psych::alpha(itemsASI[, ItemsHostilerSexismus])
psych::alpha(itemsASI[, ItemsBenevolenterSexismus])


# 5. Deskriptive Statistik (M, SD, Median pro Gruppe)
variablen <- c("ASI_Gesamt", "RT_BS_korrekt_M")
describeBy(daten[, variablen], group = daten$Gruppe, mat = TRUE, digits = 2)

# Unterscheiden sich die Gruppen im ASI? (erklärt, warum der Effekt in der ANCOVA kleiner wird)
t.test(ASI_Gesamt ~ Gruppe, data = daten, var.equal = TRUE)
cohen.d(daten[, c("ASI_Gesamt", "Gruppe")], group = "Gruppe")


# 6. Hypothese

daten$logRT <- log(daten$RT_BS_korrekt_M)
leveneTest(logRT ~ Gruppe, data = daten)
shapiro.test(daten$logRT[daten$Gruppe == "hetero"])
shapiro.test(daten$logRT[daten$Gruppe == "queer"])

t.test(logRT ~ Gruppe, data = daten,
       alternative = "greater", var.equal = TRUE)
cohen.d(daten[, c("logRT", "Gruppe")], group = "Gruppe")

(1857.53 - 2004.70) / 2004.70 * 100

# 7. Kontrolle des ASI
daten$ASI_c <- daten$ASI_Gesamt - mean(daten$ASI_Gesamt)

# Voraussetzung: Interaktion Gruppe x ASI soll nicht signifikant sein
anova(lm(logRT ~ Gruppe * ASI_c, data = daten))

ancova <- lm(logRT ~ Gruppe + ASI_c, data = daten)

#Voraussetzungen
shapiro.test(residuals(ancova))
leveneTest(residuals(ancova) ~ daten$Gruppe)

Anova(ancova, type = 3)
summary(ancova)                 
eta_squared(Anova(ancova, type = 3), partial = TRUE, alternative = "two.sided")

(exp(-0.05) - 1) * 100 

#8. Plot 

# install.packages("ggplot2")
library(ggplot2)

Abbildung <- ggplot(daten, aes(x = ASI_Gesamt, y = RT_BS_korrekt_M,
                               shape = Gruppe, linetype = Gruppe)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  scale_y_log10(breaks = c(1400, 1800, 2200, 2600)) +
  scale_shape_discrete(labels = c("heterosexuell", "queer")) +
  scale_linetype_discrete(labels = c("heterosexuell", "queer")) +
  labs(x = "ASI-Gesamtwert (0–5)",
       y = "Reaktionszeit (ms, log-skaliert)",
       shape = "Gruppe", linetype = "Gruppe") +
  theme_classic()

Abbildung

ggsave("~/Desktop/abbildung1.png", Abbildung, width = 6, height = 4.5, dpi = 300)


citation()
citation("pwr")
citation("psych")
citation("car")
citation("effectsize")
citation("ggplot2")
