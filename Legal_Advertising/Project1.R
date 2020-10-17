# A sued B, A handled personal injury cases
# B handled only workers compensation cases
# adds resulted in more compensation cases for partner B,
# even though firm worked w personal injury cases, so A
# wants B to cover advertising expenses. 

# ---------------- Setting Up ------------------------

# Set working directory
setwd("Desktop")

# Load data
load("LEGALADV.Rdata")

# View Data
View(LEGALADV)

# Handling Missing Data 
newdata <- LEGALADV[7:48,]
View(newdata)
head(newdata)
# ------------- Installing Packages -----------------------
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

# ------------------ Summary of data -------------------------
str(newdata)
summary(newdata)

# ---------------- Scatterplots -------------------------------

# For workers compensation cases
ggplot(LEGALADV, aes(LEGALADV$ADVEXP6, LEGALADV$NEWWC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Linear plot of NewWC vs AdvExp6") +
  xlab("6-Month Cumulative Spending") +
  ylab("Number of New WC Cases")


# For personal injury cases
ggplot(LEGALADV, aes(LEGALADV$ADVEXP6, LEGALADV$NEWPI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Linear plot of NewPI vs AdvExp6") +
  xlab("6-Month Cumulative Spending") +
  ylab("Number of New PI Cases")

# ----------------- The Models ---------------------------

# NEW_PI = 7.7675 + 0.1129(ADVEXP6)
PI_lm <- lm(formula = NEWPI ~ ADVEXP6, data = newdata)
coefficients(PI_lm)

# NEW_WC = 24.5741 + 0.0098(ADVEXP6)
WC_lm <- lm(formula = NEWWC ~ ADVEXP6, data = newdata)
coefficients(WC_lm)

# ------------------- Bar plots ----------------------------

barplot(newdata$TOTADV,
        main = "Total Advertising Expenses per Month",
        xlab = "Month",
        ylab = "Expenses",
        names.arg = newdata$MONTH,
        col = "darkmagenta")


barplot(newdata$NEWPI,
        main = "New PI Cases per Month",
        xlab = "Month",
        ylab = "New PI Cases",
        names.arg = newdata$MONTH,
        col = "blue")

barplot(newdata$NEWWC,
        main = "New WC Cases per Month",
        xlab = "Month",
        ylab = "New PI Cases",
        names.arg = newdata$MONTH,
        col = "green")

barplot(newdata$ADVEXP6,
        main = "Cumulative Spending per Month",
        xlab = "Month",
        ylab = "6-Month Cumulative Spending",
        names.arg = newdata$MONTH,
        col = "purple",
        horiz = T)


PI_lm <- lm(formula = NEWPI ~ ADVEXP6, data = newdata)
coefficients(PI_lm)

WC_lm <- lm(formula = NEWWC ~ ADVEXP6, data = newdata)
coefficients(WC_lm)

ggplot(newdata, aes(ADVEXP6, NEWPI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Linear Plot of New PI Cases vs AdvExp6") +
  xlab("6-Month Cumulative Spending") +
  ylab("Number of New PI Cases")

cor(newdata$ADVEXP6, newdata$NEWPI, method = "pearson")
summary(PI_lm)
confint(PI_lm, level = 0.95)

ggplot(newdata, aes(ADVEXP6, NEWWC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Linear Plot of New WC Cases vs AdvExp6") +
  xlab("6-Month Cumulative Spending") +
  ylab("Number of New WC Cases")


cor(newdata$ADVEXP6, newdata$NEWWC, method = "pearson")
summary(WC_lm)
confint(WC_lm, level = 0.95)

pval_PI <- summary(PI_lm)$coefficient[,"Pr(>|t|)"][2]
pval_PI

pval_WC <- summary(WC_lm)$coefficient[,"Pr(>|t|)"][2]
pval_WC

cor(newdata$NEWWC, newdata$NEWPI, method = "pearson")

ggplot(newdata, aes(NEWWC, NEWPI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian() +
  theme_bw() +
  ggtitle("Linear Plot of New PI Cases vs New WC cases") +
  xlab("Number of New WC Cases") +
  ylab("Number of New PI Cases")

sqrt(var(newdata$NEWWC))
sqrt(var(newdata$NEWPI))
