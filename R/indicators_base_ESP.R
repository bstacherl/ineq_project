# ------------------------------------------------------------------------
#
# Indicators R-Script ESP
# Autoren: Stacherl & Braid
# Datum: 2019-01-04
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)

# Source the Setup scripts to provide merged household and personal data
source("R/_connection.R")
source("R/_setup_ESP.R")

# Einkommenskonzepte ------------------------------------------------------

# P1 (Eurostat): Gesamte Bevölkerung & equal sharing of resources within household
# fac.inc.1     pre tax factor income nach P1
# nat.inc.1     pre tax national income nach P1
# disp.inc.1    post tax disposable income nach P1

# P2 (wid.world): Nur Personen >= 20 Jahre & partial sharing of resources
# fac.inc.2     pre tax factor income nach P2
# nat.inc.2     pre tax national income nach P2
# disp.inc.2    post tax disposable income nach P2

# Haushaltsebene
# fac.inc.h     pre tax factor income auf Haushaltsebene
# nat.inc.h     pre tax national income auf Haushaltsebene
# disp.inc.h    post tax disposable income auf Haushaltsebene

# Aggregation der Einkommen -----------------------------------------------

# Pre-tax factor income & P1

# Summe des persönlichen Einkommens
silc.rphd <- silc.rphd %>% mutate(pip1 = py010g + 
                                    py050g + py080g)

# Summe der persönlichen Einkommen im Haushalt
silc.rphd <- silc.rphd %>% group_by(id_h) %>% 
  mutate(sum.pip1 = sum(pip1))

# Äquivalenzeinkommen nach P1
silc.rphd <- silc.rphd %>% 
  mutate(fac.inc.1 = ((sum.pip1 + hy110g + 
                         hy040g + hy090g) / hx050))
silc.rphd$pip1 <- NULL
silc.rphd$sum.pip1 <- NULL

# Pre-tax national income & P1

# Summe des persönlichen Einkommens
silc.rphd <- silc.rphd %>% 
  mutate(pip1 = py010g + py050g + py080g + py090g + py100g)

# Summe der persönlichen Einkommen im Haushalt
silc.rphd <- silc.rphd %>% group_by(id_h) %>% 
  mutate(sum.pip1 = sum(pip1))

# Äquivalenzeinkommen nach P1
silc.rphd <- silc.rphd %>% 
  mutate(nat.inc.1 = ((sum.pip1 + hy110g + 
                         hy040g + hy090g) / hx050))
silc.rphd$pip1 <- NULL
silc.rphd$sum.pip1 <- NULL

# Post-tax disposable income & P1

# Summe des persönlichen Einkommens
silc.rphd <- silc.rphd %>% 
  mutate(pip1 = py010g + py050g + py080g + py090g + 
           py100g + py110g + py120g + py130g + py140g)

# Summe der persönlichen Einkommen im Haushalt
silc.rphd <- silc.rphd %>% group_by(id_h) %>%
  mutate(sum.pip1 = sum(pip1))

# Äquivalenzeinkommen nach P1
silc.rphd <- silc.rphd %>% 
  mutate(disp.inc.1 = ((sum.pip1 + hy110g + hy040g + 
                          hy090g + hy050g + hy060g + 
                          hy070g + hy080g - hy120g - 
                          hy130g - hy140g) / hx050))
silc.rphd$pip1 <- NULL
silc.rphd$sum.pip1 <- NULL

# Pre-tax factor income & P2

silc.rphd.2 <- silc.rphd %>% filter(adult == 1)

silc.rphd.2 <- silc.rphd.2 %>% mutate(
  pip2 = py010g + py050g + py080g)
#Haushaltsmitglieder >= 20
silc.rphd.2 <- silc.rphd.2 %>% group_by(id_h) %>% 
  mutate(HM20 = sum(adult))
# End Result
silc.rphd.2 <- silc.rphd.2 %>% 
  mutate(fac.inc.2 = (pip2 + (ifelse((
    hy110g + hy040g + hy090g) == 0, 0, (
      hy110g + hy040g + hy090g) / HM20))))
silc.rphd.2$pip2 <- NULL

# Pre-Tax National Income & P2

silc.rphd.2 <- silc.rphd.2 %>% 
  mutate(nat.inc.2 = fac.inc.2 + py090g + py100g)

# Post-Tax Disposable Income & P2

silc.rphd.2 <- silc.rphd.2 %>% 
  mutate(disp.inc.2 = nat.inc.2 + 
           py110g + py120g + py130g + py140g + 
           (ifelse((hy050g + hy060g + hy070g + hy080g - 
                      hy120g - hy130g - hy140g) >= 0, ((
                        hy050g + hy060g + hy070g + hy080g - 
                          hy120g - hy130g - hy140g) / HM20), 0)))

# Pre-Tax Factor Income auf Haushaltsebene

silc.rphd <- silc.rphd %>% 
  mutate(fac.inc.h = fac.inc.1 * hx050)

# Pre-Tax National Income auf Haushaltsebene

silc.rphd <- silc.rphd %>% 
  mutate(nat.inc.h = nat.inc.1 * hx050)

# Post-Tax Disposable Income & P2

silc.rphd <- silc.rphd %>% 
  mutate(disp.inc.h = disp.inc.1 * hx050)


# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income
silc.rphd.inc <- silc.rphd %>% filter(
  disp.inc.1 > 0 & nat.inc.1 > 0 & fac.inc.1 > 0)
silc.rphd.inc2 <- silc.rphd.2 %>% filter(
    fac.inc.2 > 0 & nat.inc.2 > 0 & disp.inc.2 > 0)
silc.rphd.inc.h <- silc.rphd %>% filter(disp.inc.h > 0)


# Creating Survey Objects -------------------------------------------------

silc.p.svy <- svydesign(ids =  ~ id_h,
                         strata = ~rb020,
                         weights = ~pb040,
                         data = silc.rphd.inc) %>% convey_prep()

silc.p2.svy <- svydesign(ids =  ~ id_h,
                        strata = ~rb020,
                        weights = ~pb040,
                        data = silc.rphd.inc2) %>% convey_prep()

silc.h.svy <- svydesign(ids = ~id_h,
                         strata = ~rb020,
                         weights = ~db090,
                         data = silc.rphd.inc.h) %>% convey_prep()


# Indicators (Personenebene) --------------------------------------------------------------

# Mean Income
mean.1 <- c(
  svymean(~fac.inc.1, silc.p.svy),
  svymean(~nat.inc.1, silc.p.svy),
  svymean(~disp.inc.1, silc.p.svy))

mean.2 <- c(
  svymean(~fac.inc.2, silc.p2.svy),
  svymean(~nat.inc.2, silc.p2.svy),
  svymean(~disp.inc.2, silc.p2.svy))

mean.h <- c(
  svymean(~fac.inc.h, silc.h.svy),
  svymean(~nat.inc.h, silc.h.svy),
  svymean(~disp.inc.h, silc.h.svy))

# Median Income

median.1 <- c(
  svyquantile(~fac.inc.1, silc.p.svy, quantile = c(0.5)),
  svyquantile(~nat.inc.1, silc.p.svy, quantile = c(0.5)),
  svyquantile(~disp.inc.1, silc.p.svy, quantile = c(0.5)))

median.2 <- c(
  svyquantile(~fac.inc.2, silc.p2.svy, quantile = c(0.5)),
  svyquantile(~nat.inc.2, silc.p2.svy, quantile = c(0.5)),
  svyquantile(~disp.inc.2, silc.p2.svy, quantile = c(0.5)))

median.h <- c(
  svyquantile(~fac.inc.h, silc.h.svy, quantile = c(0.5)),
  svyquantile(~nat.inc.h, silc.h.svy, quantile = c(0.5)),
  svyquantile(~disp.inc.h, silc.h.svy, quantile = c(0.5)))

# Quantile Share Ratio P80/P20

QRS.1 <- c(
  svyqsr(~fac.inc.1, silc.p.svy),
  svyqsr(~nat.inc.1, silc.p.svy),
  svyqsr(~disp.inc.1, silc.p.svy))

QRS.2 <- c(
  svyqsr(~fac.inc.2, silc.p2.svy),
  svyqsr(~nat.inc.2, silc.p2.svy),
  svyqsr(~disp.inc.2, silc.p2.svy))

QRS.h <- c(
  svyqsr(~fac.inc.h, silc.h.svy),
  svyqsr(~nat.inc.h, silc.h.svy),
  svyqsr(~disp.inc.h, silc.h.svy))

# Top 10% Income Share

top.1 <- c(
  svytotal(~fac.inc.1, subset(silc.p.svy, fac.inc.1 >= as.numeric(
    svyquantile(~fac.inc.1, silc.p.svy,quantile = c(0.9))))) / 
    svytotal(~fac.inc.1, silc.p.svy),
  svytotal(~nat.inc.1, subset(silc.p.svy, nat.inc.1 >= as.numeric(
    svyquantile(~nat.inc.1, silc.p.svy, quantile = c(0.9))))) / 
    svytotal(~nat.inc.1, silc.p.svy),
  svytotal(~disp.inc.1, subset(silc.p.svy, disp.inc.1 >= as.numeric(
    svyquantile(~disp.inc.1, silc.p.svy, quantile = c(0.9))))) / 
    svytotal(~disp.inc.1, silc.p.svy))

top.2 <- c(
  svytotal(~fac.inc.2, subset(silc.p2.svy, fac.inc.2 >= as.numeric(
    svyquantile(~fac.inc.2, silc.p2.svy,quantile = c(0.9))))) / 
    svytotal(~fac.inc.2, silc.p2.svy),
  svytotal(~nat.inc.2, subset(silc.p2.svy, nat.inc.2 >= as.numeric(
    svyquantile(~nat.inc.2, silc.p2.svy, quantile = c(0.9))))) / 
    svytotal(~nat.inc.2, silc.p2.svy),
  svytotal(~disp.inc.2, subset(silc.p2.svy, disp.inc.2 >= as.numeric(
    svyquantile(~disp.inc.2, silc.p2.svy, quantile = c(0.9))))) / 
    svytotal(~disp.inc.2, silc.p2.svy))

top.h <- c(
  svytotal(~fac.inc.h, subset(silc.h.svy, fac.inc.h >= as.numeric(
    svyquantile(~fac.inc.h, silc.h.svy,quantile = c(0.9))))) / 
    svytotal(~fac.inc.h, silc.h.svy),
  svytotal(~nat.inc.h, subset(silc.h.svy, nat.inc.h >= as.numeric(
    svyquantile(~nat.inc.h, silc.h.svy, quantile = c(0.9))))) / 
    svytotal(~nat.inc.h, silc.h.svy),
  svytotal(~disp.inc.h, subset(silc.h.svy, disp.inc.h >= as.numeric(
    svyquantile(~disp.inc.h, silc.h.svy, quantile = c(0.9))))) / 
    svytotal(~disp.inc.h, silc.h.svy))

# Gini Coefficient

gini.1 <- c(
  svygini(~fac.inc.1, silc.p.svy),
  svygini(~nat.inc.1, silc.p.svy),
  svygini(~disp.inc.1, silc.p.svy))

gini.2 <- c(
  svygini(~fac.inc.2, silc.p2.svy),
  svygini(~nat.inc.2, silc.p2.svy),
  svygini(~disp.inc.2, silc.p2.svy))

gini.h <- c(
  svygini(~fac.inc.h, silc.h.svy),
  svygini(~nat.inc.h, silc.h.svy),
  svygini(~disp.inc.h, silc.h.svy))
