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
library(plyr)

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
# subset to year 2016
# indicators are calculated for 2016 (Zeitreihe für alle Jahre)
silc.rphd.inc <- silc.rphd %>% filter(
  disp.inc.1 > 0 & nat.inc.1 > 0 & 
    fac.inc.1 > 0 & rb010 == 2016)

silc.rphd.inc2 <- silc.rphd.2 %>% filter(
    fac.inc.2 > 0 & nat.inc.2 > 0 & 
      disp.inc.2 > 0 & rb010 == 2016)

silc.rphd.inc.h <- silc.rphd %>% filter(
  fac.inc.h > 0 & nat.inc.h >0 & 
    disp.inc.h > 0 & rb010 ==2016)


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


# Indicators (2016) --------------------------------------------------------------

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

#-------------------------------------------------------

# Zeitreihen -------------------------------------------

# subsets mit allen Jahren

silc.rphd.ts1 <- silc.rphd %>% filter(
  disp.inc.1 > 0 & nat.inc.1 > 0 & fac.inc.1 > 0)

silc.rphd.ts2 <- silc.rphd.2 %>% filter(
  fac.inc.2 > 0 & nat.inc.2 > 0 & disp.inc.2 > 0)

silc.rphd.tsh <- silc.rphd %>% filter(
  fac.inc.h > 0 & nat.inc.h >0 & disp.inc.h > 0)


# Creating Survey Objects -------------------------------------------------

silc.ts1.svy <- svydesign(ids =  ~ id_h,
                        strata = ~rb020,
                        weights = ~pb040,
                        data = silc.rphd.ts1) %>% convey_prep()

silc.ts2.svy <- svydesign(ids =  ~ id_h,
                         strata = ~rb020,
                         weights = ~pb040,
                         data = silc.rphd.ts2) %>% convey_prep()

silc.tsh.svy <- svydesign(ids = ~id_h,
                        strata = ~rb020,
                        weights = ~db090,
                        data = silc.rphd.tsh) %>% convey_prep()

# ---------------------------------------------------------------------

# Indicators (Zeitreihen) -----------------------------------------

# Mean Income

mean.ts1 <- bind_cols(svyby(~fac.inc.1, ~rb010, silc.ts1.svy, 
                            svymean, keep.var = FALSE),
                      svyby(~nat.inc.1, ~rb010, silc.ts1.svy, 
                            svymean, keep.var = FALSE), 
                      svyby(~disp.inc.1, ~rb010, silc.ts1.svy, 
                            svymean, keep.var = FALSE))

mean.ts1 <- rename(mean.ts1, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

mean.ts2 <- bind_cols(svyby(~fac.inc.2, ~rb010, silc.ts2.svy, 
                            svymean, keep.var = FALSE),
                      svyby(~nat.inc.2, ~rb010, silc.ts2.svy, 
                            svymean, keep.var = FALSE), 
                      svyby(~disp.inc.2, ~rb010, silc.ts2.svy, 
                            svymean, keep.var = FALSE))

mean.ts2 <- rename(mean.ts2, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

mean.tsh <- bind_cols(svyby(~fac.inc.h, ~rb010, silc.tsh.svy, 
                            svymean, keep.var = FALSE),
                      svyby(~nat.inc.h, ~rb010, silc.tsh.svy, 
                            svymean, keep.var = FALSE), 
                      svyby(~disp.inc.h, ~rb010, silc.tsh.svy, 
                            svymean, keep.var = FALSE))

mean.tsh <- rename(mean.tsh, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

# Median Income

median.ts1 <- bind_cols(svyby(~fac.inc.1, ~rb010, silc.ts1.svy, 
                              svyquantile, quantiles = 0.5, keep.var = FALSE),
                      svyby(~nat.inc.1, ~rb010, silc.ts1.svy, 
                            svyquantile, quantiles = 0.5, keep.var = FALSE), 
                      svyby(~disp.inc.1, ~rb010, silc.ts1.svy, 
                            svyquantile, quantiles = 0.5, keep.var = FALSE))

meadian.ts1 <- rename(median.ts1, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

median.ts2 <- bind_cols(svyby(~fac.inc.2, ~rb010, silc.ts2.svy, 
                              svyquantile, quantiles = 0.5, keep.var = FALSE),
                      svyby(~nat.inc.2, ~rb010, silc.ts2.svy, 
                            svyquantile, quantiles = 0.5, keep.var = FALSE), 
                      svyby(~disp.inc.2, ~rb010, silc.ts2.svy, 
                            svyquantile, quantiles = 0.5, keep.var = FALSE))

median.ts2 <- rename(median.ts2, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

median.tsh <- bind_cols(svyby(~fac.inc.h, ~rb010, silc.tsh.svy, 
                              svyquantile, quantiles = 0.5, keep.var = FALSE),
                      svyby(~nat.inc.h, ~rb010, silc.tsh.svy, 
                            svyquantile, quantiles = 0.5, keep.var = FALSE), 
                      svyby(~disp.inc.h, ~rb010, silc.tsh.svy, 
                            svyquantile, quantiles = 0.5, keep.var = FALSE))

median.tsh <- rename(median.tsh, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

# Quantile Ratio Share (P80/P20)

qsr.ts1 <- bind_cols(svyby(~fac.inc.1, ~rb010, silc.ts1.svy, 
                            svyqsr, keep.var = FALSE),
                      svyby(~nat.inc.1, ~rb010, silc.ts1.svy, 
                            svyqsr, keep.var = FALSE), 
                      svyby(~disp.inc.1, ~rb010, silc.ts1.svy, 
                            svyqsr, keep.var = FALSE))

qsr.ts1 <- rename(qsr.ts1, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

qsr.ts2 <- bind_cols(svyby(~fac.inc.2, ~rb010, silc.ts2.svy, 
                           svyqsr, keep.var = FALSE),
                      svyby(~nat.inc.2, ~rb010, silc.ts2.svy, 
                            svyqsr, keep.var = FALSE), 
                      svyby(~disp.inc.2, ~rb010, silc.ts2.svy, 
                            svyqsr, keep.var = FALSE))

qsr.ts2 <- rename(qsr.ts2, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

qsr.tsh <- bind_cols(svyby(~fac.inc.h, ~rb010, silc.tsh.svy, 
                           svyqsr, keep.var = FALSE),
                      svyby(~nat.inc.h, ~rb010, silc.tsh.svy, 
                            svyqsr, keep.var = FALSE), 
                      svyby(~disp.inc.h, ~rb010, silc.tsh.svy, 
                            svyqsr, keep.var = FALSE))

qsr.tsh <- rename(qsr.tsh, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

# Top 10% Income share

# 10% share for P1 (factor income, national income and disposable income)
top.ts1.f.svy <- subset(silc.ts1.svy, fac.inc.1 >= as.numeric(
  svyquantile(~fac.inc.1, silc.ts1.svy, quantile=c(0.9))))
topnum <- svyby(~fac.inc.1, ~rb010, top.ts1.f.svy, svytotal)
topden <- svyby(~fac.inc.1, ~rb010, silc.ts1.svy, svytotal)
top.ts1.f <- topnum / topden

top.ts1.n.svy <- subset(silc.ts1.svy, nat.inc.1 >= as.numeric(
  svyquantile(~nat.inc.1, silc.ts1.svy, quantile=c(0.9))))
topnum <- svyby(~nat.inc.1, ~rb010, top.ts1.n.svy, svytotal)
topden <- svyby(~nat.inc.1, ~rb010, silc.ts1.svy, svytotal)
top.ts1.n <- topnum / topden

top.ts1.d.svy <- subset(silc.ts1.svy, disp.inc.1 >= as.numeric(
  svyquantile(~disp.inc.1, silc.ts1.svy, quantile=c(0.9))))
topnum <- svyby(~disp.inc.1, ~rb010, top.ts1.d.svy, svytotal)
topden <- svyby(~disp.inc.1, ~rb010, silc.ts1.svy, svytotal)
top.ts1.d <- topnum / topden

rb010 <- c(2006 : 2017)

top.ts1 <- as.data.frame(cbind(rb010, top.ts1.f[,2], top.ts1.n[,2], top.ts1.d[,2]))

top.ts1 <- rename(top.ts1, c("V2" = "fac.inc",
                             "V3" = "nat.inc",
                             "V4" = "disp.inc" ))

# 10% share for P2 (factor income, national income and disposable income)
top.ts2.f.svy <- subset(silc.ts2.svy, fac.inc.2 >= as.numeric(
  svyquantile(~fac.inc.2, silc.ts2.svy, quantile=c(0.9))))
topnum <- svyby(~fac.inc.2, ~rb010, top.ts2.f.svy, svytotal)
topden <- svyby(~fac.inc.2, ~rb010, silc.ts2.svy, svytotal)
top.ts2.f <- topnum / topden

top.ts2.n.svy <- subset(silc.ts2.svy, nat.inc.2 >= as.numeric(
  svyquantile(~nat.inc.2, silc.ts2.svy, quantile=c(0.9))))
topnum <- svyby(~nat.inc.2, ~rb010, top.ts2.n.svy, svytotal)
topden <- svyby(~nat.inc.2, ~rb010, silc.ts2.svy, svytotal)
top.ts2.n <- topnum / topden

top.ts2.d.svy <- subset(silc.ts2.svy, disp.inc.2 >= as.numeric(
  svyquantile(~disp.inc.2, silc.ts2.svy, quantile=c(0.9))))
topnum <- svyby(~disp.inc.2, ~rb010, top.ts2.d.svy, svytotal)
topden <- svyby(~disp.inc.2, ~rb010, silc.ts2.svy, svytotal)
top.ts2.d <- topnum / topden

top.ts2 <- as.data.frame(cbind(rb010, top.ts2.f[,2], top.ts2.n[,2], top.ts2.d[,2]))

top.ts2 <- rename(top.ts2, c("V2" = "fac.inc",
                             "V3" = "nat.inc",
                             "V4" = "disp.inc" ))

# 10% share for P1 (factor income, national income and disposable income)
top.tsh.f.svy <- subset(silc.tsh.svy, fac.inc.h >= as.numeric(
  svyquantile(~fac.inc.h, silc.tsh.svy, quantile=c(0.9))))
topnum <- svyby(~fac.inc.h, ~rb010, top.tsh.f.svy, svytotal)
topden <- svyby(~fac.inc.h, ~rb010, silc.tsh.svy, svytotal)
top.tsh.f <- topnum / topden

top.tsh.n.svy <- subset(silc.tsh.svy, nat.inc.h >= as.numeric(
  svyquantile(~nat.inc.h, silc.tsh.svy, quantile=c(0.9))))
topnum <- svyby(~nat.inc.h, ~rb010, top.tsh.n.svy, svytotal)
topden <- svyby(~nat.inc.h, ~rb010, silc.tsh.svy, svytotal)
top.tsh.n <- topnum / topden

top.tsh.d.svy <- subset(silc.tsh.svy, disp.inc.h >= as.numeric(
  svyquantile(~disp.inc.h, silc.tsh.svy, quantile=c(0.9))))
topnum <- svyby(~disp.inc.h, ~rb010, top.tsh.d.svy, svytotal)
topden <- svyby(~disp.inc.h, ~rb010, silc.tsh.svy, svytotal)
top.tsh.d <- topnum / topden

top.tsh <- as.data.frame(cbind(rb010, top.tsh.f[,2], top.tsh.n[,2], top.tsh.d[,2]))

top.tsh <- rename(top.tsh, c("V2" = "fac.inc",
                             "V3" = "nat.inc",
                             "V4" = "disp.inc" ))

# Gini Coefficient

gini.ts1 <- bind_cols(svyby(~fac.inc.1, ~rb010, silc.ts1.svy, 
                            svygini, keep.var = FALSE),
                      svyby(~nat.inc.1, ~rb010, silc.ts1.svy, 
                            svygini, keep.var = FALSE), 
                      svyby(~disp.inc.1, ~rb010, silc.ts1.svy, 
                            svygini, keep.var = FALSE))

gini.ts1 <- rename(gini.ts1, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

gini.ts2 <- bind_cols(svyby(~fac.inc.2, ~rb010, silc.ts2.svy, 
                            svygini, keep.var = FALSE),
                      svyby(~nat.inc.2, ~rb010, silc.ts2.svy, 
                            svygini, keep.var = FALSE), 
                      svyby(~disp.inc.2, ~rb010, silc.ts2.svy, 
                            svygini, keep.var = FALSE))

gini.ts2 <- rename(gini.ts2, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

gini.tsh <- bind_cols(svyby(~fac.inc.h, ~rb010, silc.tsh.svy, 
                            svygini, keep.var = FALSE),
                      svyby(~nat.inc.h, ~rb010, silc.tsh.svy, 
                            svygini, keep.var = FALSE), 
                      svyby(~disp.inc.h, ~rb010, silc.tsh.svy, 
                            svygini, keep.var = FALSE))

gini.tsh <- rename(gini.tsh, c(
  "statistic"="fac.inc", "statistic1"="nat.inc", "statistic2"="disp.inc"))

library(ggplot2)
ggplot(gini.ts1, aes(x=rb010, y=fac.inc)) + 
  geom_line(aes(y=fac.inc, color = "Faktoreinkommen")) +
  geom_point(aes(y=fac.inc)) +
  geom_line(aes(y=nat.inc, color = "Nationaleinkommen")) +
  geom_point(aes(y=nat.inc)) +
  geom_line(aes(y=disp.inc, color = "verfügbares Einkommen")) +
  geom_point(aes(y=disp.inc)) +
  labs(x = "", y="Gini") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  ggtitle("Gini Koeffizient Spanien (Personeneinkommen)") + 
  scale_x_continuous(breaks = seq(2005, 2018, by = 2)) +
  scale_y_continuous(breaks = seq(0.25, 0.55, by = 0.05), limits = c(0.25, 0.55)) +
  scale_color_manual(values = c('Faktoreinkommen' = 'blue', 'Nationaleinkommen' = 'red', "verfügbares Einkommen" = "green"))