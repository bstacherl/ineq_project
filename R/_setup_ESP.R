# Setup -------------------------------------------------------------------

library(dplyr)
if(!exists(c("country", "year"))) {
  stop("Please specify country and year.")
}

country <- "ES"
year <- c("2006", "2007", "2008", "2009", 
          "2010", "2011", "2012", "2013")
# all income values for 2004 and 2005 for Spain are NAs
# Zeitreihe beginnt ab 2006

# Prepare Data ------------------------------------------------------------

# Income Measures
# 1. Pre-tax factor income (Canberra: primary income) (1+2)
# Einkommen aus Arbeit (1)
# PY010G    gross employee cash or near cash income 
# PY021G    company car
# PY050G    gross cash benefits or losses from self-employment (including royalties)
# HY110G    income received by people aged under 16
# Vermögenseinkommen (2)
# HY040G    income from rental of a property or land
# HY090G    interests, dividends, profit from capital investments in unincorporated business
# PY080G    pensions received from individual private plans
# 2. Pre-tax national income (1+2+3)
# Pensionen + Arbeitslosengeld (3)
# PY090G    unemployment benefits
# PY100G    old-age benefits
# 3. Post-tax disposable income (1+2+3+4-5)
# Alle anderen erhaltenen Transferzahlungen (4)
# PY110G    survivor' benefits 
# PY120G    sickness benefits 
# PY130G    disability benefits
# PY140G    education-related allowances
# HY050G    family/children related allowances
# HY060G    social exclusion not elsewhere classified
# HY070G    housing allowances
# HY080G    regular inter-household cash transfers received
# Steuern und Sozialversicherungsabgaben (5)
# HY120G    regular taxes on wealth
# HY130G    regular inter-household cash transfer paid
# HY140G    tax on income and social insurance contributions

###################
# Download data
###################

# Personal Data p

# pb010     year
# pb020     country
# pb030     personal ID
# px030     household ID
# pb040     personal cross-sectional weight
# pb140     year of birth
# pb150     sex
# py010g    employee cash or near cash income (gross)
# py050g    gross cash benefits or losses from self-employment 
#                 (including royalties)
# py080g    pensions received from individual private plans
# py090g    unemployment benefits
# py100g    old-age benefits
# py110g    survivor' benefits 
# py120g    sickness benefits 
# py130g    disability benefits
# py140g    education-related allowances

# p data until 2013
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% country & pb010 %in% year) %>%
  select(pb010, pb020, pb030, px030, pb040, pb140, pb150, 
         py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g) %>%
  collect(n = Inf)

# p data from 2014 onwards
silc.p14 <- tbl(pg, "c14p") %>%
  filter(pb020 == country) %>%
  select(pb010, pb020, pb030, px030, pb040, pb140, pb150, 
         py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g) %>%
  collect(n = Inf)

silc.p15 <- tbl(pg, "c15p") %>%
  filter(pb020 == country) %>%
  select(pb010, pb020, pb030, px030, pb040, pb140, pb150, 
         py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g) %>%
  collect(n = Inf)

silc.p16 <- tbl(pg, "c16p") %>%
  filter(pb020 == country) %>%
  select(pb010, pb020, pb030, px030, pb040, pb140, pb150, 
         py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g) %>%
  collect(n = Inf)

silc.p17 <- tbl(pg, "c17p") %>%
  filter(pb020 == country) %>%
  select(pb010, pb020, pb030, px030, pb040, pb140, pb150, 
         py010g, py050g, py080g, py090g, py100g, py110g, 
         py120g, py130g, py140g) %>%
  collect(n = Inf)

silc.p <- silc.p %>% 
  bind_rows(silc.p14, silc.p15, silc.p16, silc.p17)
rm(silc.p14, silc.p15, silc.p16, silc.p17)

summary(is.na(silc.p))

# Household Data h

# hb010     year
# hb020     country
# hb030     household ID
# hx050     Equivalised Household Size
# hy010     Total Household Gross Income
# hy020     Total disposable household income
# hy110g    income received by people aged under 16
# hy040g    income from rental of a property or land
# hy090g    interests, dividends, profit from capital 
#                 investments in unincorporated business
# hy050g    family/children related allowances
# hy060g    social exclusion not elsewhere classified
# hy070g    housing allowances
# hy080g    regular inter-household cash transfers received
# hy120g    regular taxes on wealth
# hy130g    regular inter-household cash transfer paid
# hy140g    tax on income and social insurance contributions

# h data until 2013
silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country & hb010 %in% year) %>%
  select(hb010, hb020, hb030, hx050, hy010, hy020, hy110g, 
         hy040g, hy090g, hy050g, hy060g, hy070g, 
         hy080g, hy120g, hy130g, hy140g) %>%
  collect(n = Inf)

# h data from 2014 onwards
silc.h14 <- tbl(pg, "c14h") %>%
  filter(hb020 == country) %>%
  select(hb010, hb020, hb030, hx050, hy010, hy020, hy110g, 
         hy040g, hy090g, hy050g, hy060g, hy070g, 
         hy080g, hy120g, hy130g, hy140g) %>%
  collect(n = Inf)

silc.h15 <- tbl(pg, "c15h") %>%
  filter(hb020 == country) %>%
  select(hb010, hb020, hb030, hx050, hy010, hy020, hy110g, 
         hy040g, hy090g, hy050g, hy060g, hy070g, 
         hy080g, hy120g, hy130g, hy140g) %>%
  collect(n = Inf)

silc.h16 <- tbl(pg, "c16h") %>%
  filter(hb020 == country) %>%
  select(hb010, hb020, hb030, hx050, hy010, hy020, hy110g, 
         hy040g, hy090g, hy050g, hy060g, hy070g, 
         hy080g, hy120g, hy130g, hy140g) %>%
  collect(n = Inf)

silc.h17 <- tbl(pg, "c17h") %>%
  filter(hb020 == country) %>%
  select(hb010, hb020, hb030, hx050, hy010, hy020, hy110g, 
         hy040g, hy090g, hy050g, hy060g, hy070g, 
         hy080g, hy120g, hy130g, hy140g) %>%
  collect(n = Inf)

silc.h <- silc.h %>% 
  bind_rows(silc.h14, silc.h15, silc.h16, silc.h17)
rm(silc.h14, silc.h15, silc.h16, silc.h17)

summary(is.na(silc.h))
# einige Beobachtungen haben NA bei Äquvalenzfaktor
# dieser kann nicht 0 gesetzt werden, deshalb fallen Beobachtungen raus
silc.h <- na.omit(silc.h, cols = c("hx050"))
silc.h$hy010[is.na(silc.h$hy010)] <- 0
silc.h$hy020[is.na(silc.h$hy020)] <- 0
silc.h$hy140g[is.na(silc.h$hy140g)] <- 0

# Household register d

# db010     year
# db020     country
# db030     household ID
# db040     region (Nuts 1 or 2)
# db090     household cross-sectional weight

# d data until 2013
silc.d <- tbl(pg, "dd") %>%
  filter(db020 %in% country & db010 %in% year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

# d data from 2014 onwards
silc.d14 <- tbl(pg, "c14d") %>%
  filter(db020 == country) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.d15 <- tbl(pg, "c15d") %>%
  filter(db020 == country) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.d16 <- tbl(pg, "c16d") %>%
  filter(db020 == country) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.d17 <- tbl(pg, "c17d") %>%
  filter(db020 == country) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.d <- silc.d %>% 
  bind_rows(silc.d14, silc.d15, silc.d16, silc.d17)
rm(silc.d14, silc.d15, silc.d16, silc.d17)

summary(is.na(silc.d))

# Personal register r

# rb010     year
# rb020     county
# rb030     personal ID
# rb050     personal cross-sectional weight
# rb080     year of birth
# rx030     household ID

# r data until 2013
silc.r <- tbl(pg, "rr") %>% 
  filter(rb020 %in% country & rb010 %in% year) %>%
  select(rb010, rb020, rb030, rb050, rb080, rx030) %>%
  collect(n = Inf)

# r data from 2014 onwards
silc.r14 <- tbl(pg, "c14r") %>% 
  filter(rb020 == country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rx030) %>%
  collect(n = Inf)

silc.r15 <- tbl(pg, "c15r") %>% 
  filter(rb020 == country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rx030) %>%
  collect(n = Inf)

silc.r16 <- tbl(pg, "c16r") %>% 
  filter(rb020 == country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rx030) %>%
  collect(n = Inf)

silc.r17 <- tbl(pg, "c17r") %>% 
  filter(rb020 == country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rx030) %>%
  collect(n = Inf)

silc.r <- silc.r %>% 
  bind_rows(silc.r14, silc.r15, silc.r16, silc.r17)
rm(silc.r14, silc.r15, silc.r16, silc.r17)


#################### Merging ###################

# Create IDs for merging personal data (household ID + year)
silc.r <- silc.r %>% mutate(id_h = paste0(rx030, rb010))
silc.p <- silc.p %>% mutate(id_h = paste0(px030, pb010))
# Create IDs for merging personal data (personal ID + year)
silc.p <- silc.p %>% mutate(id_p = paste0(pb030, pb010))
silc.r <- silc.r %>% mutate(id_p = paste0(rb030, rb010))

# joining p data to r data (damit Personen unter 16 erhalten bleiben)
silc.rp <- left_join(silc.r, silc.p, by = c("id_h" = "id_h", "id_p" = "id_p"))
# überflüssige (doppelte) Variablen weg
silc.rp <- subset(silc.rp, select=-c(pb010, pb020, pb030, px030, pb140))
silc.rp <- silc.rp %>% mutate(age = rb010 - rb080)
summary(is.na(silc.rp))
# sind NA, weil einige Beobachtungen nur im Register 
# (haben keine entsprechenden personal data) 
# (sondern nur angeführt als im Haushalt lebend)
# daher werden NAs 0 gesetzt
silc.rp[is.na(silc.rp)] <- 0


# Create IDs for merging household data (household ID + year)
silc.h <- silc.h %>% mutate(id_h = paste0(hb030, hb010))
silc.d <- silc.d %>% mutate(id_h = paste0(db030, db010))
# Merge household data h & d
silc.hd <- left_join(silc.h, silc.d, by = c("id_h" = "id_h"))

# Merge personal and household data (rp & hd)
silc.rphd <- left_join(silc.rp, silc.hd, by = c("id_h" = "id_h"))
silc.rphd <- subset(silc.rphd, select=-c(hb010, hb020, hb030, db010, db020, db030))

# dummy für über 20
silc.rphd <- silc.rphd %>% mutate(adult=ifelse(age >= 20, 1, 0))

summary(is.na(silc.rphd))
# 228 NAs bei hx050 produziert (müssen ausgeschlossen werden)
silc.rphd <- na.omit(silc.rphd, cols = c("hx050"))
