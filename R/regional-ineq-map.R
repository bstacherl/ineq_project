rm(list=ls())

# working directory muss in ineq_project sein
# temp <- tempfile(fileext = ".zip")
# download.file("http://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2013-03m.shp.zip", temp)
# outDir<-"./data"
# unzip(temp, exdir=outDir)
library(rgdal)
# zipF <- list.files(path = "./data", pattern=".zip", full.names = T)
# sapply(zipF, unzip, exdir = outDir)
# list.files(path="./data", pattern = 'NUTS_RG_03M_2013.*.shp$')
shp <- readOGR(dsn = "./data", layer ="NUTS_RG_03M_2013_4326_LEVL_3", encoding = "UTF-8")
shp <- spTransform(shp, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
plot(shp)
spain <- shp[grepl("ES", shp[["NUTS_ID"]]), ]
spain <- as.character(spain$NUTS_ID)
islands <- c("ES703", "ES704", "ES705", "ES706", "ES707", "ES708", "ES709", "ES531", "ES532", "ES533", "ES630", "ES640")
spain <- setdiff(spain, islands)
shp <- shp[ shp$NUTS_ID %in% spain, ]
plot(shp)

library(xlsx)
library(openxlsx)
library(readxl)
library(XML)

# download.file("http://appsso.eurostat.ec.europa.eu/nui/download?p=4c4dc5f0-0971-4b24-b5fa-cd91af619cca-1546102236507_&_=1546103337998", destfile = "./data/gdp.xls")
# does not work to open in R directly
# downloaded xls file manually into data 

gdp <- read_xls("./data/gdp.xls", col_names = TRUE, skip = 8)
gdp <- gdp[grepl("ES", gdp[["GEO/TIME"]]), ]
gdp <- gdp[, -which(names(gdp) == "2016")]
names(gdp)[1]<-paste("NUTS_ID")
gdp <- gdp[ gdp$NUTS_ID %in% spain, ]
# gdp for NUTS 3 regions (in million €)

pop <- read_xls("./data/population.xls", col_names = TRUE, skip = 8)
names(pop)[1]<-paste("NUTS_ID")
pop <- pop[ pop$NUTS_ID %in% spain, ]

pop$`2000` <- as.numeric(pop$`2000`)
pop$`2001` <- as.numeric(pop$`2001`)
pop$`2002` <- as.numeric(pop$`2002`)
pop$`2003` <- as.numeric(pop$`2003`)
pop$`2004` <- as.numeric(pop$`2004`)
pop$`2005` <- as.numeric(pop$`2005`)
pop$`2006` <- as.numeric(pop$`2006`)
pop$`2007` <- as.numeric(pop$`2007`)
gdp$`2000` <- as.numeric(gdp$`2000`)

library(tibble)
gdp <- column_to_rownames(gdp, var = "NUTS_ID")
pop <- column_to_rownames(pop, var = "NUTS_ID")
gdp <- gdp * 1000000
pop <- pop * 1000
pc_gdp <- gdp / pop

colnames(pc_gdp) <- paste("pcgdp", colnames(pc_gdp), sep = "_")
pc_gdp <- rownames_to_column(pc_gdp, var = "NUTS_ID")
pc_gdp$av_gdp_0008 <- rowMeans(pc_gdp[2:10], na.rm = FALSE, dims = 1)
pc_gdp$av_gdp_0915 <- rowMeans(pc_gdp[11:17], na.rm = FALSE, dims = 1)
pc_gdp$av_gdp <- rowMeans(pc_gdp[2:17], na.rm = FALSE, dims = 1)

# Erstelle Karte
shp <- merge(shp, pc_gdp, all.x = TRUE, all.y = TRUE, by.x = "NUTS_ID", by.y = "NUTS_ID") 
library(GISTools)
shades <- auto.shading(shp$av_gdp_0008)
choropleth(shp, shp$av_gdp_0008, shades)

shades <- auto.shading(shp$av_gdp_0915)
choropleth(shp, shp$av_gdp_0915, shades)

shades <- auto.shading(shp$av_gdp)
choropleth(shp, shp$av_gdp, shades)


# Moran's I für alle verfügbaren Jahre (2000 - 2015)

# weights matrix
# Moran's I
# Moran Scatterplot

coords <-coordinates(shp)
library(spdep)
# Queen
queen_nb <- poly2nb(shp, row.names = shp$NUTS_ID, queen = T)
W.list <- nb2listw(queen_nb, style = "W", zero.policy = TRUE)
W.queen <- listw2mat(W.list)
plot(shp)
# Nachbarschaft
plot(queen_nb, coords, add=TRUE, col="green", cex=0.5)

library(ape)
m.00 <- Moran.I(x = shp$pcgdp_2000, weight = W.queen)
m.00 <- as.data.frame(m.00)
m.00$year <- 2000
m.01 <- Moran.I(x = shp$pcgdp_2001, weight = W.queen)
m.01 <- as.data.frame(m.01)
m.01$year <- 2001
m.02 <- Moran.I(x = shp$pcgdp_2002, weight = W.queen)
m.02 <- as.data.frame(m.02)
m.02$year <- 2002
m.03 <- Moran.I(x = shp$pcgdp_2003, weight = W.queen)
m.03 <- as.data.frame(m.03)
m.03$year <- 2003
m.04 <- Moran.I(x = shp$pcgdp_2004, weight = W.queen)
m.04 <- as.data.frame(m.04)
m.04$year <- 2004
m.05 <- Moran.I(x = shp$pcgdp_2005, weight = W.queen)
m.05 <- as.data.frame(m.05)
m.05$year <- 2005
m.06 <- Moran.I(x = shp$pcgdp_2006, weight = W.queen)
m.06 <- as.data.frame(m.06)
m.06$year <- 2006
m.07 <- Moran.I(x = shp$pcgdp_2007, weight = W.queen)
m.07 <- as.data.frame(m.07)
m.07$year <- 2007
m.08 <- Moran.I(x = shp$pcgdp_2008, weight = W.queen)
m.08 <- as.data.frame(m.08)
m.08$year <- 2008
m.09 <- Moran.I(x = shp$pcgdp_2009, weight = W.queen)
m.09 <- as.data.frame(m.09)
m.09$year <- 2009
m.10 <- Moran.I(x = shp$pcgdp_2010, weight = W.queen)
m.10 <- as.data.frame(m.10)
m.10$year <- 2010
m.11 <- Moran.I(x = shp$pcgdp_2011, weight = W.queen)
m.11 <- as.data.frame(m.11)
m.11$year <- 2011
m.12 <- Moran.I(x = shp$pcgdp_2012, weight = W.queen)
m.12 <- as.data.frame(m.12)
m.12$year <- 2012
m.13 <- Moran.I(x = shp$pcgdp_2013, weight = W.queen)
m.13 <- as.data.frame(m.13)
m.13$year <- 2013
m.14 <- Moran.I(x = shp$pcgdp_2014, weight = W.queen)
m.14 <- as.data.frame(m.14)
m.14$year <- 2014
m.15 <- Moran.I(x = shp$pcgdp_2015, weight = W.queen)
m.15 <- as.data.frame(m.15)
m.15$year <- 2015
library(dplyr)
m.I <- bind_rows(m.00, m.01, m.02, m.03, m.04, m.05, m.06, m.07, m.08, m.09, m.10, m.11, m.12, m.13, m.14, m.15)
rm(m.00, m.01, m.02, m.03, m.04, m.05, m.06, m.07, m.08, m.09, m.10, m.11, m.12, m.13, m.14, m.15)

moran.test(shp$pcgdp_2000, listw = W.list, alternative = "greater")
moran.mc(shp$pcgdp_2000, listw = W.list, alternative = "greater", nsim = 100)

# Moran Scatterplot for average of GDP 2000 - 2015
moran.plot(shp$av_gdp, listw = W.list)


