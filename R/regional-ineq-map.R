rm(list=ls())
library(rgdal)
library(xlsx)
library(openxlsx)
library(readxl)
library(XML)
library(dplyr)
library(tibble)
library(GISTools)

# working directory muss in ineq_project sein
# temp <- tempfile(fileext = ".zip")
# download.file("http://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2013-03m.shp.zip", temp)
# outDir<-"./data"
# unzip(temp, exdir=outDir)
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

gdp <- column_to_rownames(gdp, var = "NUTS_ID")
pop <- column_to_rownames(pop, var = "NUTS_ID")
gdp <- gdp * 1000000
pop <- pop * 1000
pc_gdp <- gdp / pop

colnames(pc_gdp) <- paste("pcgdp", colnames(pc_gdp), sep = "_")
pc_gdp <- rownames_to_column(pc_gdp, var = "NUTS_ID")
pc_gdp$av_gdp_0815 <- rowMeans(pc_gdp[10:17], na.rm = FALSE, dims = 1)
pc_gdp$av_gdp <- rowMeans(pc_gdp[2:17], na.rm = FALSE, dims = 1)

# Erstelle Karte
shp <- merge(shp, pc_gdp, all.x = TRUE, all.y = TRUE, by.x = "NUTS_ID", by.y = "NUTS_ID") 

shades <- auto.shading(shp$av_gdp_0815)
choropleth(shp, shp$av_gdp_0815, shades, main = "pro Kopf BIP (Durchschnitt 2008-2015)")
choro.legend(100000, 4704615, sh=shades, cex=0.6)


# Moran's I für alle verfügbaren Jahre (2000 - 2015)

# weights matrix
# Moran's I

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

mI <- bind_rows(Moran.I(x = shp$pcgdp_2008, weight = W.queen),
                Moran.I(x = shp$pcgdp_2009, weight = W.queen),
                Moran.I(x = shp$pcgdp_2010, weight = W.queen),
                Moran.I(x = shp$pcgdp_2011, weight = W.queen),
                Moran.I(x = shp$pcgdp_2012, weight = W.queen),
                Moran.I(x = shp$pcgdp_2013, weight = W.queen),
                Moran.I(x = shp$pcgdp_2014, weight = W.queen),
                Moran.I(x = shp$pcgdp_2015, weight = W.queen))
m.years <- c(2008:2015)
mI <- as.data.frame(cbind(m.years, mI[,1], mI[,4]))
mI <- rename(mI, c("m.years"="year", "observed"="Moran's I", "p.value" = "p value"))

write.csv(mI, file = "./reports/ESP/tables/moran.csv")
