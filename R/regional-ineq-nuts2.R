# regional inequality 2015

dat15 <- silc.rphd %>% filter(rb010 == 2015)



nuts2 <- aggregate(dat15$disp.inc.1, by=list(dat15$db040), FUN=mean)
nuts2 <- rename(nuts2, c("x" = "disp.inc", "Group.1" = "Region"))

dat15.svy <- svydesign(ids =  ~ rb030,
                       strata = ~db040,
                       weights = ~pb040,
                       data = dat15) %>% convey_prep()

gini.nuts <- svyby(~disp.inc.h, ~db040, dat15.svy, svygini, keep.var = FALSE)
gini.nuts <- rename(gini.nuts, c("db040" = "Region", "statistic" = "gini"))
nuts2 <- left_join(nuts2, gini.nuts, by = c("Region" = "Region"))


shp <- readOGR(dsn = "./data", layer ="NUTS_RG_03M_2013_4326_LEVL_2", encoding = "UTF-8")
shp <- spTransform(shp, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
plot(shp)
spain <- shp[grepl("ES", shp[["NUTS_ID"]]), ]
spain <- as.character(spain$NUTS_ID)
islands <- c("ES70", "ES53")
spain <- setdiff(spain, islands)
shp <- shp[ shp$NUTS_ID %in% spain, ]
plot(shp)

shp <- merge(shp, nuts2, all.x = TRUE, all.y = FALSE, by.x = "NUTS_ID", by.y = "Region") 
library(GISTools)
shades <- auto.shading(shp$disp.inc)
choropleth(shp, shp$disp.inc, shades, main = "regionales verfügbares Einkommen (2015)")
choro.legend(100000, 4704615, sh=shades, cex=0.6)

shades <- auto.shading(shp$gini)
choropleth(shp, shp$gini, shades, main = "regionaler Gini (2015)")
choro.legend(100000, 4704615, sh=shades, cex=0.6)
