## ## Librerías requeridas
packs <- c('dplR','plyr','tmap','rasterVis',
           'RColorBrewer','hexbin','raster','rgdal','parallel',
           'R.utils', 'rvest','xml2','reshape2','zoo','nlme',
           'ggeffects','stringr','ggplot2','ecochange')
newpacks <- c('readxl','tidyverse','viridis','gstat', 'stargazer','xtable')
packs <- c(packs, newpacks)
sapply(packs, require, character.only = TRUE)
## dir()
## Cálculo del CO2F
setwd("F:/Proyecto_Esc/devel")
setwd('/home/wihe/Documents/tuh32536/co2Medellin/devel')
setwd('/home/wilar/Documents/tuh32536/co2Medellin/devel')
## dataC  <- read.csv('F14C_posterior1.csv')
## dataC  <- read.csv('F14C_posteriornew.csv', dec=".", sep = ";")
dataC  <- read.csv('F14C_posteriornew.csv', dec=".", sep = ",")
dataIg <- read.csv('CO2_Ignacio.csv')
co2 <- read.table("co2_annmean_mlo.txt")
names(co2) <- c('Año.cofechado', 'co2', 'V3')
cod  <- read.csv('Codigos_posterior.csv')
treeR <- read.tucson('treerings_posterior.rwl')
dataC[,'Año.cofechado'] <- round(dataC[,'Año.cofechado'],0)
splitC <- split(dataC, dataC$'Arbol')
subC <- lapply(splitC, function(x)
               x[,c('Año.cofechado','delta14C')])
## lapply(subC, function(x)head(x, 2))
fmatch <- function(tomatch.){
    suppressWarnings(Reduce(function(x,y){
        merge(x,y, by = 'Año.cofechado', all = TRUE)},tomatch.))}
merC <- fmatch(subC)
merC <- merC[order(merC$'Año.cofechado'),]
merC <- merC[!is.na(merC$'Año.cofechado'),]
names(merC) <- c('Año.cofechado', names(subC))
treeR$'Año.cofechado' <- rownames(treeR)
treeC <- merge(treeR, merC, by = 'Año.cofechado',all.x = TRUE)
rownames(treeC) <- treeC$'Año.cofechado'
treeCC <- merge(treeC, co2[,1:2], by = 'Año.cofechado')
#jfc <- read.csv('JFJ_SIL_C14_Tabla2.csv')
NH3 <- read.csv2('NHZ3_mean.csv', dec=",", sep = ";")
str(NH3)
NH3$'Year.AD' <- round(NH3$'Year.AD', 0)
NH3
#zona3mean<-summarise_at(group_by(NH3,Year.AD),vars(mean.Delta14C,sd.Delta14C),funs(mean(.,na.rm=TRUE)))

zona3mean <- NH3%>%group_by(Year.AD)%>%summarize(mean.Delta14C = mean(mean.Delta14C))%>%filter(Year.AD > 1969)

#names(jfc) <- c('year','14C','sd.14C')
names(zona3mean) <- c('year','14C','sd.14C')
# treeCCSuiza <- merge(treeCC, jfc,
#                      by.x = 'Año.cofechado', by.y = 'year',
#                      all.x = TRUE)
treeCCNH3 <- merge(treeCC, zona3mean,
                     by.x = 'Año.cofechado', by.y = 'year',
                     all.x = TRUE)
#zona2mean <- read.csv('NHZ2_mean.csv')
#zona2mean$'Year.AD' <- round(zona2mean$'Year.AD', 0)
# xlsnm <- 'S0033822221000953sup002-1.xls'
# sheet. <- excel_sheets(xlsnm)
# NHzone3 <- xlsnm %>% read_excel(sheet = sheet.[3])%>%
#     filter(complete.cases(.))%>%mutate_if(is.character, as.numeric)%>%
#     select(1:3)
# names(NHzone3) <- names(zona2mean)
# zona3mean <- NHzone3%>%group_by(Year.AD)%>%summarize(mean.Delta14C = mean(mean.Delta14C))%>%filter(Year.AD > 1969)
# NHzone3$'Year.AD' <- round(NHzone3$'Year.AD', 0)
# refcurve <- zona2mean
# refcurve <- zona3mean
## treeCCSuizaMean <- merge(treeCCSuiza, zona2mean,
##                          by.x = 'Año.cofechado', by.y = 'Year.AD', all.x = TRUE)
## str(treeCCSuizaMean)
# treeCCSuizaMean <- merge(treeCCSuiza, refcurve,
#                          by.x = 'Año.cofechado', by.y = 'Year.AD', all.x = TRUE)
# str(treeCCSuizaMean)
## Mean 14C from different places:
# treeCCSuizaMean$'14Caverage' <- apply(treeCCSuizaMean[,c('14C','mean.Delta14C')],
#                                       1, function(x)mean(x, na.rm = TRUE))
cFoss <- function(CO2,CBG,D14C){
    CO2*(CBG-D14C)/(D14C + 1E3)
}
nmtreeCC <- names(treeCC)
sub14C <- treeCC[,nmtreeCC[(ncol(treeR) + 1):(ncol(treeCC) - 1)]]
# CO2MED <- cFoss(treeCC$'co2',
#                 CBG = treeCCSuizaMean$'14Caverage',
#                 D14C = sub14C)

CO2MED <- cFoss(treeCCNH3$'co2',
                CBG = treeCCNH3$'14C',
                D14C = sub14C)
rownames(CO2MED) <- rownames(treeC)
CO2M <- cbind(time = as.numeric(rownames(CO2MED)), CO2MED)
## Validation of the CO2F using data from other studies
meanCO2M <- data.frame(tiempo = CO2M[,1L], CO2F= apply(CO2M[,-c(1,12)], 1, mean, na.rm = TRUE))
merigco <- merge(dataIg, meanCO2M, by = 'tiempo')
mer2000 <- subset(merigco, tiempo >= 2000)
## plot(CO2_Med~tiempo, data = mer2000, pch = 19, col = 'red')
## points(CO2_Med~tiempo, data = mer2000, pch = 19, col = 'black')
## plot(CO2F~tiempo, data = merigco, pch = 19)
## ## plot(CO2_Med~H_Med, data = merigco, pch = 19, col = 'red')
## points(CO2_Med~H_Med, data = mer2000, pch = 19, col = 'black')
## mer2000 <- subset(merigco, tiempo >= 2000)
plot(CO2F~CO2_Med, data = mer2000, pch = 19,
     xlab = expression(paste("CO"[2]," [Mg]")),
     ylab = expression(paste("[CO2F]","")),
     ylim = c(0,13),
     cex.axis = 1.3,
     cex.lab = 1.1
     ## axes = F
          )
grid(nx = NULL, ny = NULL, col = "grey40", lty = "dotted")
model <- lm(CO2F~CO2_Med, data = mer2000)
Pvalue = pf(summary(model)$fstatistic[1],
            summary(model)$fstatistic[2],
            summary(model)$fstatistic[3],
            lower.tail = FALSE)
abline(model)
text(mer2000$CO2_Med,mer2000$CO2F, labels = mer2000$tiempo, pos = 3, col = 'grey40', adj = 1)
Pvalue = pf(summary(model)$fstatistic[1],
            summary(model)$fstatistic[2],
            summary(model)$fstatistic[3],
            lower.tail = FALSE)
R2     = summary(model)$r.squared
t1     = paste0("Probabilidad: ", signif(Pvalue, digits=3))
t2     = paste0("R-cuadrado: ",signif(R2, digits=3))
t3     = paste0("Intercepto: ", signif(coef(model)[1], digits=3))
t4     = paste0("Pendiente: ", signif(coef(model)[2], digits=3))
text(2100000, 12, labels = t1, pos=4)
text(2100000, 11.5, labels = t2, pos=4)
text(2100000, 11, labels = t3, pos=4)
text(2100000, 10.5, labels = t4, pos=4)
## otras comparaciones
## plot(CO2_Med~tiempo, data = mer2000, pch = 19, col = 'red')
## points(CO2_Med~tiempo, data = mer2000, pch = 19, col = 'black')
## plot(CO2F~tiempo, data = merigco, pch = 19)
## ## plot(CO2_Med~H_Med, data = merigco, pch = 19, col = 'red')
## points(CO2_Med~H_Med, data = mer2000, pch = 19, col = 'black')
## mer2000 <- subset(merigco, tiempo >= 2000)
## MIXED-EFFECTS MODEL (modelo de interpolación temporal) 
df  <- melt(CO2M, id.vars = 'time', variable.name = 'sample')
## ## head(df)
df[,'sample'] <- gsub("[a-zA-Z ]", "",df[,'sample'])
df[,'sample'] <- as.factor(df[,'sample'])
## Extraction of trees with issues
subt <- factor(c('37','40','28'))
df1 <- df[!df$'sample'%in%subt,]
## Excluding the sample nr 37 does not improves the model
## df1 <- df[!df$'sample'%in%factor('37'),]
## df1 <- df[!df$'sample'%in%factor('40'),]
## df1 <- df[!df$'sample'%in%factor('28'),]
gd <- na.omit(groupedData(value ~ poly(time,2)|sample, data = df1))
crd <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
## coor <- read.csv('F:/Proyecto_Esc/devel/sp_coord.csv')
coor <- read.csv('~/Documents/tuh32536/co2Medellin/devel/sp_coord.csv')

spdf <- SpatialPointsDataFrame(coords = coor[,5:4], data = coor,
                               proj4string = crs(crd))
gd1 <- as_tibble(spdf)%>%select(Longitud, Codigo)%>%
    mutate(sample = gsub("[a-zA-Z ]", "",Codigo))%>%
    mutate(zone = cut(Longitud, 3))%>%select(sample, zone)%>%
    left_join(gd)%>%drop_na()%>%as.data.frame()
gd <- groupedData(value ~ poly(time,2)|sample, data = gd1)
## head(gd)
## gd <- na.omit(groupedData(value ~ poly(time,2)|sample, data = df))
M1 = lme(gd, method="ML")
summary(M1)
coefs <- coef(M1)
plot(augPred(M1, primary = ~time))
gd2 <- update(gd, formula = value ~ poly(time,2)|zone/sample)
M2 = lme(gd2, method="ML")
coefs2 <- coef(M2)
summary(M2)
M0 <- lm(value ~ poly(time,2), gd)
summary(M0)
## require('stargazer')
stargazer(M1,  type = 'latex',
          title = 'Fixed-effects estimators obtained using NH zone 3 data',
          font.size = 'scriptsize',covariate.labels = c('t','t2','intercept'))
## anova3 <- anova(M2,M1,M0)
anova(M1,M0)
##    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
## M1     1 10 1552.447 1588.092 -766.2232                        
## M0     2  4 1825.569 1839.828 -908.7847 1 vs 2 285.1229  <.0001
## It seems getData from nlme is having issues: 
## dt <- getData(M1)
## M0 <- lm(value ~ poly(time,2), dt)
## ctrl <- lmeControl(opt='optim')
## M2 <- update(M1, correlation = corARMA(p = 1, q = 1), control=ctrl)
## M2 <- update(M1, correlation = corARMA(p = 1, q = 1), )
## plot(augPred(M2, primary = ~time))
summary(M2)
an2 <- anova(M2,M1,M0)

an22 <- as.data.frame(an2)[-1L]
an22[,sapply(an22, is.numeric)] <- lapply(an22[, sapply(an22,is.numeric)], function(x)round(x,3))
rownames(an22) <- NULL
an22[1,1] <- 'fe, zone/tree'
an22[2,1] <- 'fe, tree'
an22[3,1] <- 'fe'

xtable(an22)


## after better cross-dating:
## M2     1 12 1553.456 1596.230 -764.7279                         
## M1     2 10 1552.447 1588.092 -766.2232 1 vs 2   2.99066  0.2242
## M0     3  4 1825.569 1839.828 -908.7847 2 vs 3 285.12290  <.0001
## before better cross-dating:
##    Model df      AIC      BIC    logLik   Test   L.Ratio p-value
## M2     1 12 1590.347 1633.028 -783.1733                         
## M1     2 10 1592.850 1628.418 -786.4247 1 vs 2   6.50287  0.0387
## M0     3  4 1813.751 1827.978 -902.8754 2 vs 3 232.90138  <.0001
## Extract prediction band from lme fit
## intr <- intervals(M2)
intr <- intervals(M1)
require('ggeffects')
alpha. <- 0.05
ggpredict(M1, "time") %>% plot(rawdata = T, dot.alpha = alpha.)
ggpredict(M1, "time", type = "re") %>% plot(rawdata = T, dot.alpha = alpha.)
## Cálculo de la retícula de AMVA
nts <- 1980:2020
nts <- 1971:2018
## nts <- seq(2000,2020,5)
smp <- levels(df$'sample')
ntsf <- rep(nts, length(smp))
smpf <- rep(smp, each = length(nts))
nwdt <- data.frame(time = ntsf, sample = smpf)
preds <- data.frame(time = nwdt$'time',
                    predict(M1, nwdt, level = 0:1))
## crd <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
## coor <- read.csv('~/Documents/tuh32536/co2Medellin/devel/sp_coord.csv')
## spdf <- SpatialPointsDataFrame(coords = coor[,5:4], data = coor,
##                                proj4string = crs(crd))
elmed1 <- raster('elmed1.tif')
## plot(elmed1)
roadMed <- readOGR('.', 'viasMed')
## deta <- readOGR('~/Documents/tuh32536/co2Medellin/devel/Malla_vial/', 'Malla_vial')
## rmed <- crop(roadMed, extent(newarea))
## plot(rmed)
seq. <- c(seq(1980,2015,5), 2018)
seq. <- nts
preds <- na.omit(preds)
head(preds)
preds_ls <- split(preds, preds$'sample')
year_preds_df <- preds[preds$'time'%in%seq.,] 
## year_preds_ls <- lapply(preds_ls, function(x)
##                         subset(x, time%in%seq.))
## year_preds_df <- do.call('rbind', year_preds_ls)
## names(year_preds_df) <- NULL 
yrini. <- year_preds_df
## yrini<- subset(preds, time == as.factor(as.character(x)))
yrini.[,'sample'] <- paste0('FU',yrini.[,'sample'])
yrini <- yrini.[,!names(yrini.)%in%c('predict.fixed')]
head(yrini)
spdf. <- as.data.frame(spdf)
mergedf. <- merge(yrini, spdf.[,!names(spdf.)%in%c('CAP','Fecha')], by.y = 'Codigo', by.x = 'sample')
mergedf <- mergedf.[,!names(mergedf.)%in%c('Longitud.1','Latitud.1')] 
## elmed1 <- raster('elmed1.tif')
## roadMed <- readOGR('.', 'viasMed')
mrd <- subset(mergedf, !is.na(mergedf$'predict.sample'))
## newarea <- c(-75.61, -75.554, 6.2, 6.29)
## year <- 1985
## time_df <- subset(mrd, time == year)
## Esta función itera las interpolaciones
yrints <- function(x, seed = 123){
yrini<- subset(preds, time == as.factor(as.character(x)))
yrini[,'sample'] <- paste0('FU',yrini[,'sample'])
yrini <- yrini[,-c(1,3)]
mergedf <- merge(spdf, yrini, by.x = 'Codigo', by.y = 'sample')
## elmed1 <- raster('elmed1.tif')
## roadMed <- readOGR('.', 'viasMed')
mrd <- subset(mergedf, !is.na(mergedf$'predict.sample'))
newarea <- c(-75.61, -75.554, 6.18, 6.29)
newmrd <- mrd[!mrd$'Codigo'%in%c('FU37','FU40'),] 
newr <- crop(elmed1, extent(newarea))
rmed <- crop(roadMed, extent(newarea))
grd <- as.data.frame(rasterToPoints(newr, spatial =T))[,-1L]
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(newmrd)
set.seed(seed)
P.idw <- gstat::idw(predict.sample ~ 1, newmrd, newdata=grd, idp=2.0)
r <- raster(P.idw)}
## Modelos de interpolación IDW
## seq. <- c(1990:2018)
seq. <- c(1980:2018)
## seq. <- c(seq(1985,2015,5), 2018)
inters. <- Map(function(x)
              yrints(x), seq.)
## names(inters.) <- paste0('año ',seq.)
names(inters.) <- seq.
inters <- stack(inters.) 
seq.. <- paste0('X',c(seq(1980, 2015, 5), 2018))
inters5 <- inters[[seq..]]
coor. <- coor[-c(33:37),]
coor.. <- coor.[,4:5]
cr <- data.frame(coordinates(coor..))
coordinates(cr) <- ~Longitud+Latitud
extent(cr)
p1 <- plotebv(inters5) 
## p1 + latticeExtra::layer(sp.points(cr, pch = 19, col = 'red', cex = 0.4), columns = 1, rows = 1)
p1 + latticeExtra::layer(sp.points(cr, pch = 19, col = 'red', cex = 0.4))

inters2sd <- inters
sdfrom <- calc(inters2sd, fun = sd)
p1 <- plotebv(sdfrom, col.regions = rev(viridis_pal(option = 'C')(255))) 
p2 <- p1 + latticeExtra::layer(sp.points(cr, pch = 19, col = 'white', cex = 0.4), columns = 1, rows = 1)
p2

fileName <- 'CO2_1980_2018_sd_NH3_calibrated'
## printPath <- file.path('/home',Sys.info()['user'],'Documents/tuh32536/co2Medellin/devel')
printPath <- file.path('/home',Sys.info()['user'],'Documents/tuh32536/co2Medellin/beamerResults/fig')
printPlot <- function(fileName, printPath = NULL, ext = 'pdf'){
    if(is.null(printPath))
        printPath <- file.path('/home',Sys.info()['user'],'Desktop')
    path.. <- paste(printPath, paste(fileName, ext, sep = '.'), sep = '/')
dev.copy(pdf, path..)
## dev.copy(png, path.., bg = 'white')
dev.off()
}
printPlot(fileName, printPath)

## plotebv(inters)
## extractedObs <- na.omit(as.data.frame(raster::extract(inters, spdf, sp = TRUE)))
## nms <- names(extractedObs)[-c(2,3,15,16)]
## extractedObs <- extractedObs[, nms] 
## CO2estimates <- write.csv(extractedObs, file = "CO2estimates.csv")
## grid <- getValues(inters)
## write.csv(grid, file = "gridCO2.csv")

## names(inters)

## as.mat <- as.matrix(inters[[1]])

## mats <- Map(function(x)
##     as.matrix()

## mats <- Map(function(x)
##     write.csv(as.matrix(x),
##               file = paste(names(x), 'csv', sep = '.')),
##     as.list(inters))









ext <- 'pdf'
## fileName <- 'ICE_curves_six_more_important_tuned'
fileName <- 'CO2_1980_2018_sd'
printPath <- file.path('/home',Sys.info()['user'],'Documents/tuh32536/co2Medellin/devel')
path.. <- file.path(printPath, paste(fileName, ext, sep = '.'))
ggsave(path.., p2)


plot(sdfrom)


p1 <- plotebv(inters) 
## p1 + latticeExtra::layer(sp.points(cr, pch = 19, col = 'red', cex = 0.4), columns = 1, rows = 1)
p1 + latticeExtra::layer(sp.points(cr, pch = 19, col = 'red', cex = 0.4), columns = 4, rows = 2)

getwd()
## path.. <- paste(printPath, paste(fileName, ext, sep = '.'), sep = '/')
dev.copy(pdf, 'location_interpolations_02_NH3.pdf')
dev.off()
getwd()


## require('ggmap')
## register_google(key = "AIzaSyAR-fOxi4l8-OtNqxVVEdjSuuPyuFM0l2g")
## tmp <- qmplot(Longitud, Latitud, data = coor., maptype = "toner-lite", color = I("red"))

