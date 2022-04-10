# Code for the manuscript entitled Reconstructing past fossil-fuel CO2 concentrations 
# using tree rings and radiocarbon in the urban area of Medellín, Colombia, 
# by M. Vasquez, W. Lara, J.I. del Valle, and C.A. Sierra.


## Load packages 
packs <- c('plyr','tmap','rasterVis','hexbin','raster','rgdal', 'rvest','reshape2','zoo','nlme',
           'stringr','ggplot2','ecochange', 'dplyr' , 'ggrepel', 'ggpmisc', 'tidyverse','gstat')
sapply(packs, require, character.only = TRUE)


# Directory 
getwd()


#Load datasets
dataC  <- read.csv2('F14C_posteriornew.csv', dec=".", sep = ";")
dataIg <- read.csv('CO2_Ignacio.csv')
co2 <- read.table("co2_annmean_mlo.txt")
names(co2) <- c('Año.cofechado', 'co2', 'V3')
cod  <- read.csv('Codigos_posterior.csv')
treeR <- read.tucson('treerings_posterior.rwl')
NH3 <- read.csv2('NHZ3_mean.csv', dec=",", sep = ";")
coor <- read.csv('sp_coord.csv')


dataC[,'Año.cofechado'] <- round(dataC[,'Año.cofechado'],0)
splitC <- split(dataC, dataC$'Arbol')
subC <- lapply(splitC, function(x)
               x[,c('Año.cofechado','delta14C')])
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

# Radiocarbon from the Northern Hemisphere Zone 3 curve 
str(NH3)
NH3$'Year.AD' <- round(NH3$'Year.AD', 0)
NH3
zona3mean <-NH3%>%group_by(Year.AD)%>%summarize(mean.Delta14C = mean(mean.Delta14C),sd.Delta14C = sd(sd.Delta14C) )%>%filter(Year.AD > 1969)



# Plot of radiocarbon concentration in the background air and in the measured tree rings expressed as delta14C (‰) over time (Figure 1)
splitC1 <- split(dataC, dataC$'Año.cofechado')
names(dataC)
subC1 <- lapply(splitC1, function(x)
  x[,c('Año.cofechado','delta14C','err...')])
dt <- do.call(rbind.data.frame, subC1)
names(dt)
names(dt) <- c(names(zona3mean)[1L], names(dt)[-1L])
merg <- zona3mean%>%left_join(dt)

plots <-
  ggplot(merg, aes(x=Year.AD, y=delta14C)) + 
  geom_point(color = 'red') +
  geom_line(aes(x=Year.AD, y=mean.Delta14C)) +
  ylab(expression(Delta^14~C))+ xlab("Time") +
  scale_color_manual()+
  theme(aspect.ratio = 3/2,
    text = element_text(size = 25),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
    legend.position = 'bottom') 

getwd()
ggsave('Figure1.pdf',plots)
dev.off()
getwd()



# Calculation of the CO2 concentration of fossil fuel origin [CO2F]
names(zona3mean) <- c('year','14C','sd.14C')
treeCCNH3 <- merge(treeCC, zona3mean,
                     by.x = 'Año.cofechado', by.y = 'year',
                     all.x = TRUE)

cFoss <- function(CO2,CBG,D14C){
    CO2*(CBG-D14C)/(D14C + 1E3)
}
nmtreeCC <- names(treeCC)
sub14C <- treeCC[,nmtreeCC[(ncol(treeR) + 1):(ncol(treeCC) - 1)]]

CO2MED <- cFoss(treeCCNH3$'co2',
                CBG = treeCCNH3$'14C',
                D14C = sub14C)
rownames(CO2MED) <- rownames(treeC)
CO2M <- cbind(time = as.numeric(rownames(CO2MED)), CO2MED)

CO2M2 <- data.frame(Year = CO2M[,1L], CO2F.mean= apply(CO2M[,-c(1,12)], 1, mean, na.rm = TRUE), CO2F.sd= apply(CO2M[,-c(1,12)], 1, sd, na.rm = TRUE))


# Plot of the concentration of fossil fuel CO2 [CO2F] values over time
ggplot(CO2M2, aes(x=Year, y=CO2F.mean)) + geom_bar(stat="identity")+ theme_minimal()+ theme(axis.text.x = element_text(angle = 90))  + ylab("[CO2F] (ppm)")+ xlab("Time") +
  geom_errorbar(aes(ymin=CO2F.mean-CO2F.sd, ymax=CO2F.mean+CO2F.sd), width=.5,position=position_dodge(.9),col= "#CD5C5C") +
  scale_x_continuous(breaks = seq(1980, 2017, by = 3))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

dev.copy(pdf, 'Figure2.pdf')
dev.off()
getwd()


# Comparison with emission data
meanCO2M <- data.frame(tiempo = CO2M[,1L], CO2F= apply(CO2M[,-c(1,12)], 1, mean, na.rm = TRUE))
CO2_MedTg <- dataIg$CO2_Med/1e6 
dataIg1<-cbind(dataIg,CO2_MedTg)
merigco <- merge(dataIg1, meanCO2M, by = 'tiempo')
mer2000 <- subset(merigco, tiempo >= 2000)

model <- lm(CO2F~CO2_MedTg, data = mer2000)
Pvalue = pf(summary(model)$fstatistic[1],
            summary(model)$fstatistic[2],
            summary(model)$fstatistic[3],
            lower.tail = FALSE)
R2     = summary(model)$r.squared


tb.pm <- tibble(Parameter = c("Probability", "R-squared", "Intercept", "Slope"),
                Value = c(signif(Pvalue, digits = 3), (signif(R2,digits=4)), (signif(coef(model)[1],digits = 3)), (signif(coef(model)[2], digits=3))))
data.tb <- tibble(x = 2, y = 12, tb = list(tb.pm))


# Plot of comparison between average values of the concentrations of fossil fuel calculated in this study and official CO2 emission data 
ggplot(mer2000, aes(CO2_MedTg, CO2F, label=tiempo)) + geom_point()+  
  geom_text_repel(aes(label=tiempo), 
                  size=3)+
  geom_abline(intercept = model$coefficients[1], slope = model$coefficients[2], lty = 2, color = "darkgrey")+ 
  geom_table(data = data.tb, aes(x, y, label = tb)) +
  theme_classic() +
  labs(x= expression(paste("CO"[2]," (Tg)")),
       y = expression(paste("[CO2F]"," (ppm)")))

dev.copy(pdf, 'Figure3.pdf')
dev.off()
getwd()


# Time-series analysis
#Mixed-effects model 
df  <- melt(CO2M, id.vars = 'time', variable.name = 'sample')
## ## head(df)
df[,'sample'] <- gsub("[a-zA-Z ]", "",df[,'sample'])
df[,'sample'] <- as.factor(df[,'sample'])
## Extraction of trees with issues
subt <- factor(c('37'))
df1 <- df[!df$'sample'%in%subt,]
gd <- na.omit(groupedData(value ~ poly(time,2)|sample, data = df1))
crd <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

spdf <- SpatialPointsDataFrame(coords = coor[,5:4], data = coor,
                               proj4string = crs(crd))
gd1 <- as_tibble(spdf)%>%select(Longitud, Codigo)%>%
    mutate(sample = gsub("[a-zA-Z ]", "",Codigo))%>%
    mutate(zone = cut(Longitud, 3))%>%select(sample, zone)%>%
    left_join(gd)%>%drop_na()%>%as.data.frame()
gd <- groupedData(value ~ poly(time,2)|sample, data = gd1)


# First model
M1 = lme(gd, method="ML")
summary(M1)
coefs <- coef(M1)

# Plot of Mixed-effects model of the concentrations of fossil fuel CO2 [CO2F] over time 
plot(augPred(M1, primary = ~time), ylab= expression(paste("[CO2F] (ppm)")), xlab = "Time", pch=19, col="black",cex=0.5, lwd=1.5)

dev.copy(pdf, 'Figure4.pdf')
dev.off()
getwd()


# Another models
gd2 <- update(gd, formula = value ~ poly(time,2)|zone/sample)
M2 = lme(gd2, method="ML")
coefs2 <- coef(M2)
summary(M2)
M0 <- lm(value ~ poly(time,2), gd)
summary(M0)


# Comparison between models
anova(M1,M0)


# Spatial analysis

nts <- 1980:2020
nts <- 1971:2018
smp <- levels(df$'sample')
ntsf <- rep(nts, length(smp))
smpf <- rep(smp, each = length(nts))
nwdt <- data.frame(time = ntsf, sample = smpf)
preds <- data.frame(time = nwdt$'time',
                    predict(M1, nwdt, level = 0:1))

elmed1 <- raster('elmed1.tif')
roadMed <- readOGR('.', 'viasMed')

seq. <- c(seq(1980,2015,5), 2018)
seq. <- nts
preds <- na.omit(preds)
head(preds)
preds_ls <- split(preds, preds$'sample')
year_preds_df <- preds[preds$'time'%in%seq.,] 
yrini. <- year_preds_df
yrini.[,'sample'] <- paste0('FU',yrini.[,'sample'])
yrini <- yrini.[,!names(yrini.)%in%c('predict.fixed')]
head(yrini)
spdf. <- as.data.frame(spdf)
mergedf. <- merge(yrini, spdf.[,!names(spdf.)%in%c('CAP','Fecha')], by.y = 'Codigo', by.x = 'sample')
mergedf <- mergedf.[,!names(mergedf.)%in%c('Longitud.1','Latitud.1')] 
mrd <- subset(mergedf, !is.na(mergedf$'predict.sample'))



# This function iterates the interpolations
yrints <- function(x, seed = 123){
yrini<- subset(preds, time == as.factor(as.character(x)))
yrini[,'sample'] <- paste0('FU',yrini[,'sample'])
yrini <- yrini[,-c(1,3)]
mergedf <- merge(spdf, yrini, by.x = 'Codigo', by.y = 'sample')
mrd <- subset(mergedf, !is.na(mergedf$'predict.sample'))
newarea <- c(-75.61, -75.554, 6.18, 6.29)
newmrd <- mrd[!mrd$'Codigo'%in%c('FU37'),] 
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


## IDW interpolation models
seq. <- c(1980:2018)
inters. <- Map(function(x)
              yrints(x), seq.)
names(inters.) <- seq.
inters <- stack(inters.) 
seq.. <- paste0('X',c(seq(1980, 2015, 5), 2018))
inters5 <- inters[[seq..]]
coor. <- coor[-c(33:37),]
coor.. <- coor.[,4:5]
cr <- data.frame(coordinates(coor..))
coordinates(cr) <- ~Longitud+Latitud
extent(cr)

# Plot of Spatial interpolation of fossil fuel concentrations [CO2F] 
p1 <- plotebv(inters5) 
p1 + latticeExtra::layer(sp.points(cr, pch = 19, col = 'red', cex = 0.4))

dev.copy(pdf, 'Figure5.pdf')
dev.off()
getwd()


# Plot of Uncertainty map of spatial interpolations of fossil fuel concentrations [CO2F] 
inters2sd <- inters
sdfrom <- calc(inters2sd, fun = sd)
p1 <- plotebv(sdfrom, col.regions = rev(viridis_pal(option = 'C')(255))) 
p2 <- p1 + latticeExtra::layer(sp.points(cr, pch = 19, col = 'white', cex = 0.4), columns = 1, rows = 1)
p2


dev.copy(pdf, 'Figure.pdf')
dev.off()
getwd()


