rm(list = ls()) #clear list

rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table")
ipak(packages)

#load packages
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(plyr)
library(zoo)
library(ggthemes)
library(readr)
library(countrycode)
library(devtools)
library(wid)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)

#read data
#note we excluded 
dat <- fread(here("rg-fiscal-stance-final.csv"))
dat <- subset(dat, year %in% c('1985','1986','1987','1988','1989','1990','1991','1992','1993','1994','1995','1996','1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019'))
dat <- subset(dat, ccode %in% c('AUS','AUT','BEL','CAN','DEU','DNK','ESP','FIN','FRA','GBR','GRC','ITA','JPN','KOR','NLD','NOR','NZL','PRT','SWE','USA'))
unique(dat$ccode)

#change type of output gap variable
typeof(dat$outputgap)
dat$outputgap <- as.numeric(dat$outputgap)

#calculate real interest rates
dat$realLTrate <- dat$LTrate - dat$inflation

#invert finreform
dat$fin_repression <- 1/dat$finreform

#change in PDebt
dat <- ddply(dat,"ccode", transform,
                    change_PDebt=c(NA,diff(PDebt))) #calculate population growth

#change in cyclically-adjusted primary balance
dat <- ddply(dat,"ccode", transform,
             change_ca_primary_FB=c(NA,diff(ca_primary_FB))) #calculate population growth

#calculate public debt in dollar terms
dat$PDebt_absolute <- dat$PDebt * dat$GDP

#calculate fiscal space variable (ratio of government debt to total tax revenue)
dat$FiscalSpace <- dat$PDebt_absolute/dat$TaxRevenue/1000000

#calculate net foreign assets to GDP
dat$NetForeignAssets <- dat$Net_foreign_assets_World_Bank / dat$GDP_World_Bank*100

#calculate GDP size
dat$GDP_size <- dat$GDP_World_Bank / dat$World_GDP*100

#calculate spread to Germany
dat$spread_Germany <- dat$LTrate - dat$LTrate_Germany

#calculate spread to US
dat$spread_USA <- dat$LTrate - dat$LTrate_USA

#calculate sum of public debt
dat <- 
  dat %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(sum_PDebt = sum(PDebt_absolute, na.rm=TRUE)) %>%
  ungroup()

#size of debt in relation to OECD total
dat$debt_size <- dat$PDebt_absolute/dat$sum_PDebt*100

#rename variables according to introduction in the paper
dat$r<-dat$LTrate
dat$g<-dat$GDP_growth
dat$real_GDP_gr<-dat$real_GDP_growth
dat$real_LTrate<- dat$realLTrate
dat$safety<-dat$safe_Germany
dat$fin_open<-dat$ka_open
dat$NFA<-dat$NetForeignAssets
dat$EuroMember<-dat$EuroMembership
dat$EuroCrisis<-dat$EuroCrisis20082012
dat$StandaCrisis<-dat$Standalone20082012

#country sub-samples: Euro and standalone
dat_Euro <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'ESP', 'FIN', 'FRA', 'GRC', 'IRL', 'ITA', 'NLD', 'PRT'))
dat_standalone <- subset(dat, ccode %in% c('AUS', 'CAN', 'CHE', 'DNK', 'GBR', 'JPN', 'KOR', 'NOR', 'NZL', 'SWE', 'USA'))

#Euro periphery vs. core
dat_Euro_periphery <- subset(dat, ccode %in% c('ESP', 'GRC', 'IRL', 'ITA', 'PRT'))
dat_Euro_core <- subset(dat, ccode %in% c('AUT', 'BEL', 'DEU', 'FIN', 'FRA', 'NLD'))

#3-year average
dat_3_year <- dat %>%
  dplyr::arrange(ccode, year) %>%
  dplyr::group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  dplyr::group_by(ccode, Period) %>%
  dplyr::summarise_all(mean)

dat_Euro_3_year <- dat_Euro %>%
  dplyr::arrange(ccode, year) %>%
  dplyr::group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  dplyr::group_by(ccode, Period) %>%
  dplyr::summarise_all(mean)

dat_standalone_3_year <- dat_standalone %>%
  dplyr::arrange(ccode, year) %>%
  dplyr::group_by(ccode) %>%
  dplyr::mutate(Period = gl(ceiling(n() / 3), 3, length = n())) %>%
  dplyr::group_by(ccode, Period) %>%
  dplyr::summarise_all(mean)

#descriptive statistics
#mean
mean(dat$rminusg, na.rm=TRUE)

#median
median(dat$rminusg, na.rm=TRUE)

#minmax
min(dat$rminusg, na.rm=TRUE)
max(dat$rminusg, na.rm=TRUE)

#standard deviation
sd(dat$rminusg, na.rm=TRUE)

#density plot
partialvector <- dat$rminusg
m<-mean(dat$rminusg, na.rm=TRUE)
std<-sqrt(var(dat$rminusg, na.rm=TRUE))

#Distribution of r-g differentials (Figure A1)
hist(partialvector,breaks = 30, freq=F,main="Distribution of interest-growth differentials (r-g)",xlab="interest-growth differential (r-g) in percentage points",ylab="Density", ylim=c(0,0.12), xlim=c(-30,30))
lines(density(partialvector, na.rm=TRUE), col="black", lwd=2) 
abline(v = 0.869776, col="red", lwd=3, lty=2)
abline(v = 0.3911451, col="green", lwd=3, lty=2)
text(8, 0.1, "mean = 0.87", col="red")
text(-9, 0.1, "median = 0.39", col="green")
density(partialvector, na.rm=TRUE)

#Does r-g predict the fiscal stance?
#3-year-ahead cyclically-adjusted primary balance
dat <- 
  dat %>%
  dplyr::arrange(ccode) %>%
  dplyr::group_by(ccode) %>%
  dplyr::mutate(lead_ca_primary_FB_1 = dplyr::lead(ca_primary_FB, n = 1, default = NA)) %>%
  dplyr::mutate(lead_ca_primary_FB_2 = dplyr::lead(ca_primary_FB, n = 2, default = NA)) %>%
  dplyr::mutate(lead_ca_primary_FB_3 = dplyr::lead(ca_primary_FB, n = 3, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_1 = dplyr::lead(primary_FB, n = 1, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_2 = dplyr::lead(primary_FB, n = 2, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_3 = dplyr::lead(primary_FB, n = 3, default = NA))

dat_ca_primary_FB_future3 <- data.frame(dat$lead_ca_primary_FB_1, dat$lead_ca_primary_FB_2, dat$lead_ca_primary_FB_3)
dat_ca_primary_FB_future3$ca_primary_FB_future3 <- rowMeans(dat_ca_primary_FB_future3, na.rm=TRUE)
dat_ca_primary_FB_future3<- select(dat_ca_primary_FB_future3, ca_primary_FB_future3)
dat <- data.frame(dat, dat_ca_primary_FB_future3)

dat_primary_FB_future3 <- data.frame(dat$lead_primary_FB_1, dat$lead_primary_FB_2, dat$lead_primary_FB_3)
dat_primary_FB_future3$primary_FB_future3 <- rowMeans(dat_primary_FB_future3, na.rm=TRUE)
dat_primary_FB_future3<- select(dat_primary_FB_future3, primary_FB_future3)
dat <- data.frame(dat, dat_primary_FB_future3)

dat_Euro <- 
  dat_Euro %>%
  dplyr::arrange(ccode) %>%
  dplyr::group_by(ccode) %>%
  dplyr::mutate(lead_ca_primary_FB_1 = dplyr::lead(ca_primary_FB, n = 1, default = NA)) %>%
  dplyr::mutate(lead_ca_primary_FB_2 = dplyr::lead(ca_primary_FB, n = 2, default = NA)) %>%
  dplyr::mutate(lead_ca_primary_FB_3 = dplyr::lead(ca_primary_FB, n = 3, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_1 = dplyr::lead(primary_FB, n = 1, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_2 = dplyr::lead(primary_FB, n = 2, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_3 = dplyr::lead(primary_FB, n = 3, default = NA))

dat_ca_primary_FB_future3_Euro <- data.frame(dat_Euro$lead_ca_primary_FB_1, dat_Euro$lead_ca_primary_FB_2, dat_Euro$lead_ca_primary_FB_3)
dat_ca_primary_FB_future3_Euro$ca_primary_FB_future3 <- rowMeans(dat_ca_primary_FB_future3_Euro, na.rm=TRUE)
dat_ca_primary_FB_future3_Euro<- select(dat_ca_primary_FB_future3_Euro, ca_primary_FB_future3)
dat_Euro <- data.frame(dat_Euro, dat_ca_primary_FB_future3_Euro)

dat_primary_FB_future3_Euro <- data.frame(dat_Euro$lead_primary_FB_1, dat_Euro$lead_primary_FB_2, dat_Euro$lead_primary_FB_3)
dat_primary_FB_future3_Euro$primary_FB_future3 <- rowMeans(dat_primary_FB_future3_Euro, na.rm=TRUE)
dat_primary_FB_future3_Euro<- select(dat_primary_FB_future3_Euro, primary_FB_future3)
dat_Euro <- data.frame(dat_Euro, dat_primary_FB_future3_Euro)

dat_standalone <- 
  dat_standalone %>%
  dplyr::arrange(ccode) %>%
  dplyr::group_by(ccode) %>%
  dplyr::mutate(lead_ca_primary_FB_1 = dplyr::lead(ca_primary_FB, n = 1, default = NA)) %>%
  dplyr::mutate(lead_ca_primary_FB_2 = dplyr::lead(ca_primary_FB, n = 2, default = NA)) %>%
  dplyr::mutate(lead_ca_primary_FB_3 = dplyr::lead(ca_primary_FB, n = 3, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_1 = dplyr::lead(primary_FB, n = 1, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_2 = dplyr::lead(primary_FB, n = 2, default = NA)) %>%
  dplyr::mutate(lead_primary_FB_3 = dplyr::lead(primary_FB, n = 3, default = NA))

dat_ca_primary_FB_future3_standalone <- data.frame(dat_standalone$lead_ca_primary_FB_1, dat_standalone$lead_ca_primary_FB_2, dat_standalone$lead_ca_primary_FB_3)
dat_ca_primary_FB_future3_standalone$ca_primary_FB_future3 <- rowMeans(dat_ca_primary_FB_future3_standalone, na.rm=TRUE)
dat_ca_primary_FB_future3_standalone<- select(dat_ca_primary_FB_future3_standalone, ca_primary_FB_future3)
dat_standalone <- data.frame(dat_standalone, dat_ca_primary_FB_future3_standalone)

dat_primary_FB_future3_standalone <- data.frame(dat_standalone$lead_primary_FB_1, dat_standalone$lead_primary_FB_2, dat_standalone$lead_primary_FB_3)
dat_primary_FB_future3_standalone$primary_FB_future3 <- rowMeans(dat_primary_FB_future3_standalone, na.rm=TRUE)
dat_primary_FB_future3_standalone<- select(dat_primary_FB_future3_standalone, primary_FB_future3)
dat_standalone <- data.frame(dat_standalone, dat_primary_FB_future3_standalone)

#panel unit root and cointegration tests
dat_UR <- select(dat, ccode, year, primary_FB)
dat_UR <- na.omit (dat_UR)
dat_UR <- pdata.frame(dat_UR)
dat_test <- select(dat, rminusg, PDebt, rminusg, outputgap, Election, inflation)
dat_test <- na.omit (dat_test)

#Unit root for r-g
dat_UR <- pdata.frame(dat, index = c("ccode", "year"))
cips <- plm::cipstest(dat_UR$rminusg, type = "drift")
cips

#Maddala-Wu test for cointegration (Table A2)
purtest(dat_test, pmax = 4, exo = "intercept", test = "madwu")

#fixed-effects regressions
#results for Table 2

fiscalstance_v2_no_ca <- plm(primary_FB~lag(PDebt) + rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v2_no_ca)
coeftest(fiscalstance_v2_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro_no_ca <- plm(primary_FB~lag(PDebt) + rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(fiscalstance_v2_Euro_no_ca)
coeftest(fiscalstance_v2_Euro_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone_no_ca <- plm(primary_FB~lag(PDebt) + rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(fiscalstance_v2_standalone_no_ca)
coeftest(fiscalstance_v2_standalone_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

#with interaction of PDebt and rminus g
fiscalstance_v2_interact_no_ca <- plm(primary_FB~lag(PDebt) * rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v2_interact_no_ca)
coeftest(fiscalstance_v2_interact_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro_interact_no_ca <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(fiscalstance_v2_Euro_interact_no_ca)
coeftest(fiscalstance_v2_Euro_interact_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone_interact_no_ca <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(fiscalstance_v2_standalone_interact_no_ca)
coeftest(fiscalstance_v2_standalone_interact_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

#plus interaction of PDebt with output gap
fiscalstance_v2_interact_x2_no_ca <- plm(primary_FB~lag(PDebt) * rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation+outputgap*lag(PDebt), index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v2_interact_x2_no_ca)
coeftest(fiscalstance_v2_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro_interact_x2_no_ca <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation+outputgap*lag(PDebt), index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(fiscalstance_v2_Euro_interact_x2_no_ca)
coeftest(fiscalstance_v2_Euro_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone_interact_x2_no_ca <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation+outputgap*lag(PDebt), index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(fiscalstance_v2_standalone_interact_x2_no_ca)
coeftest(fiscalstance_v2_standalone_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="sss"))

#robustness check: 3 year average
fiscalstance_v2_interact_no_ca_3_year <- plm(primary_FB~lag(PDebt) * rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_3_year)
summary(fiscalstance_v2_interact_no_ca_3_year)
coeftest(fiscalstance_v2_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro_interact_no_ca_3_year <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro_3_year)
summary(fiscalstance_v2_Euro_interact_no_ca_3_year)
coeftest(fiscalstance_v2_Euro_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone_interact_no_ca_3_year <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone_3_year)
summary(fiscalstance_v2_standalone_interact_no_ca_3_year)
coeftest(fiscalstance_v2_standalone_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="sss"))

#exclude USA and Germany
dat_exclude_US_DEU <- subset(dat, ccode %in% c('AUS','AUT','BEL','CAN','DNK','ESP','FIN','FRA','GBR','GRC','ITA','JPN','KOR','NLD','NOR','NZL','PRT','SWE'))
dat_exclude_DEU <- subset(dat, ccode %in% c('AUT','BEL','ESP','FIN','FRA','GRC','ITA','NLD','PRT'))
dat_exclude_USA <- subset(dat, ccode %in% c('AUS','CAN','DNK','GBR','JPN','KOR','NOR','NZL','SWE'))

fiscalstance_v2_interact_no_ca_excl_DEU_USA <- plm(primary_FB~lag(PDebt) * rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_US_DEU)
summary(fiscalstance_v2_interact_no_ca_excl_DEU_USA)
coeftest(fiscalstance_v2_interact_no_ca_excl_DEU_USA, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_interact_no_ca_excl_DEU <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_DEU)
summary(fiscalstance_v2_interact_no_ca_excl_DEU)
coeftest(fiscalstance_v2_interact_no_ca_excl_DEU, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_interact_no_ca_excl_USA  <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_exclude_USA)
summary(fiscalstance_v2_interact_no_ca_excl_USA)
coeftest(fiscalstance_v2_interact_no_ca_excl_USA, vcov.=function(x) vcovHC(x, type="sss"))

#include time fixed effects
fiscalstance_v2_interact_no_ca_tfe <- plm(primary_FB~lag(PDebt) * rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="twoways", data=dat)
summary(fiscalstance_v2_interact_no_ca_tfe)
coeftest(fiscalstance_v2_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro_interact_no_ca_tfe <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="twoways", data=dat_Euro)
summary(fiscalstance_v2_Euro_interact_no_ca_tfe)
coeftest(fiscalstance_v2_Euro_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone_interact_no_ca_tfe <- plm(primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="twoways", data=dat_standalone)
summary(fiscalstance_v2_standalone_interact_no_ca_tfe)
coeftest(fiscalstance_v2_standalone_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="sss"))

#cyclically-adjusted primary fiscal balance
#regressions
fiscalstance_v2 <- plm(ca_primary_FB~lag(PDebt) + rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v2)
coeftest(fiscalstance_v2, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro <- plm(ca_primary_FB~lag(PDebt) + rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(fiscalstance_v2_Euro)
coeftest(fiscalstance_v2_Euro, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone <- plm(ca_primary_FB~lag(PDebt) + rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(fiscalstance_v2_standalone)
coeftest(fiscalstance_v2_standalone, vcov.=function(x) vcovHC(x, type="sss"))

#with interaction of PDebt and rminus g
fiscalstance_v2_interact <- plm(ca_primary_FB~lag(PDebt) * rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v2_interact)
coeftest(fiscalstance_v2_interact, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro_interact <- plm(ca_primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(fiscalstance_v2_Euro_interact)
coeftest(fiscalstance_v2_Euro_interact, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone_interact <- plm(ca_primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation, index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(fiscalstance_v2_standalone_interact)
coeftest(fiscalstance_v2_standalone_interact, vcov.=function(x) vcovHC(x, type="sss"))

#plus interaction of PDebt with output gap
fiscalstance_v2_interact_x2 <- plm(ca_primary_FB~lag(PDebt) * rminusg + outputgap+Election+lag(PDebt) + rminusg + inflation+outputgap*lag(PDebt), index=c("ccode", "year"), model="within", effect="individual", data=dat)
summary(fiscalstance_v2_interact_x2)
coeftest(fiscalstance_v2_interact_x2, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_Euro_interact_x2 <- plm(ca_primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation+outputgap*lag(PDebt), index=c("ccode", "year"), model="within", effect="individual", data=dat_Euro)
summary(fiscalstance_v2_Euro_interact_x2)
coeftest(fiscalstance_v2_Euro_interact_x2, vcov.=function(x) vcovHC(x, type="sss"))

fiscalstance_v2_standalone_interact_x2 <- plm(ca_primary_FB~lag(PDebt) * rminusg+outputgap+Election+lag(PDebt) + rminusg + inflation+outputgap*lag(PDebt), index=c("ccode", "year"), model="within", effect="individual", data=dat_standalone)
summary(fiscalstance_v2_standalone_interact_x2)
coeftest(fiscalstance_v2_standalone_interact_x2, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.fiscalstance_v2_no_ca <- list(coeftest(fiscalstance_v2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_no_ca <- list(coeftest(fiscalstance_v2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_no_ca <- list(coeftest(fiscalstance_v2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro_no_ca <- list(coeftest(fiscalstance_v2_Euro_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro_no_ca <- list(coeftest(fiscalstance_v2_Euro_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro_no_ca <- list(coeftest(fiscalstance_v2_Euro_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_standalone_no_ca <- list(coeftest(fiscalstance_v2_standalone_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_standalone_no_ca <- list(coeftest(fiscalstance_v2_standalone_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_standalone_no_ca <- list(coeftest(fiscalstance_v2_standalone_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_no_ca <- list(coeftest(fiscalstance_v2_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_no_ca <- list(coeftest(fiscalstance_v2_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_no_ca <- list(coeftest(fiscalstance_v2_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro_interact_no_ca <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro_interact_no_ca <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro_interact_no_ca <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_standalone_interact_no_ca <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_standalone_interact_no_ca <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_standalone_interact_no_ca <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_standalone_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_standalone_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_standalone_interact_no_ca_3_year <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca_3_year, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_no_ca_excl_DEU_USA <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_DEU_USA, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_no_ca_excl_DEU_USA <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_DEU_USA, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_no_ca_excl_DEU_USA <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_DEU_USA, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_no_ca_excl_DEU <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_DEU, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_no_ca_excl_DEU <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_DEU, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_no_ca_excl_DEU <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_DEU, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_no_ca_excl_USA <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_USA, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_no_ca_excl_USA <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_USA, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_no_ca_excl_USA <- list(coeftest(fiscalstance_v2_interact_no_ca_excl_USA, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_Euro_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_standalone_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_standalone_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_standalone_interact_no_ca_tfe <- list(coeftest(fiscalstance_v2_standalone_interact_no_ca_tfe, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_x2_no_ca <- list(coeftest(fiscalstance_v2_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_x2_no_ca <- list(coeftest(fiscalstance_v2_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_x2_no_ca <- list(coeftest(fiscalstance_v2_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro_interact_x2_no_ca <- list(coeftest(fiscalstance_v2_Euro_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro_interact_x2_no_ca <- list(coeftest(fiscalstance_v2_Euro_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro_interact_x2_no_ca <- list(coeftest(fiscalstance_v2_Euro_interact_x2_no_ca, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2 <- list(coeftest(fiscalstance_v2, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2 <- list(coeftest(fiscalstance_v2, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2 <- list(coeftest(fiscalstance_v2, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro <- list(coeftest(fiscalstance_v2_Euro, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro <- list(coeftest(fiscalstance_v2_Euro, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro <- list(coeftest(fiscalstance_v2_Euro, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_standalone <- list(coeftest(fiscalstance_v2_standalone, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_standalone <- list(coeftest(fiscalstance_v2_standalone, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_standalone <- list(coeftest(fiscalstance_v2_standalone, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact <- list(coeftest(fiscalstance_v2_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact <- list(coeftest(fiscalstance_v2_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact <- list(coeftest(fiscalstance_v2_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro_interact <- list(coeftest(fiscalstance_v2_Euro_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro_interact <- list(coeftest(fiscalstance_v2_Euro_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro_interact <- list(coeftest(fiscalstance_v2_Euro_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_standalone_interact <- list(coeftest(fiscalstance_v2_standalone_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_standalone_interact <- list(coeftest(fiscalstance_v2_standalone_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_standalone_interact <- list(coeftest(fiscalstance_v2_standalone_interact, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_interact_x2 <- list(coeftest(fiscalstance_v2_interact_x2, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_interact_x2 <- list(coeftest(fiscalstance_v2_interact_x2, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_interact_x2 <- list(coeftest(fiscalstance_v2_interact_x2, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

ses.fiscalstance_v2_Euro_interact_x2 <- list(coeftest(fiscalstance_v2_Euro_interact_x2, vcov.=function(x) vcovHC(x, type="HC1"))[,2]) #heteroskedasticity-robust standard errors
tvals.fiscalstance_v2_Euro_interact_x2 <- list(coeftest(fiscalstance_v2_Euro_interact_x2, vcov.=function(x) vcovHC(x, type="HC1"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.fiscalstance_v2_Euro_interact_x2 <- list(coeftest(fiscalstance_v2_Euro_interact_x2, vcov.=function(x) vcovHC(x, type="HC1"))[,4]) # heteroskedasticity-robust p-val

#main regression results (table 2)
stargazer(fiscalstance_v2_no_ca, fiscalstance_v2_Euro_no_ca, fiscalstance_v2_standalone_no_ca, fiscalstance_v2_interact_no_ca, fiscalstance_v2_Euro_interact_no_ca, fiscalstance_v2_standalone_interact_no_ca, fiscalstance_v2_interact, fiscalstance_v2_Euro_interact, fiscalstance_v2_standalone_interact, t=list(unlist(tvals.fiscalstance_v2_no_ca), unlist(tvals.fiscalstance_v2_Euro_no_ca),unlist(tvals.fiscalstance_v2_standalone_no_ca),unlist(tvals.fiscalstance_v2_interact_no_ca), unlist(tvals.fiscalstance_v2_Euro_interact_no_ca),unlist(tvals.fiscalstance_v2_standalone_interact_no_ca), unlist(tvals.fiscalstance_v2_interact), unlist(tvals.fiscalstance_v2_Euro_interact),unlist(tvals.fiscalstance_v2_standalone_interact)), se=list(unlist(ses.fiscalstance_v2_no_ca), unlist(ses.fiscalstance_v2_Euro_no_ca),unlist(ses.fiscalstance_v2_standalone_no_ca),unlist(ses.fiscalstance_v2_interact_no_ca), unlist(ses.fiscalstance_v2_Euro_interact_no_ca),unlist(ses.fiscalstance_v2_standalone_interact_no_ca), unlist(ses.fiscalstance_v2_interact),unlist(ses.fiscalstance_v2_Euro_interact),unlist(ses.fiscalstance_v2_standalone_interact)), p=list(unlist(pvals.fiscalstance_v2_no_ca), unlist(pvals.fiscalstance_v2_Euro_no_ca),unlist(pvals.fiscalstance_v2_standalone_no_ca),unlist(pvals.fiscalstance_v2_interact_no_ca), unlist(pvals.fiscalstance_v2_Euro_interact_no_ca),unlist(pvals.fiscalstance_v2_standalone_interact_no_ca), unlist(pvals.fiscalstance_v2_interact), unlist(pvals.fiscalstance_v2_Euro_interact),unlist(pvals.fiscalstance_v2_standalone_interact)))

#robustness checks (Table A3)
stargazer(fiscalstance_v2_interact_no_ca_excl_DEU_USA, fiscalstance_v2_interact_no_ca_excl_DEU, fiscalstance_v2_interact_no_ca_excl_USA, fiscalstance_v2_interact_no_ca_3_year, fiscalstance_v2_Euro_interact_no_ca_3_year, fiscalstance_v2_standalone_interact_no_ca_3_year, fiscalstance_v2_interact_no_ca_tfe, fiscalstance_v2_Euro_interact_no_ca_tfe, fiscalstance_v2_standalone_interact_no_ca_tfe, t=list(unlist(tvals.fiscalstance_v2_interact_no_ca_excl_DEU_USA),unlist(tvals.fiscalstance_v2_interact_no_ca_excl_DEU), unlist(tvals.fiscalstance_v2_interact_no_ca_excl_USA), unlist(tvals.fiscalstance_v2_interact_no_ca_3_year), unlist(tvals.fiscalstance_v2_Euro_interact_no_ca_3_year),unlist(tvals.fiscalstance_v2_standalone_interact_no_ca_3_year),unlist(tvals.fiscalstance_v2_interact_no_ca_tfe), unlist(tvals.fiscalstance_v2_Euro_interact_no_ca_tfe),unlist(tvals.fiscalstance_v2_standalone_interact_no_ca_tfe)), se=list(unlist(ses.fiscalstance_v2_interact_no_ca_excl_DEU_USA), unlist(ses.fiscalstance_v2_interact_no_ca_excl_DEU), unlist(ses.fiscalstance_v2_interact_no_ca_excl_USA), unlist(ses.fiscalstance_v2_interact_no_ca_3_year), unlist(ses.fiscalstance_v2_Euro_interact_no_ca_3_year),unlist(ses.fiscalstance_v2_standalone_interact_no_ca_3_year),unlist(ses.fiscalstance_v2_interact_no_ca_tfe),unlist(ses.fiscalstance_v2_Euro_interact_no_ca_tfe),unlist(ses.fiscalstance_v2_standalone_interact_no_ca_tfe)), p=list(unlist(pvals.fiscalstance_v2_interact_no_ca_excl_DEU_USA),unlist(pvals.fiscalstance_v2_interact_no_ca_excl_DEU), unlist(pvals.fiscalstance_v2_interact_no_ca_excl_USA), unlist(pvals.fiscalstance_v2_interact_no_ca_3_year), unlist(pvals.fiscalstance_v2_Euro_interact_no_ca_3_year),unlist(pvals.fiscalstance_v2_standalone_interact_no_ca_3_year),unlist(pvals.fiscalstance_v2_interact_no_ca_tfe),unlist(pvals.fiscalstance_v2_Euro_interact_no_ca_tfe), unlist(pvals.fiscalstance_v2_standalone_interact_no_ca_tfe)))

#quantile regressions

#all countries
quantile_q10_allcountries <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat, tau = 0.1)
summary(quantile_q10_allcountries)

quantile_q50_allcountries <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat, tau = 0.5)
summary(quantile_q50_allcountries)

quantile_q90_allcountries <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat, tau = 0.9)
summary(quantile_q90_allcountries)

dat$model_Q10_allcountries <- stats::predict(quantile_q10_allcountries, newdata=dat)
dat$model_Q50_allcountries <- stats::predict(quantile_q50_allcountries, newdata=dat)
dat$model_Q90_allcountries <- stats::predict(quantile_q90_allcountries, newdata=dat)

#plot
plot_quantiles_PDebt_allcountries <- ggplot(dat,aes(PDebt,primary_FB_future3,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat, aes(x=rminusg, y=model_Q10_allcountries, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat, aes(x=PDebt, y=model_Q50_allcountries, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat, aes(x=PDebt, y=model_Q90_allcountries, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  xlim(0,250)+
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 3-year future primary fiscal balance (in % of GDP)") +
  ggtitle("Future primary fiscal balance and public debt levels\n 20 OECD countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No", "Yes"), values = c("grey49", "sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_quantiles_PDebt_allcountries

plot_quantiles_rminusg_allcountries <- ggplot(dat,aes(rminusg,primary_FB_future3,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat, aes(x=rminusg, y=model_Q10_allcountries, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat, aes(x=PDebt, y=model_Q50_allcountries, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat, aes(x=PDebt, y=model_Q90_allcountries, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  xlim(-20,30)+
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("r-g differential (in percentage points)") +
  ylab("Predicted 3-year future primary fiscal balance (in % of GDP)") +
  ggtitle("Future primary fiscal balance and r-g\n 20 OECD countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("No", "Yes"), values = c("grey49", "sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_quantiles_rminusg_allcountries

#save plot
#filename <- "figures/plot_quantiles_PDebt_allcountries.pdf"
#ggsave(filename, plot = plot_quantiles_PDebt_allcountries, width = 10, height = 6)

#Euro
quantile_q10_Euro <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat_Euro, tau = 0.1)
summary(quantile_q10_Euro)

quantile_q50_Euro <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat_Euro, tau = 0.5)
summary(quantile_q50_Euro)

quantile_q90_Euro <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat_Euro, tau = 0.9)
summary(quantile_q90_Euro)

dat_Euro$model_Q10_Euro <- stats::predict(quantile_q10_Euro, newdata=dat_Euro)
dat_Euro$model_Q50_Euro <- stats::predict(quantile_q50_Euro, newdata=dat_Euro)
dat_Euro$model_Q90_Euro <- stats::predict(quantile_q90_Euro, newdata=dat_Euro)

#plot
plot_quantiles_PDebt_Euro <- ggplot(dat_Euro,aes(PDebt,primary_FB_future3,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro, aes(x=rminusg, y=model_Q10_Euro, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=PDebt, y=model_Q50_Euro, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=PDebt, y=model_Q90_Euro, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  xlim(0,250)+
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 3-year future primary fiscal balance (in % of GDP)") +
  ggtitle("Future primary fiscal balance and public debt levels\n 10 Euro countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("Yes", "No"), values = c("grey49", "sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_quantiles_PDebt_Euro

plot_quantiles_rminusg_Euro <- ggplot(dat_Euro,aes(rminusg,primary_FB_future3,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_Euro, aes(x=rminusg, y=model_Q10_Euro, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=PDebt, y=model_Q50_Euro, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_Euro, aes(x=PDebt, y=model_Q90_Euro, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  xlim(-20,40)+
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("r-g differential (in percentage points)") +
  ylab("Predicted 3-year future primary fiscal balance (in % of GDP)") +
  ggtitle("Future primary fiscal balance and r-g\n 10 Euro countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='Eurozone') +
  scale_color_manual(labels = c("Yes", "No"), values = c("grey49", "sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_quantiles_rminusg_Euro

#save plot
#filename <- "figures/plot_quantiles_PDebt_Euro.pdf"
#ggsave(filename, plot = plot_quantiles_PDebt_Euro, width = 10, height = 6)

#standalone
quantile_q10_standalone <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat_standalone, tau = 0.1)
summary(quantile_q10_standalone)

quantile_q50_standalone <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat_standalone, tau = 0.5)
summary(quantile_q50_standalone)

quantile_q90_standalone <- rq(primary_FB_future3~PDebt + rminusg + PDebt * rminusg + outputgap + Election + inflation, data=dat_standalone, tau = 0.9)
summary(quantile_q90_standalone)

dat_standalone$model_Q10_standalone <- stats::predict(quantile_q10_standalone, newdata=dat_standalone)
dat_standalone$model_Q50_standalone <- stats::predict(quantile_q50_standalone, newdata=dat_standalone)
dat_standalone$model_Q90_standalone <- stats::predict(quantile_q90_standalone, newdata=dat_standalone)

#plot
plot_quantiles_PDebt_standalone <- ggplot(dat_standalone,aes(PDebt,primary_FB_future3,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_standalone, aes(x=rminusg, y=model_Q10_standalone, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=PDebt, y=model_Q50_standalone, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=PDebt, y=model_Q90_standalone, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  xlim(0,250)+
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q10", "Q90"))+
  labs(linetype = "Quantile")+
  xlab("Public debt in % of GDP") +
  ylab("Predicted 3-year future primary fiscal balance (in % of GDP)") +
  ggtitle("Future primary fiscal balance and public debt levels\n 10 stand-alone countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='standalone') +
  scale_color_manual(labels = c("Yes", "No"), values = c("sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_quantiles_PDebt_standalone

plot_quantiles_rminusg_standalone <- ggplot(dat_standalone,aes(rminusg,primary_FB_future3,colour=factor(Eurozone)))+
  geom_point()+
  geom_smooth(method="lm", data=dat_standalone, aes(x=rminusg, y=model_Q10_standalone, linetype="dotted"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=PDebt, y=model_Q50_standalone, linetype="dashed"), 
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  geom_smooth(method="lm", data=dat_standalone, aes(x=PDebt, y=model_Q90_standalone, linetype="solid"),
              colour = "slateblue2", se = F, stat = "smooth", size=0.5, fullrange = TRUE) +
  xlim(-20,20)+
  scale_linetype_manual(values=c("dotted", "dashed", "solid"), labels=c("Q50", "Q20", "Q80"))+
  labs(linetype = "Quantile")+
  xlab("r-g differential (in percentage points)") +
  ylab("Predicted 3-year future primary fiscal balance (in % of GDP)") +
  ggtitle("Future primary fiscal balance and r-g\n 10 stand-alone countries")+
  theme(axis.line.x = element_line(color="black", size = 0.1),
        axis.line.y = element_line(color="black", size = 0.1)) +
  theme(axis.title.x=element_text(size=6)) +
  theme(axis.title.y=element_text(size=6)) + 
  labs(color='standalone') +
  scale_color_manual(labels = c("Yes", "No"), values = c("sandybrown")) +
  theme(panel.background = element_rect(fill = "white"))
plot_quantiles_rminusg_standalone

#save plot
#filename <- "figures/plot_quantiles_PDebt_standalone.pdf"
#ggsave(filename, plot = plot_quantiles_PDebt_standalone, width = 10, height = 6)

fig_quantiles_rg <- ggpubr::ggarrange(
  plot_quantiles_rminusg_Euro, plot_quantiles_rminusg_standalone, plot_quantiles_PDebt_Euro, plot_quantiles_PDebt_standalone, ncol = 2, nrow=2, labels = c("A)", "B)", "C)", "D)"), 
  common.legend = TRUE, legend = "bottom")
fig_quantiles_rg

#filename <- "figures/fig_quantiles_rg.pdf"
#ggsave(filename, plot = fig_quantiles_rg, width = 12, height = 10)
