## global.R ##
library("dplyr")
library("xlsx")
library("tidyverse")
####################################################################################################
# DONNEES CARTOGRAPHIQUES
####################################################################################################
# 10 VARIABLES FACTOR 
# Station;Commune;Adresse;Identifiant;Type;Connecteur;StationCode;ChargeBoxIdentity;CodePDC;PDL_IDC;
# 4 INT Numero;CodeInsee;lat;lon
# 3/01/19 Bornes avec ruralite
df <- read.csv2("data/bornesWithRuralite.csv",encoding = "UTF-8")
# idc <- slice(df,1)
# idc$IDC
df <- mutate(df,IDC=gsub(pattern="IDC",replacement="",PDL_IDC))
df <- mutate(df,IDC=gsub(pattern="PDL",replacement="",IDC))                 
df <- mutate(df,IDC=gsub(pattern=" ",replacement="",IDC))
df$IDC <- as.factor(df$IDC)
# 6/05 conserver uniquement les stations pour lesquelles on a le signal ENEDIS
df <- filter(df, IDC != "")
dfresume <- df %>% group_by(IDC) %>% summarise(Station=unique(Station),
                                         Commune=unique(Commune),
                                         Adresse=first(Adresse),
                                         Type=paste(Type,collapse="-"),
                                         Connecteur=paste(Connecteur,collapse="-"),
                                         CodePDC=first(CodePDC),#2 codes pour Laragne et Risoul
                                         Numero=paste(Numero,collapse="-"),
                                         CodeInsee=paste(CodeInsee,collapse="-"),
                                         StationCode=unique(StationCode),
                                         ChargeBoxIdentity=first(ChargeBoxIdentity),
                                         lat=unique(lat),
                                         lon=unique(lon),
                                         Ruralite=unique(Ruralite)
                                         )
#liste des stations
listStations <- levels(dfresume$Station)
nbstations <- length(listStations)
#liste des PDC
listPDC <- levels(dfresume$CodePDC)
nbPDC <- length(listPDC)
#liste des Villes / Communes ?
listVilles <- levels(dfresume$Commune)
nbVilles <- length(listVilles)
##################################################################################################
# SIGNAL ENEDIS/RTE : a refaire chaque jour
# le signal porte sur toute la journee periode RTE + periode Enedis
##################################################################################################
signaldata <- read.csv2("signal/signal.csv",encoding = "UTF-8")
#colnames(signaldata)
signaldata <- select(signaldata,date,IDC,X0.2,X2.4,X4.6,X6.8,X8.10,X10.12,X12.14,X14.16,X16.18,X18.20,X20.22,X22.24)
colnames(signaldata) <- c("date","IDC","h02","h24","h46","h68","h810","h1012","h1214","h1416","h1618","h1820","h2022","h2224")

# signal as factor => as character
# signaldata <-  mutate_if(signaldata, is.factor, as.character)
signaldata <- signaldata %>% mutate_if(is.factor, as.character)

# dfresume 72 bornes - Signal 71 bornes
# Column `IDC` joining factors with different levels, coercing to character vector
dfresume$IDC <- as.character(dfresume$IDC)
#signaldata$IDC <- as.character(signaldata$IDC)
borneswithsignal <- left_join(dfresume,signaldata,by=c("IDC"))

#2 latitude et longitude as factor => as numeric
borneswithsignal$lat <- as.numeric(as.character(borneswithsignal$lat))
borneswithsignal$lon <- as.numeric(as.character(borneswithsignal$lon))

# pour affichage sur la carto : 
borneswithsignal$h02 <- factor(borneswithsignal$h02,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h24 <- factor(borneswithsignal$h24,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h46 <- factor(borneswithsignal$h46,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h68 <- factor(borneswithsignal$h68,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h810 <- factor(borneswithsignal$h810,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h1012 <- factor(borneswithsignal$h1012,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h1214 <- factor(borneswithsignal$h1214,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h1416 <- factor(borneswithsignal$h1416,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h1618 <- factor(borneswithsignal$h1618,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h1820 <- factor(borneswithsignal$h1820,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h2022 <- factor(borneswithsignal$h2022,levels=c("Blanc","Vert","Vert2"))
borneswithsignal$h2224 <- factor(borneswithsignal$h2224,levels=c("Blanc","Vert","Vert2"))

# Ajout du signal croise
croise <- read.csv2("croise/croise.csv",encoding = "UTF-8")
croise <- add_column(croise, Charge=1)
borneswithsignal <- left_join(borneswithsignal,croise,by=c("CodePDC"="Borne"))
# # si le champ charge NA alors mettre Libre
borneswithsignal <- mutate(borneswithsignal,Charge=ifelse(is.na(Charge),0,Charge))
# #write.csv2(borneswithsignal,"testCarto.csv",row.names=FALSE,fileEncoding = "UTF-8",quote=FALSE)
myIcons <- iconList(
   charge = makeIcon("flash.png", "ferry-18@2x.png", 18, 18),
   libre = makeIcon("empty.png", "danger-24@2x.png", 2,2)
 )
# #croise <- read.csv2("testCarto.csv",encoding = "UTF-8")
borneswithsignal <- mutate(borneswithsignal,type=factor(ifelse(Charge==1,"charge","libre")))

####################################################################################
# IDC==25642042 inconnu IDC==25642041 connu
# signaljour <- select(filter(signaldata,IDC==25642041&date=="31-07-2019"),c("h02","h24","h46","h68","h810","h1012","h1214","h1416","h1618","h1820","h2022","h2224"))
##################################################################################################
# SIGNAL ENEDIS HISTORIQUE : 30 juillet 2019
##################################################################################################
# fichier original UC2.xlsx traite par traitementSignalHisto.R
signalhisto <- read.csv2("signal/signalhisto.csv",encoding = "UTF-8", stringsAsFactors=FALSE)

##################################################################################################
# SIGNAL ENEDIS/RTE : historique
# du 19 juin au 5 aout - 2087 => 2059 apres suppression entete
# 29 jours
# 71 IDC - normalement
# recuperation de la date, IDC et 12 variables : 12 periodes de 2h
# le signal porte sur toute la journee periode RTE + periode Enedis
##################################################################################################
#setwd("F:/SHINY-DEV/UC2-SIGNALTL")
signalallhisto <- read.csv2("signal/allhisto.csv",encoding = "UTF-8")
#supprimer les lignes d entete
signalallhisto <- filter(signalallhisto,IDC!="IDC")

#colnames(signaldata)
signalallhisto <- select(signalallhisto,date,IDC,X0.2,X2.4,X4.6,X6.8,X8.10,X10.12,X12.14,X14.16,X16.18,X18.20,X20.22,X22.24)
#colnames(signalallhisto) <- c("date","IDC","h02","h24","h46","h68","h810","h1012","h1214","h1416","h1618","h1820","h2022","h2224")
periodes=c('0h-2h','2h-4h','4h-6h','6h-8h','8h-10h','10h-12h','12h-14h','14h-16h','16h-18h','18h-20h','20h-22h','22h-24h')
colnames(signalallhisto) <- c("date","IDC",periodes)
# signal as factor => as character
signalallhisto <- signalallhisto %>% mutate_if(is.factor, as.character)
# date
signalallhisto <- mutate(signalallhisto, date=lubridate::dmy(date))
# verification du nb d observations par jour
# count(signalhisto,date)
# tri par date
signalallhisto <- arrange(signalallhisto,date)

##################################################################################################
# SIGNAL ENEDIS HISTORIQUE + TRANSACTIONS : 12 aout 2019
# V2 le 13 septembre 2019
# decoupage des transactions par signal
# + decoupage des transactions par periode + visu signal
##################################################################################################
# fichier original UC2.xlsx et transactions.csv traites par traitementSignalHisto.R
# toutes les bornes transactions par SIGNAL
transactionsBornehistoMoisAll <- read.csv2("data/transactionsHistoPerSignal.csv",encoding = "UTF-8", stringsAsFactors=TRUE)
transactionsBornehistoMoisAll$Month <- lubridate::ymd(transactionsBornehistoMoisAll$MonthDebut)
transactionsBornehistoMoisAll <- mutate(transactionsBornehistoMoisAll, BorneVille = paste(Borne,Ville))

# avec les bornes ayant des transactions : TOP 20 sur la periode historique 2018 et Hiver 2019
transactionsBornehistoMois <- read.csv2("data/transactionsHistoPerSignalTop.csv",encoding = "UTF-8", stringsAsFactors=TRUE)
transactionsBornehistoMois$Month <- lubridate::ymd(transactionsBornehistoMois$MonthDebut)
transactionsBornehistoMois <- mutate(transactionsBornehistoMois, BorneVille = paste(Borne,Ville))

# ggplot(transactionsBornehistoMois, aes(Month,nbTransaction)) +
#   geom_col(aes(fill=signal))+ 
#   scale_x_date(date_breaks = "1 months",date_labels = "%m\n%y",expand = c(0,0))+
#   scale_fill_manual(values=c("gray96","darkgray","#62A667","green"), 
#                     labels=c("Blanc","nosignal","Vert","Vert2"), drop = FALSE)+
#   facet_wrap( ~BorneVille,ncol=3)
##################################################################################################
# 13/09/2019
# + decoupage des transactions par periode + visu signal
##################################################################################################
# toutes les bornes
transactionsBornehistoPeriodeAll <- read.csv2("data/transactionsHistoPerPeriode.csv",encoding = "UTF-8", stringsAsFactors=TRUE)
transactionsBornehistoPeriodeAll$Month <- lubridate::ymd(transactionsBornehistoPeriodeAll$MonthDebut)
transactionsBornehistoPeriodeAll <- mutate(transactionsBornehistoPeriodeAll, BorneVille = paste(Borne,Ville))
signalBorne <- filter(transactionsBornehistoPeriodeAll,Borne=="FR*S05*S05070*A*B2")
# ordonner la periode du jour
periodes=c("h68","h810","h1012","h1214","h1416","h1618","nuit")
transactionsBornehistoPeriodeAll$periodeJour <- factor(transactionsBornehistoPeriodeAll$periodeJour,levels=periodes)
# data <- filter(transactionsBornehistoPeriodeAll,IDC=="25642065")
# datajuin <- filter(data,Month=="2018-06-01")
# avec les bornes ayant des transactions : TOP 20 sur la periode historique 2018 et Hiver 2019
transactionsBornehistoPeriode <- read.csv2("data/transactionsHistoPerPeriodeTop.csv",encoding = "UTF-8", stringsAsFactors=TRUE)
transactionsBornehistoPeriode$Month <- lubridate::ymd(transactionsBornehistoPeriode$MonthDebut)
transactionsBornehistoPeriode <- mutate(transactionsBornehistoPeriode, BorneVille = paste(Borne,Ville))
# ordonner la periode du jour
# periodes=c("h68","h810","h1012","h1214","h1416","h1618","nuit")
transactionsBornehistoPeriode$periodeJour <- factor(transactionsBornehistoPeriode$periodeJour,levels=periodes)

# ordonner la periode du jour
#   periodes=c('0h-2h','2h-4h','4h-6h','6h-8h','8h-10h','10h-12h','12h-14h','14h-16h',"16h-18h","18h-20h","20h-22h","22h-24h")
periodes=c("h68","h810","h1012","h1214","h1416","h1618","nuit")
transactionsBornehistoPeriode$periodeJour <- factor(transactionsBornehistoPeriode$periodeJour,levels=periodes)

##################################################################################################
# SIGNAL ENEDIS QUOTIDIEN + TRANSACTIONS : 13 septembre 2019
# 
##################################################################################################
# pour affichage des transactions avec le signal quotidien sur 3 mois
transactionsSignalQuotidien <- read.csv2("data/transactionsQuotidienPerPeriode.csv",encoding = "UTF-8", stringsAsFactors=TRUE)
transactionsSignalQuotidien$date <- lubridate::ymd(transactionsSignalQuotidien$DateDebut)
# ordonner la periode du jour
periodes=c('0h-2h','2h-4h','4h-6h','6h-8h','8h-10h','10h-12h','12h-14h','14h-16h','16h-18h','18h-20h','20h-22h','22h-24h')
transactionsSignalQuotidien$periode <- factor(transactionsSignalQuotidien$periode,levels=periodes)
#ajouter le jour et le mois
transactionsSignalQuotidien$mois <- lubridate::month(transactionsSignalQuotidien$date)
# supprimer les données de septembre
transactionsSignalQuotidien <- filter(transactionsSignalQuotidien,(mois>=6)&(mois<=8))
transactionsSignalQuotidien$mois <- factor(transactionsSignalQuotidien$mois,levels=c("6","7","8"))
levels(transactionsSignalQuotidien$mois)<-c("Juin","Juillet","Aout")
transactionsSignalQuotidien$jour <- lubridate::day(transactionsSignalQuotidien$date)
#levels(transactionsSignalQuotidien$jour)
levels(transactionsSignalQuotidien$signal)
#signalBorne <- filter(transactionsSignalQuotidien,IDC=="25642065")
##################################################################################################



