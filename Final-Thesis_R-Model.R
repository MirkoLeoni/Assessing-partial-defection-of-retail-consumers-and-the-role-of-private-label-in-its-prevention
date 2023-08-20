options(prompt = "R> ", continue = "+  ")
library(data.table)
library(depmixS4)
library(lubridate)

path = "C:/Users/loryp/OneDrive - Politecnico di Milano/TESI/Dati_TESI"
#anagrafica_pvl = fread(paste(path,"Anagrafica_pvl.csv",sep="/"),dec=",",header=T)

dt_mediobasso = fread(paste(path,"Dataset_TESI_TestMedioBasso.csv",sep="/"),dec=",",header=T)
dt_alto = fread(paste(path,"Dataset_TESI_TestAlto.csv",sep="/"),dec=",",header=T)

colnames(dt_mediobasso)[c(2,5,8,10,11)] = c("datascontrino","numerotessera","totvenduto","eanprodotto","descrizione")
colnames(dt_alto)[c(2,5,8,9,10)] = c("datascontrino","numerotessera","totvenduto","eanprodotto","descrizione")

dt_alto[,totvenduto:=as.numeric(totvenduto)]
dt_mediobasso[,totvenduto:=as.numeric(totvenduto)]
dt_alto[,numerotessera:=as.character(numerotessera)]
dt_mediobasso[,numerotessera:=as.character(numerotessera)]



aux_a = dt_alto[,.(tot=sum(totvenduto)), by="numerotessera"]
aux_mb = dt_mediobasso[,.(tot=sum(totvenduto)), by="numerotessera"]

random_id = unique(aux_mb$numerotessera)[1:5000]


dt_transazioni_raw = rbind(dt_alto[,.(datascontrino,numerotessera,totvenduto,eanprodotto,descrizione)],
                           dt_mediobasso[numerotessera %in% random_id,.(datascontrino,numerotessera,totvenduto,eanprodotto,descrizione)])

#dt_transazioni_raw = dt_mediobasso[,.(datascontrino,numerotessera,totvenduto,eanprodotto,descrizione)]


dt_transazioni_clean = dt_transazioni_raw[totvenduto>0, .(datascontrino,numerotessera,totvenduto,eanprodotto,descrizione)]
dt_transazioni_clean[,datascontrino := as.character(datascontrino)]
dt_transazioni_clean[,numerotessera := as.character(numerotessera)]
dt_transazioni_clean[,eanprodotto := as.character(eanprodotto)]
dt_transazioni_clean[,descrizione := as.character(descrizione)]
dt_transazioni_clean[,totvenduto := as.numeric(totvenduto)]

#anagrafica_pvl[,eanprodotto:=as.character(eanprodotto)]
#dt_transazioni_clean = merge(dt_transazioni_clean,anagrafica_pvl,by=c("eanprodotto","descrizione"),all.x=T)
#dt_transazioni_clean[is.na(dt_transazioni_clean)==T,]=0

# gestione prodotti

aux_anagrafica = unique(dt_transazioni_clean[substr(eanprodotto,1,8) == "80028950","descrizione"])
aux_anagrafica[,pvl_crai01:=as.integer(grepl("CRAI",descrizione,fixed=T))]
pvl_CRAI = c("CUORI DI LATTUGA CRA","OLIO SEMI ARACHIDE C","BEV. AVENA BIO C","FUNGHI TRIFOLATI CRA",
             "SHAMPOO NEUTRO C","PANE GRATTUGIATO CRA","CANNELLA MAC G35 CRA","PANETTI CR","6 GEL.CONI BIGUSTO C",
             "VALERIANELLA BIO CRA","PIATTI FONDI ECO CRA","SEPPIIOLINE PULITE C","PIATTI FRUTTA ECO CR","FUNGHI CHAMPIGNON CR",
             "INSALATA RUSSA C","BENDA COES.AUTOAD.CR","SUPER MOP RICAMBIO C","SIST.CATTURAPOLV.CRA","PEPER.FILETTI R/G CR",
             "GUANTI CASA CRA","LIEVITO PER DOLCI CR","INSAPOR.X PATATE C","PEPER.AGRODOLCE  CRA","CANNELLA STECCHE CRA",
             "MISTO LEG/CEREA. CRA")
aux_anagrafica[,pvl_crai02:=ifelse(descrizione %in% pvl_CRAI,1,0)]
aux_anagrafica[,pvl_CRAI:=ifelse((pvl_crai01+pvl_crai02)==0,0,1)]
aux_anagrafica[,pvl_pi01:=as.integer(grepl("P.I",descrizione,fixed=T))]
pvl_PI = c("RISO NEROPIAC ITALIANI G500","GEL.PI G200 LIMONE SICILIA","FUNGHI CHAMPIGNON PI",
           "RASCHERA DOP PIAC. I","TROFIE PIACERI ITALIANI 500GR","CARCIOFI INT.CRAI PI",
           "GRAPPA PIAC.ITAL.STR","FIL.ACCIUGHE PIAC.IT","LIQ. MIRTO PIACERI I","NOCCIOLE P.I.PIEMONT",
           "PIADINA ROMAGNOLA P ITA 360GR","RAVIOLONI P/ITAL RIC/SPIN 250G","MOZZ.BUFALA CAMP. DOP PI G250",
           "MIELE ACACIA PIACERI ITAL 400G","GELATO PIAC.ITA","FORM GORGONZOLA DOP P IT 200GR","BATTUTA DI FASSONE")
aux_anagrafica[,pvl_pi02:=ifelse(descrizione %in% pvl_PI,1,0)]
aux_anagrafica[,pvl_pi03:=as.integer(grepl("P/I",descrizione,fixed=T))]
aux_anagrafica[,pvl_PI:=ifelse((pvl_pi01+pvl_pi02+pvl_pi03)==0,0,1)]
aux_anagrafica[pvl_PI+pvl_CRAI == 2, "pvl_CRAI"] = 0
aux_anagrafica[,pvl_toremove:=as.integer(grepl("SHOP",descrizione,fixed=T))]
aux_anagrafica = aux_anagrafica[pvl_toremove != 1]

aux_anagrafica[,eco_toremove:=ifelse((as.integer(grepl("BIO",descrizione,fixed=T))+as.integer(grepl("SCEL",descrizione,fixed=T))) != 0 & (pvl_CRAI+pvl_PI)==0,1,0) ]
aux_anagrafica = aux_anagrafica[eco_toremove == 0]

anagrafica_test = unique(dt_transazioni_clean[substr(eanprodotto,1,8) == "80028950",.(eanprodotto,descrizione)])
anagrafica_test = merge(anagrafica_test, aux_anagrafica[,.(descrizione,pvl_PI,pvl_CRAI)],by= c("descrizione"))
anagrafica_test = anagrafica_test[,pvl_eco:=ifelse((pvl_CRAI+pvl_PI)==0,1,0)]

dt_transazioni_clean = merge(dt_transazioni_clean,anagrafica_test,by=c("eanprodotto","descrizione"),all.x=T)
dt_transazioni_clean[is.na(dt_transazioni_clean)==T]=0

# creazione scontrini
dt_scontrini = dt_transazioni_clean[,.(totscontrino=sum(totvenduto),pvl_eco=sum(pvl_eco),pvl_CRAI=sum(pvl_CRAI),pvl_PI=sum(pvl_PI),totprods=.N),
                                    by=.(datascontrino,numerotessera)]
dt_scontrini[, meseanno := as.numeric(substring(datascontrino,1,6))]
dt_scontrini = dt_scontrini[][order(numerotessera,meseanno)]

# Cashiers detection ----
is_cashier = dt_scontrini[,.(totgiorni=length(unique(substring(datascontrino,1,8))),totspese=.N ),by=numerotessera][order(numerotessera)]
is_cashier[,ratio := totspese/totgiorni]
is_cashier[,cashier := ifelse(is_cashier[,ratio]>2,T,F)]
id_cashiers = is_cashier[cashier == T,numerotessera]
table(is_cashier$cashier)

dt_scontrini = dt_scontrini[!(numerotessera %in% id_cashiers), ]

# Monetary-Frequency by month ----
# noi avevamo considerato solo da 202009-202209, ora proviamo considerando da 202010-202210
MF_mese = dt_scontrini[,.(mon=sum(totscontrino),freq=.N,totprods=sum(totprods),pvl_eco=sum(pvl_eco),pvl_CRAI=sum(pvl_CRAI),pvl_PI=sum(pvl_PI)),
                       by=.(numerotessera,meseanno)][order(-numerotessera,meseanno)]
MF_mese[, perc_pvl_eco:=pvl_eco/totprods]
MF_mese[, perc_CRAI:=pvl_CRAI/totprods]
MF_mese[, perc_PI:=pvl_PI/totprods]
MF_mese = MF_mese[meseanno!=202011 & meseanno!=202211 & meseanno!=202010 & meseanno!=202210,] # & meseanno!=202010 & meseanno!=202210

MF_mese[,less_then:=ifelse(mon<=5,1,0)]
aux = MF_mese[,.(less_then=sum(less_then)),by=.(numerotessera)]

id_touse = unique(aux[less_then<=5,numerotessera])

MF_mese = MF_mese[numerotessera %in% id_touse,]


MF_mese = MF_mese[!(numerotessera %in% unique(MF_mese[mon<25 & perc_PI>0.02, numerotessera])),]
MF_mese = MF_mese[!(numerotessera %in% unique(MF_mese[mon>0 & perc_CRAI>0.50, numerotessera])),]

# Create time serie ----
meseanno_array = sort(unique(MF_mese$meseanno))
# in ts_raw ogni cliente ha una time series da 24 periodi, se  nel periodo i non ha acquistato verra messo uno 0
ts_raw = merge(x = data.table( numerotessera =  sort(rep(unique(MF_mese$numerotessera), length(meseanno_array))),
                               meseanno = rep(meseanno_array, length(unique(MF_mese$numerotessera))) ),
               y = MF_mese, by = c("numerotessera", "meseanno"), all.x = T)
ts_raw[is.na(ts_raw) == T] = 0
ts_raw[,is_buying:=ifelse(freq==0,0,1)]

activity_p = ts_raw[,.(activity_periods = (max(which(is_buying==1))-min(which(is_buying==1))+1),buying_periods=sum(is_buying)),by=numerotessera]
active_cust = sort(activity_p[activity_periods>=13 & buying_periods/activity_periods>=0.5,numerotessera])
length(active_cust)
ts_raw = ts_raw[numerotessera %in% active_cust][order(numerotessera,meseanno)]

# in ts_clean ogni cliente ha la time series a partire dal suo primo acquisto, se ha churnato "i" periodi del churn avranno come valore 0
for (i in active_cust) {
  if (i == active_cust[1]) {
    first_purchase = ts_raw[freq != 0, .(first_purchase = min(meseanno)), by = numerotessera]
    ts_clean = data.frame()
    j=1
  }
  ts_clean = rbind(ts_clean, ts_raw[ numerotessera == i & meseanno >= first_purchase[numerotessera == i, first_purchase],] )
  if(j%%1000 == 0) { print(j) }
  j=j+1
  if (i == active_cust[length(active_cust)]) { 
    rm(list = c("i","j")) 
    ts_clean = ts_clean[][order(numerotessera, meseanno)]
    print("---- END ----")
  }
}

# HMM fitting ----
set.seed(1)
ntimes = ts_clean[,.N,by=numerotessera]$N
mod_stab = depmix(response = list(mon~1,freq~1), data = as.data.frame(ts_clean), nstates = 3,
                  ntimes = ntimes, family = list(gaussian(),poisson("log")),transition = ~perc_CRAI+perc_PI)
fm_stab = fit(mod_stab)
summary(fm_stab)





# composizione classi ----

dt_posterior = data.table(numerotessera=ts_clean$numerotessera,meseanno=ts_clean$meseanno,posterior(fm_stab, type = "filtering"))
colnames(dt_posterior)[3:4] = c("St1_2","St2_1")
dt_posterior = dt_posterior[,.(numerotessera,meseanno,St2_1,St1_2)]
dt_posterior[,Class:=ifelse(St1_2>St2_1,2,1)]





