## Title: Investor Sentiment and Anomalies in Brazilian Market
##
## Version: 0.0.1
##
## Description: Script to compute que Investor Sentiment Index of the brazilian
## market.                       
## 

## CONTENTS ## #################################################################
## 1. SETTINGS
## 2. LOAD DATA AND CLEAN
## 3. INVESTOR SENTIMENT INDEX
## 4. PRICING MODELS
## 5. CONSTRUCT PORTFOLIOS
## 6. INVESTOR SENTIMENT AND ANOMALIES
##

#'

## 1. SETTINGS ## ##############################################################
## Definir par?metros
## 
## SOBRE O PERIODO:
##
## Periodo momento (jul do ano n-1 a mai do ano n)
## Para calculo do das Carteiras: precos  de jun/(n  ):jun/(n+1) (13 meses)
##                                retorno de jul/(n  ):jun/(n+1) (12 meses)
##
## Para calculo do Fator Momento: precos  de jun/(n-1):mai/(n  ) (12 meses)
##                                retorno de jul/(n-1):mai/(n  ) (11 meses)
##

# load(paste(getwd(),"/Data/", "20141022_FINAL.RData", sep=""))

## Definir Parametros / Set Parameters -----------------------------------------
setwd("C:/Dropbox/investorSentiment") # Pasta de Trabalho / Working Directory

PERIOD.n <- 1999 # Ano Inicial
PERIOD.N <- 2014 # Ano Final

## Periodo Mensal / Monthly Period
PERIOD.MON <- paste(PERIOD.n,"-06/", PERIOD.N, "-06", sep="")
#                   JUN/n        a     JUN/N    (Ex. 1999-06/2014-06)

## Periodo Anual (Dados de negociacoes) / Yearly  Period
PERIOD.JUN <- paste(PERIOD.n+1,"-06/", PERIOD.N-1, "-06", sep="")
#                   JUN/(n+1)    a    JUN/(N-1) (Ex. 2000-06/2013-06)

## Periodo Anual (Dados de contabeis) / Yearly  Period
PERIOD.DEZ <- paste(PERIOD.n,"-12/", PERIOD.N-2, "-12", sep="")
#                      DEZ/n     a    DEZ/(N-2) (Ex. 1999-12/2012-12)

## Instalar/Carregar pacotes / Install/Load packages -----------------------------------------
ip <- installed.packages()
for (i in c("zoo", "dynlm", "lubridate", "Quandl", "TTR", "XML", "xts", "lmtest", "sandwich", "HH")){
  if ( !(i %in% ip) ) { install.packages(i) }
  if (i=="Quandl") { library(i, character.only = T) ; Quandl.auth("WP2rt8HsRo3kjWsRkLY5") } else library(i, character.only = T)
}
rm(list=c("ip","i"))

## Executar minhas fun?oes / Run my functions
source("R/functions.R")

## 2. LOAD DATA AND CLEAN ## ###################################################
##
## Importar e limpar dados / Load and Clean Data
##

## 2.1 Importar Dados / Load Data # ============================================

### Importar Dados Online / Load Online Data # ---------------------------------

### BAIXAR E SALVAR DADOS LOCALMENTE ###
# # Emissoes de Acoes Iniciais - IPO (Fonte: Site CVM)
# dbCVM_IPO <- coletaVariosAnosCVM(PERIOD.n:PERIOD.N, coletaIPOnaCVM)
# write.table(dbCVM_IPO, "Data/IPOs.csv", quote=F, sep=";", row.names=F)
# # Emissoes de Acoes Subsequentes (Fonte: Site CVM)
# dbCVM_SUB <- coletaVariosAnosCVM(PERIOD.n:PERIOD.N, coletaSubsequentesnaCVM)
# write.table(dbCVM_SUB, "Data/SUBs.csv", quote=F, sep=";", row.names=F)
# # Emissoes de Dividas (Fonte: Site CVM)
# dbCVM_DEB <- coletaVariosAnosCVM(PERIOD.n:PERIOD.N, coletaDEBnaCVM)
# write.table(dbCVM_DEB, "Data/DEBs.csv", quote=F, sep=";", row.names=F)

### LER DADOS BAIXADOS E SALVOS LOCALMENTE ###
# Emissoes de Acoes Iniciais - IPO (Fonte: Site CVM)
dbCVM_IPO <- read.table("Data/IPOs.csv", header=T,
                        sep=";", stringsAsFactors = F)
dbCVM_IPO$data <- as.Date(dbCVM_IPO$data)
# Emissoes de Acoes Subsequentes (Fonte: Site CVM)
dbCVM_SUB <- read.table("Data/SUBs.csv", header=T,
                        sep=";",stringsAsFactors = F)
dbCVM_SUB$data <- as.Date(dbCVM_SUB$data)
# Emissoes de Dividas (Fonte: Site CVM)
dbCVM_DEB <- read.table("Data/DEBs.csv", header=T,
                        sep=";", stringsAsFactors = F)
dbCVM_DEB$data <- as.Date(dbCVM_DEB$data)

### Ler Dados em CSV / Load CSV Data # -----------------------------------------

## Carregando matriz de precos / Stock Prices
mPrices <- importaBaseCSV("Input/mPrices.csv", PERIOD.MON)

## Importar Valor de Mercado da Empresa / Read Market Value of the Firm
mMVfirm <- importaBaseCSV("Input/mMarketValueFirm.csv", PERIOD.MON, ignora=1)

## Importar Patrimonio Liquido / Read Book Firm
yBookFirm <- importaBaseCSV("Input/yBookFirm.csv", PERIOD.MON, formato="%Y",
                            ignora=0)

## Importar Valor de Mercado da Classe (para ponderacao)
mMVclass <- importaBaseCSV("Input/mMarketValue.csv", PERIOD.MON, ignora=1)

## Importar Volume medio em reais ultimos 12 meses
mVolume <- importaBaseCSV("Input/mVolume.csv", PERIOD.MON, ignora=1)

## Importar Indice de Negociabilidade da Bovespa / Bovespa Negociability Index
mNegociab <- importaBaseCSV("Input/mNegociabilidade.csv", PERIOD.MON)

## 2.2 Calcular Variaveis / Compute Variables # ================================

## Valor de Mercado da Classe em JUNHO
yMVclassJun <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="jun"),]

## Valor de Merdado da Classe em DEZEMBRO
yMVclassDez <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="dez"),]

## Valor de Mercado da Empresa JUNHO
yMVfirmJun <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="jun"),]

## Valor de Mercado da Empresa DEZEMBRO
yMVfirmDez <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="dez"),]

## Book-to-Market
yBM <- yBookFirm / yMVfirmDez

## Indice de Negociabilidade da Bovespa / Bovespa Negociability Index
yNegociab <- mNegociab[(months(as.Date(rownames(mNegociab)), T)=="jun"),]

## Calcular Retornos Logaritimos Mensais / Compute Logarithmic Monthly Return
pXTS     <- as.xts(mPrices)
mReturns <- as.data.frame( diff(log(pXTS), lag=1) ) ; rm(pXTS)

## Calcular Variavel Momento
yMomentum <- computeMomentum (mReturns)

## 2.2 Filtrar Amostra / Filter Sample # =======================================

f_PortfReturns  <- filterPortfReturns(mReturns, PERIOD.n, PERIOD.N)
f_NoFinancial   <- filterNoFinancial(initialSample(mPrices), "Input/dbStocks.csv")
f_MVclass       <- filterMonthMV(mMVclass, PERIOD.n, PERIOD.N)
f_MOM           <- filterMOM(mReturns, PERIOD.n, PERIOD.N)
f_MVjun         <- filterNA(mMVfirm, PERIOD.n, PERIOD.N, "jun")
f_MVdez         <- filterNA(mMVfirm, PERIOD.n, PERIOD.N, "dez")
f_BookNA        <- filterNA(yBookFirm, PERIOD.n, PERIOD.N, "dez")
f_BookPositive  <- filterGreaterThan(yBookFirm, 0, PERIOD.n, PERIOD.N, "dez")
f_IN            <- filterGreaterThan(yNegociab, 0.01, PERIOD.n, PERIOD.N, "jun")

# 
# ## Calcular Amostra Inicial
# ySample0 <- initialSample(mPrices) ; rownames(ySample0) <- rownames(yMVclassJun)
# 
# ## Filtrar de Empresas Nao Financeiras
# ySample1 <- filterNoFinancial(ySample0, "Input/dbStocks.csv")
# f_NoFinancial <- ySample1
# 
# ## Filtrar a??es com cota??es 24 meses consecutivos
# ySample2 <- filterNo24months(mPrices, ySample1)
# 
# ## Filtro Valor de Mercado em 30/06/n e 31/12/n-1
# ySample3 <- ySample2 # Cria matriz de controle da amostra a partir da ultima
# ySample3[-1,][ (yMVfirmDez <= 0) ] <- FALSE # Falso p/ valores n positivos em n-1
# ySample3[-1,][ is.na(yMVfirmDez) ] <- FALSE # Falso p/ valores invalidos em n-1
# ySample3[ (yMVfirmJun <= 0) ] <- FALSE
# ySample3[ is.na(yMVfirmJun) ] <- FALSE
# 
# ## Filtro Patrimonio Liquido
# ySample4 <- ySample3 # Cria matriz de controle da amostra a partir da ultima
# ySample4[-1,][ (yBookFirm <= 0) ] <- FALSE # Falso p/ valores n positivos em n-1
# ySample4[-1,][ is.na(yBookFirm) ] <- FALSE # Falso p/ valores invalidos em n-1
# 
# ## Filtrar Indice de Negociabilidade maior que 0,01
# ySample5 <- ySample4
# ySample5[yNegociab < 0.001] <- F ; ySample5[is.na(yNegociab)] <- F
# 
# ## Amostra Final / Final Sampe
# ySample <- ySample5
# sampleReportAll(ySample0, ySample1, ySample2, ySample3, ySample4, ySample5)
# 
# ## 2.3 Limpar Dados / Clean Data # =============================================
# 
# yMVfirmJun  <- cleanData(yMVfirmJun,  ySample)
# yMVfirmDez  <- cleanData(yMVfirmDez,  ySample, LAG=1)
# yMVclassJun <- cleanData(yMVclassJun, ySample)
# yMVclassDez <- cleanData(yMVclassDez, ySample, LAG=1)
# yVolumeJun  <- cleanData(yVolumeJun,  ySample)
# yVolumeDez  <- cleanData(yVolumeDez,  ySample, LAG=1)
# yBM         <- cleanData(yBM,         ySample, LAG=1)
# yMomentum   <- cleanData(yMomentum,   ySample)
# rownames (yMomentum) <- rownames(yVolumeJun)

## 3. INVESTOR SENTIMENT INDEX ## ##############################################
## 3. ?ndice de Sentimento
## 3.1. Ler/Calcular Proxies
## Temporalidade das Proxies: Selecionar proxies que ser?o defasadas
## 3.2. ?ndice de Sentimento n?o Ortogonalizado
## 3.3. ?ndice de Sentimento Ortogonalizado ? vari?veis macroecon?micas  

## 3.1 Read/Compute Proxies # ==================================================

## Periodo p/ Proxies Sentimento. Ex.: JAN/n a JUN/N (Ex. 1999-01/2014-06)
PERIOD.PRX <- paste(PERIOD.n,"-01/", PERIOD.N, "-06", sep="")

## Calcular NIPO # -------------------------------------------------------------
prx_NIPO <- calcularNIPO(dbCVM_IPO)

# Substituir por uma media-movel
prx_NIPO$CVM <- c(rep(mean(prx_NIPO$CVM[1:5]),5),SMA(prx_NIPO$CVM,6)[6:nrow(prx_NIPO)])
# plot(as.xts(ts(prx_NIPO$CVM, start=c(PERIOD.n,1), frequency=(12))))

## Calcular S # ----------------------------------------------------------------
prx_S <- calcularS(dbCVM_IPO, dbCVM_SUB, dbCVM_DEB)

# TRATANDO OS VALORES FALFANTES
library(TTR)
# Substituindo valores zerados pela medias dos ultimos meses
prx_S$A[3]              <- SMA(prx_S$A,   2)[prx_S$A==0][1]
prx_S$A[prx_S$A==0]     <- SMA(prx_S$A,   6)[prx_S$A==0]
prx_S$DEB[prx_S$DEB==0] <- SMA(prx_S$DEB, 6)[prx_S$DEB==0]
# http://www.fmlabs.com/reference/default.htm?url=ExpMA.htm
prx_S$Issues <- prx_S$A / ( prx_S$A + prx_S$DEB ) # Recalculando S

# Aplicando uma media movel
prx_S$Issues <- c(prx_S$Issues[1:5],SMA(prx_S$Issues,6)[-(1:5)])

# Retirando Tendencia
prx_S$Issues <- resid( lm(prx_S$Issues ~ c(1:nrow(prx_S) ) ) )

prx_S <- as.data.frame(as.xts(prx_S)[PERIOD.PRX])
# plot(as.xts(ts(prx_S$Issues, start=c(PERIOD.n,1), frequency=(12))))

## Calcular TURN - Conforme BWY (2011) # ---------------------------------------
prx_TURNv <- calcularTURN ("Input/mNegociabilidade.csv",
                           "Input/mVolFinanNoMes.csv",
                           "Input/mMarketValue.csv",
                           PERIOD.PRX, lagDetrend=4, Liq=0.01)

# plot(as.xts(ts(prx_TURNv$TURN,  start=c(PERIOD.n,1), frequency=(12))))
# plot(as.xts(ts(prx_TURNv$dTURN, start=c(PERIOD.n,1), frequency=(12))))

# # Retirando tendencia linear
# prx_TURNv$dTURN <- resid(lm( prx_TURNv$TURN ~ c(1:nrow(prx_TURNv) )) )
# plot(as.xts(ts(prx_TURNv$dTURN, start=c(PERIOD.n,1), frequency=(12))))

# ## Calcular TURN - Conforme BW (2006)
# prx_TURNq <- calcularTURNqtd ("Input/mNegociabilidade.csv",
#                               "Input/mQN.csv",
#                               "Input/mQT.csv",
#                               #"Input/mQTOutStanding.csv",
#                               PERIOD.PRX, lagDetrend=4, Liq=0.01,
#                               wins=0.1)
# plot(as.xts(ts(prx_TURNq$QN, start=c(PERIOD.n,1), frequency=(12))))
# plot(as.xts(ts(prx_TURNq$dTURN, start=c(PERIOD.n,1), frequency=(12))))

## Calcular PVOL ---------------------------------------------------------------
prx_PVOL <- calcularPVOL(periodo = PERIOD.PRX, weightType = "EW",
                         Liq=0.01, nPortf=3)
# tmp <- row.names(prx_PVOL)[1] ; tmp <- c(year(tmp),month(tmp))
# tmp <- ts(scale(prx_PVOL$PVOL), start=tmp, frequency=12)
# plot(as.xts(tmp)) ; rm(tmp)

## Calcular TRIN (ARMS) --------------------------------------------------------
PERIODO <- paste(PERIOD.n-2,"-06/",PERIOD.N,"-07", sep="") # "1998-06/2014-07"

P <- as.xts(importaBaseCSV("Input/mPrices.csv", PERIODO))
R <- as.data.frame( diff(log(P), lag=1) )
QT <- importaBaseCSV("Input/mQTOutStanding2.csv", PERIODO, ignora=1)
QN <- importaBaseCSV("Input/mQN.csv", PERIODO, ignora=1)

TRIN_High <- matrix(rep(TRUE, nrow(QT)*ncol(QT)),
                    nrow=nrow(QT), ncol=ncol(QT), dimnames=dimnames(QT))
TRIN_High[is.na(R)] <- FALSE
TRIN_High[is.na(QT)]      <- FALSE
TRIN_High[is.na(QN)]      <- FALSE
TRIN_High[mNegociab<=0.01]  <- FALSE
TRIN_Low <- TRIN_High
TRIN_High[-nrow(TRIN_High)][ (R[-1,]  < 0) ] <- FALSE
TRIN_Low[  -nrow(TRIN_Low)][ (R[-1,] >= 0) ] <- FALSE

prx_TRIN <- cbind(
    Qa = mapply( function(Q, A) sum(A, na.rm=T), as.data.frame(t(QT)), as.data.frame(t(TRIN_High)) ),
    Qb = mapply( function(Q, A) sum(A, na.rm=T), as.data.frame(t(QT)), as.data.frame(t(TRIN_Low)) ),
    Va = mapply( function(V, A) sum(V[A], na.rm=T), as.data.frame(t(QN)), as.data.frame(t(TRIN_High)) ),
    Vb = mapply( function(V, A) sum(V[A], na.rm=T), as.data.frame(t(QN)), as.data.frame(t(TRIN_Low)) ))
prx_TRIN <- as.data.frame(prx_TRIN[-1,])

# prx_TRIN$TRIN <- ( prx_TRIN$Vb / prx_TRIN$Qb ) / ( prx_TRIN$Va / prx_TRIN$Qa )
prx_TRIN$TRIN <- ( prx_TRIN$Qa / prx_TRIN$Qb ) * ( prx_TRIN$Vb / prx_TRIN$Va )

# summary(prx_TRIN$TRIN);sd(prx_TRIN$TRIN)
# prx_TRIN$TRIN <- c(prx_TRIN$TRIN[1:11],SMA(prx_TRIN$TRIN, 12)[-(1:11)])

prx_TRIN <- as.data.frame(as.xts(prx_TRIN)[PERIOD.PRX])

# plot(ts(prx_TRIN$Qa / prx_TRIN$Qb, start=c(1999,1), frequency=12))
plot(as.xts(ts(prx_TRIN$TRIN, start=c(1999,1), frequency=12)))
summary(prx_TRIN$TRIN);sd(prx_TRIN$TRIN)
head(prx_TRIN)

rm(list=c("P", "R", "QT", "QN", "TRIN_High", "TRIN_Low", "PERIODO"))

## Organizar / Importar Proxies # ----------------------------------------------
LAG <- 1
mProxies <- ts.intersect(
    NIPO      = ts(prx_NIPO$CVM ,  start=c(PERIOD.n,1), frequency=(12)),
    S         = ts(prx_S$Issues ,  start=c(PERIOD.n,1), frequency=(12)),
    TURN      = ts(prx_TURNv$dTURN, start=c(PERIOD.n,1), frequency=(12)),
    # TURN    = ts(prx_TURNq$dTURN, start=c(PERIOD.n,1), frequency=(12)),
    # QN      = ts(prx_TURNq$dQN,   start=c(PERIOD.n,1), frequency=(12)),
    PVOL      = ts(prx_PVOL$PVOL,  start=c(PERIOD.n,1), frequency=(12)),
    TRIN      = ts(prx_TRIN$TRIN,   start=c(PERIOD.n,1), frequency=(12)),
    NIPOlag   = lag(ts(prx_NIPO$CVM,   start=c(PERIOD.n,1), frequency=(12)),  -LAG),
    Slag      = lag(ts(prx_S$Issues,   start=c(PERIOD.n,1), frequency=(12)),  -LAG),
    TURNlag   = lag(ts(prx_TURNv$dTURN, start=c(PERIOD.n,1), frequency=(12)), -LAG),
    # TURNlag = lag(ts(prx_TURNq$dTURN, start=c(PERIOD.n,1), frequency=(12)), -LAG),
    # QNlag   = lag(ts(prx_TURNq$dQN,   start=c(PERIOD.n,1), frequency=(12)), -LAG),
    PVOLlag   = lag(ts(prx_PVOL$PVOL,  start=c(PERIOD.n,1), frequency=(12)),  -LAG),
    TRINlag   = lag(ts(prx_TRIN$TRIN,  start=c(PERIOD.n,1), frequency=(12)),  -LAG),
    dframe = T)
row.names(mProxies) <- as.character(as.Date(index(as.xts(mProxies[[1]]))))

plot(ts(mProxies[,1:5], start=start(mProxies[[1]]), frequency=12),
     main="Proxies")

## Correlations
as.dist(round(cor(mProxies, use="na.or.complete"),2))

## 3.2 First Step # ============================================================
# Estimating first component of all proxies and their lags and choose the best
PCAstep1 <- prcomp(mProxies, scale=T, center = TRUE)

abs(round(cor(PCAstep1$x[,"PC1"],mProxies),2))       # The correlations
mBestProxies <- chooseLAG(mProxies)                  # Choosing LAGs...
colnames(mBestProxies)                               # Best proxies

tmp <- round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)
if (sum(tmp < 0) > sum(tmp > 0)) { mBestProxies <- mBestProxies * (-1) }
rm(tmp)

round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)        # Correlation with PC1
as.dist(round(cor(mBestProxies),2))                  # Correlations between them

## 3.3 Second Step # ===========================================================

## Estimating first component of the best proxies
PCAstep2 <-prcomp(mBestProxies, scale=T, center = TRUE)

## Correlation with PC1 of the 1? step
abs(cor(PCAstep1$x[,"PC1"],PCAstep2$x[,"PC1"]))

# ## Proportion of Variance
summary(PCAstep2)

## Not orthogonalized index (osb.: not important)
## Inverter sinal se tiver mais proxies negativas
tmp <- PCAstep2$rotation[,"PC1"]
if ( sum(tmp < 0) > sum(tmp > 0) ) {
    PCAstep2$rotation[,"PC1"] <- PCAstep2$rotation[,"PC1"] * (-1)
} ; rm(tmp)
PCAstep2$rotation[,"PC1"]

## 3.4 Third Step # ============================================================
# Estimate orthogonilized proxies by the regression all raw proxies

## PIB (% Change)
## https://www.quandl.com/BCB/4380-GDP-monthly-current-prices-R-million
data_inicial <- as.Date(row.names(mBestProxies)[1])
PIB <- Quandl("BCB/4380",
              trim_start=data_inicial, trim_end="2014-06-30",
              transformation="rdiff",
              sort="asc")

## OECD Dummy (Recession)
## https://www.quandl.com/FRED/BRARECM
data_inicial <- as.Date(row.names(mBestProxies)[1])
RECESS <- Quandl("FRED/BRARECM",
                 trim_start=data_inicial, trim_end="2014-06-30", sort="asc")

mMacroeconomics <- data.frame(PIB=PIB$Value, RECESS=RECESS$Value,
                              row.names=PIB$Date)

#rm(list = c("PIB", "RECESS", "data_inicial"))

# Estimando Proxies Ortogonalizada
mProxiesOrtog <- ortogonalizeProxies(mBestProxies, mMacroeconomics)

# ## Plotar todas as mProxiesOrtog
tmp <- row.names(mProxiesOrtog)[1] ; tmp <- c(year(tmp),month(tmp))
plot(ts(mProxiesOrtog, start=tmp, frequency=12))

# Estimando Componentes Principais da Terceira Etapa
PCAstep3 <-prcomp(mProxiesOrtog, scale.=T, center = TRUE)

# Verificando correlacao com o primeiro indice
abs(cor(PCAstep2$x[,"PC1"],PCAstep3$x[,"PC1"]))

# Percentual explicado da variancia
summary(PCAstep3)

# Scree plot of eigenvalues
screeplot(PCAstep3, type="line", main="Scree Plot Indice de Sentimento")

PCAstep3$rotation[,"PC1"] # Equacao do Indice de Sent. Ortogonalizado

## Salvando Variavel
tmp <- c(year(row.names(mProxiesOrtog)[1]), month(row.names(mProxiesOrtog)[1]))
Sentiment <- ts(PCAstep3$x[,"PC1"], start=tmp, frequency=12)
SentNO    <- ts(PCAstep2$x[,"PC1"], start=tmp, frequency=12)

## Plotar Sentimento e SentimentoNO (N?o Ortogonalizado)
plot(SentNO*-1, col="dark red", lty="dashed")
lines(Sentiment, col="blue")
abline(h = 0, lty = 3, col="gray")
abline(h = median(Sentiment), lty = 3)

plot(as.xts(Sentiment), main="Indice de Sentimento")

IBV  <- Quandl("BCB/7845", type="ts", collapse="monthly", sort="asc",
               #               transformation="rdiff",
               trim_start=row.names(mProxiesOrtog)[1],
               trim_end=row.names(mProxiesOrtog)[nrow(mProxiesOrtog)])

## Plotar Sentimento e IBOVESPA (centralizado em zero e variancia unica)
tmp <- c(year(row.names(mProxiesOrtog)[1]), month(row.names(mProxiesOrtog)[1]))
Sent <- ts(scale(PCAstep3$x[,"PC1"]), start=tmp, frequency=12)
IBV  <- ts(scale(IBV), start=tmp, frequency=12)

plot(Sent, col="dark red", type="l", main="ISI e IBOVESPA")
lines(IBV, col="green")
lines(scale(SentNO*-1), col="dark red", lty="dashed")

## Vers?es Alternativas do Indice Conforme Literatura # =======================

## Sentiment YearEnd
# Regredir (JUL a JUN) com Sentimento do final do ano anterior
# conforme BW 2006 e BWY 2012
ep <- which(month(row.names(mProxiesOrtog))==12)
tmp <- PCAstep3$x[,"PC1"][ep]
SentYEm <- ts(tmp[sort(rep(1:length(tmp),12))],
              start=c(year(names(tmp)[1]),month(names(tmp)[1])+1), frequency=12)
SentYEm <- Sentiment[c(1,endpoints(Sentiment, on="months", k=12)+1)]
SentYEm <- ts(SentYEm[sort(rep(1:length(SentYEm)-1,12))], start=c(2000,7),
              end=end(Sentiment), frequency=12)

## Sentiment Last June
# Regredir (JUL a JUN) com Sentimento do junho do ano anterior
ep <- which(month(row.names(mProxiesOrtog))==6)
tmp <- PCAstep3$x[,"PC1"][ep]
SentLJ <- ts(tmp[sort(rep(1:length(tmp),12))],
             start=c(year(names(tmp)[1]),month(names(tmp)[1])+1),
             end=end(Sentiment), frequency=12)

# round(Sentiment,2)
# round(SentYEm,  2)
# round(SentLJ,   2)

# # Sent <- Sentiment # Sentiment Monthly
# # Sent <- SentQ
# # Sent <- SentYEm
# # Sent <- SentLJm
# # Sent <- SentYEq
# # Sent <- SentLJq

## 4. PRICING MODELS ## #######################################################
## 5.1 Serie do Retorno do Ativos Livre de Risco
## 5.2 Serie do Retorno da Carteira de Mercado
## 5.3 Construir Carteiras por Fatores
## 5.3 Fator Tamanho (Intera??o Tamanho e BM)
## 5.4 Fator BM      (Intera??o BM e Tamanho)
## 5.5 Serie de retorno dos demais fatores (MOM, LIQ)
##       Fator Momento
##       Fator Liquidez

## 4.1 Ativos Livre de Risco # =================================================
## Ativo Livre de Risco (download da Serie SELIC do Banco Central)
Rf <- riskFreeRate(PERIOD.n, PERIOD.N)

## 4.2 Carteira de Mercado # ===================================================

## Carteira de Mercado (Calculada)

# Filtro p/ calculo da carteira de Mercado
f_MKT <- data.frame(lapply(f_PortfReturns * f_MVclass * f_NoFinancial,
                           as.logical), row.names=rownames(f_MVclass))

# Transformando a classe do valor de mercado de integer p/ numerico
# por causa da memoria
mMVclass <- data.frame(lapply(mMVclass, as.numeric ), row.names=rownames(mMVclass))

tmp <- computeMarketPortfolio(mReturns, mMVclass, f_MKT) ; rm(f_MKT)
MKT <- ts(tmp$rVW, start=c(PERIOD.n,07), frequency=12) ; rm(tmp)

## Baixando Carteira de Mercado (Ibovespa) p/ COMPARA??O
IBV  <- Quandl("BCB/7845", type="ts", collapse="monthly", sort="asc",
               #               transformation="rdiff",
               trim_start=as.Date(paste(PERIOD.n+1,"-06-01", sep="")),
               trim_end=as.Date(paste(PERIOD.N,"-06-01", sep="")))
IBV <- diff(log(IBV),1)

#' Correla??o entre a carteira calculada e o IBOVESPA
#' Com empresas Financeiras = 0.96
as.dist(cor(merge(MKT=as.zoo(MKT), IBOV=as.zoo(IBV), all=F)))

MKT <- MKT-Rf
mean(MKT)*100

## 4.2 Fatores de Risco # ======================================================

## TAMANHO  (VM Empresa Jun)
mMVfirm <- importaBaseCSV("Input/mMarketValueFirm.csv", PERIOD.MON, ignora=1)
yMVfirmJun <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="jun"),]
yMVfirmJun <- cleanData(yMVfirmJun, f_PortfReturns)
yMVfirmJun <- cleanData(yMVfirmJun, f_MVclass)
yMVfirmJun <- cleanData(yMVfirmJun, f_NoFinancial)

## BM
mMVfirm    <- importaBaseCSV("Input/mMarketValueFirm.csv", PERIOD.MON, ignora=1)
yMVfirmDez <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="dez"),]
yMVfirmDez <- cleanData(yMVfirmDez, f_PortfReturns, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_NoFinancial, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_MVclass, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_BookNA, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_BookPositive, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_MVdez, LAG=1)
yBookFirm <- importaBaseCSV("Input/yBookFirm.csv", PERIOD.MON, formato="%Y",
                            ignora=0)
yBookFirm  <- cleanData(yBookFirm,  f_PortfReturns, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_NoFinancial, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_MVclass, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_BookNA, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_BookPositive, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_MVdez, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_MVdez, LAG=1)

yBM <- yBookFirm / yMVfirmDez

## MOMENTO
yMomentum <- computeMomentum (mReturns)
rownames(yMomentum) <- sub("-05", "-06", rownames(yMomentum))
yMomentum <- cleanData(yMomentum, f_PortfReturns)
yMomentum <- cleanData(yMomentum, f_MVclass)
yMomentum <- cleanData(yMomentum, f_NoFinancial)

# yMVfirmJun <- cleanData(yMVfirmJun, f_IN)
# yBM        <- cleanData(yBM, f_IN, LAG=1)
# yMomentum  <- cleanData(yMomentum, f_IN)

## Carteiras por Fator (aF: assets Factors)
aF_Size_S <- portfolioSelectAssets(yMVfirmJun,2,1) * 1 # Small
aF_Size_B <- portfolioSelectAssets(yMVfirmJun,2,2) * 1 # Big   

aF_BM_H <- portfolioSelectAssets(yBM,3,1) * 1 # Value  (High BM)
aF_BM_N <- portfolioSelectAssets(yBM,3,2) * 1 # Neutral
aF_BM_L <- portfolioSelectAssets(yBM,3,3) * 1 # Growth (Low BM)

aF_MOM_W <- portfolioSelectAssets(yMomentum,3,1) * 1 # Wins
aF_MOM_L <- portfolioSelectAssets(yMomentum,3,3) * 1 # Loss

## Carteiras a partir da Intera??o
##
## Conforme French Site
## http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
##
aF_SH <- aF_Size_S[-1,] * aF_BM_H # Small Value (High BM)
aF_SN <- aF_Size_S[-1,] * aF_BM_N # Small Neutral
aF_SL <- aF_Size_S[-1,] * aF_BM_L # Small Growth (Low BM)
aF_BH <- aF_Size_B[-1,] * aF_BM_H # Big Value (High BM)
aF_BN <- aF_Size_B[-1,] * aF_BM_N # Big Neutral
aF_BL <- aF_Size_B[-1,] * aF_BM_L # Big Growth (Low BM)
aF_SH <- data.frame(lapply(aF_SH, as.logical), row.names=rownames(yBM))
aF_SH <- data.frame(lapply(aF_SH, as.logical), row.names=rownames(yBM)) # Small Value (High BM)
aF_SN <- data.frame(lapply(aF_SN, as.logical), row.names=rownames(yBM)) # Small Neutral
aF_SL <- data.frame(lapply(aF_SL, as.logical), row.names=rownames(yBM)) # Small Growth (Low BM)
aF_BH <- data.frame(lapply(aF_BH, as.logical), row.names=rownames(yBM)) # Big Value (High BM)
aF_BN <- data.frame(lapply(aF_BN, as.logical), row.names=rownames(yBM)) # Big Neutral
aF_BL <- data.frame(lapply(aF_BL, as.logical), row.names=rownames(yBM)) # Big Growth (Low BM)
aF_SWI <- (aF_Size_S * aF_MOM_W)[-1,] # Small Win
aF_SLO <- (aF_Size_S * aF_MOM_L)[-1,] # Small Los
aF_BWI <- (aF_Size_B * aF_MOM_W)[-1,] # Big Win
aF_BLO <- (aF_Size_B * aF_MOM_L)[-1,] # Big Los
aF_SWI <- data.frame(lapply(aF_SWI, as.logical), row.names=rownames(yBM))
aF_SLO <- data.frame(lapply(aF_SLO, as.logical), row.names=rownames(yBM))
aF_BWI <- data.frame(lapply(aF_BWI, as.logical), row.names=rownames(yBM))
aF_BLO <- data.frame(lapply(aF_BLO, as.logical), row.names=rownames(yBM))

## Retornos
portF_SH  <- portfolioSerie(mReturns, mMVclass, aF_SH)
portF_SN  <- portfolioSerie(mReturns, mMVclass, aF_SN)
portF_SL  <- portfolioSerie(mReturns, mMVclass, aF_SL)
portF_BH  <- portfolioSerie(mReturns, mMVclass, aF_BH)
portF_BN  <- portfolioSerie(mReturns, mMVclass, aF_BN)
portF_BL  <- portfolioSerie(mReturns, mMVclass, aF_BL)
portF_SWI <- portfolioSerie(mReturns, mMVclass, aF_SWI)
portF_SLO <- portfolioSerie(mReturns, mMVclass, aF_SLO)
portF_BWI <- portfolioSerie(mReturns, mMVclass, aF_BWI)
portF_BLO <- portfolioSerie(mReturns, mMVclass, aF_BLO)
rm(list=ls(pattern = "aF_"))

## Fatores de Risco # ----------------------------------------------------------

## FATOR TAMANHO (SMB)
##
## SMB = 1/3 (Small Value + Small Neutral + Small Growth)
##       - 1/3 (Big Value + Big Neutral + Big Growth)
##
SMB <-(portF_SH$rVW+portF_SN$rVW+portF_SL$rVW)/3-(portF_BH$rVW+portF_BN$rVW+portF_BL$rVW)/3
SMB <-ts(SMB, start=c(PERIOD.n+1,7), frequency=12)

## FATOR BM (HML)
##
## HML = 1/2 (Small Value + Big Value)   - 1/2 (Small Growth + Big Growth)
##
HML <- (portF_SH$rVW + portF_BH$rVW)/2 - (portF_SL$rVW + portF_BL$rVW)/2
HML <- ts(HML, start=c(PERIOD.n+1,7), frequency=12)

## FATOR MOMENTO (UMD)
##
## Mom =    1/2 (Small High + Big High) - 1/2(Small Low + Big Low).	
##
UMD <- (portF_SWI$rVW + portF_BWI$rVW)/2 - (portF_SLO$rVW + portF_BLO$rVW)/2
UMD <- ts(UMD, start=c(PERIOD.n+1,7), frequency=12)

# ## Pequeno Teste dos Fatores
# summary(lm(portF_SH$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_SN$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_SL$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_BH$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_BN$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_BL$rVW-Rf ~ MKT + SMB + HML))
# 
# NW(lm(portF_SH$rVW-Rf ~ MKT + SMB + HML + UMD))
# NW(lm(portF_SN$rVW-Rf ~ MKT + SMB + HML + UMD))
# NW(lm(portF_SL$rVW-Rf ~ MKT + SMB + HML + UMD))
# NW(lm(portF_BH$rVW-Rf ~ MKT + SMB + HML + UMD))
# NW(lm(portF_BN$rVW-Rf ~ MKT + SMB + HML + UMD))
# NW(lm(portF_BL$rVW-Rf ~ MKT + SMB + HML + UMD))

## Salvando fatores
CAPM <- ts.intersect(MKT)
FF3F <- ts.intersect(MKT, SMB, HML)
CA4F <- ts.intersect(MKT, SMB, HML, UMD)

## 5. CONSTRUCT PORTFOLIOS ## ##################################################
## 5.1 QUINTIS
## 5.2 DECIS
## 5.1 BW  Portfolios
## 5.2 SYY Portfolios

# (...) 5.R


summary(PCAstep3)
PCAstep3$rotation[,"PC1"] # Equacao do Indice de Sent. Ortogonalizado

reportAvaregeVW("LongShort", Sentiment, 1)
reportAvaregeVW("Short", Sentiment, 1)
reportAvaregeVW("Long", Sentiment, 1)

reportAvaregeVW("LongShort", Sentiment, 1, FF3F)
reportAvaregeVW("Short", Sentiment, 1, FF3F)
reportAvaregeVW("Long", Sentiment, 1, FF3F)

x <- reportRegVW(Sentiment, LAG, FF3F) ; x[x[,9]<=0.1,] # LongShort
x[x[,6]<=0.1,]                                          # Short
x[x[,3]<=0.1,]                                          # Long

reportAvaregeEW("LongShort", Sentiment, 1)
reportAvaregeEW("Short", Sentiment, 1)
reportAvaregeEW("Long", Sentiment, 1)

reportAvaregeEW("LongShort", Sentiment, 1, FF3F)
reportAvaregeEW("Short", Sentiment, 1, FF3F)
reportAvaregeEW("Long", Sentiment, 1, FF3F)

x <- reportRegEW(Sentiment*-1, LAG, FF3F) ; x[x[,9]<=0.1,] # LongShort
x[x[,6]<=0.1,]                                          # Short
x[x[,3]<=0.1,]                                          # Long

# reportRegEW(Sentiment, 1, FF3F)
# reportRegVW(Sentiment, 1, FF3F)
# reportRegEW(SentYEm, 0, FF3F)
# reportRegVW(SentYEm, 0, FF3F)
# reportRegEW(SentLJ, 0, FF3F)
# reportRegVW(SentLJ, 0, FF3F)

## Combinadas apenas as anomalias com Alpha significante
# Verificar as que s?o significantes
unlist(lapply(LSew, function (x) NW(dynlm(x$LS ~ + FF3F))[1,4]<0.01))
ls_COMBew$LONG  <- (ls_BMew$LONG   + ls_MOMew$LONG  + ls_VOLew$LONG  + ls_LPew$LONG  + ls_EBTDAew$LONG  + ls_GSew$LONG  + ls_ROAew$LONG  + ls_AGew$LONG  + ls_INVew$LONG ) / 9
ls_COMBew$SHORT <- (ls_BMew$SHORT  + ls_MOMew$SHORT + ls_VOLew$SHORT + ls_LPew$SHORT + ls_EBTDAew$SHORT + ls_GSew$SHORT + ls_ROAew$SHORT + ls_AGew$SHORT + ls_INVew$SHORT) / 9
ls_COMBew$LS    <- ls_COMBew$LONG - ls_COMBew$SHORT
NW(dynlm(ls_COMBew$LS ~ FF3F))

# Verificar as que s?o significantes
unlist(lapply(LSvw, function (x) NW(dynlm(x$LS ~ + FF3F))[1,4]<0.01))
ls_COMBvw$LONG  <- (ls_TAMvw$LONG  + ls_BMvw$LONG   + ls_VOLvw$LONG  + ls_LPvw$LONG  + ls_ENDIVvw$LONG  + ls_ROAvw$LONG ) / 6
ls_COMBvw$SHORT <- (ls_TAMvw$SHORT + ls_BMvw$SHORT  + ls_VOLvw$SHORT + ls_LPvw$SHORT + ls_ENDIVvw$SHORT + ls_ROAvw$SHORT) / 6
ls_COMBvw$LS    <- ls_COMBvw$LONG - ls_COMBvw$SHORT
NW(dynlm(ls_COMBvw$LS ~ FF3F))
LSew <- list(ls_TAMew, ls_BMew ,ls_MOMew, ls_VOLew, ls_LIQew, ls_LPew, ls_EBTDAew, ls_ENDIVew, ls_GSew, ls_ROAew, ls_AGew, ls_INVew)
LSvw <- list(ls_TAMvw, ls_BMew ,ls_MOMvw, ls_VOLvw, ls_LIQvw, ls_LPvw, ls_EBTDAvw, ls_ENDIVvw, ls_GSvw, ls_ROAvw, ls_AGvw, ls_INVew)
names(LSew) <- c("TAM", "BM" ,"MOM", "VOL", "LIQ", "LP", "EBTDA", "ENDIV", "GS", "ROA", "AG", "INV")
names(LSvw) <- c("TAM", "BM" ,"MOM", "VOL", "LIQ", "LP", "EBTDA", "ENDIV", "GS", "ROA", "AG", "INV")


# ls_BM <- computeLongShort(mReturns, mMVclass, yBM, 10, 1, 10, Rf) # SYY
# ls_BM  <- computeLongShort(mReturns, mMVclass, yBM, 10, 1, 5,  Rf)  # BW Mid - Low
# ls_BM  <- computeLongShort(mReturns, mMVclass, yBM, 10, 6, 10, Rf)  # BW High - Mid


# summary(dynlm(ls_COMBew$LONG ~ FF3F))
# LSew <- list(ls_TAMew, ls_BMew ,ls_MOMew, ls_VOLew, ls_LIQew, ls_LPew, ls_EBTDAew, ls_ENDIVew, ls_ROAew, ls_AGew, ls_INVew)
# LSvw <- list(ls_TAMvw, ls_BMew ,ls_MOMvw, ls_VOLvw, ls_LIQvw, ls_LPvw, ls_EBTDAvw, ls_ENDIVvw, ls_ROAvw, ls_AGvw, ls_INVew)
# 
# LS$LONG  <- (ls_TAM$LONG + ls_LIQ$LONG + ls_VOL$LONG + ls_BM$LONG + ls_MOM$LONG +
#                  ls_EBTDA$LONG + ls_ENDIV$LONG + ls_LP$LONG + ls_ROA$LONG) / 9
# LS$SHORT <- (ls_TAM$SHORT + ls_LIQ$SHORT + ls_VOL$SHORT + ls_BM$SHORT + ls_MOM$SHORT +
#                  ls_EBTDA$SHORT + ls_ENDIV$SHORT + ls_LP$SHORT + ls_ROA$SHORT) / 9

# CORRELACAO DAS ESTRATEGIAS
round(as.dist(cor(data.frame(TAM   = (ls_TAM$LONG - ls_TAM$SHORT),
                             LIQ   = (ls_LIQ$LONG - ls_LIQ$SHORT),
                             VOL   = (ls_VOL$LONG - ls_VOL$SHORT),
                             BM    = (ls_BM$LONG - ls_BM$SHORT),
                             MOM   = (ls_MOM$LONG - ls_MOM$SHORT),
                             EBTDA = (ls_EBTDA$LONG - ls_EBTDA$SHORT),
                             ENDIV = (ls_ENDIV$LONG - ls_ENDIV$SHORT),
                             LP    = (ls_LP$LONG - ls_LP$SHORT),
                             ROA   = (ls_ROA$LONG - ls_ROA$SHORT),
                             TODAS = (LS$LONG - LS$SHORT)))),2)

## 6. INVESTOR SENTIMENT AND ANOMALIES ## ######################################
##    Sentimento do Investidor e Anomalias
##
## 6.1. An?lise das M?dias ap?s per?odos de Sentimento Alto e Baixo
## 6.2. Modelos Econom?tricos
## 6.2.1 Extremos e sentimento defasado
## 6.2.2 Extremos, sentimeto defasado e fatores de risco
## 6.2.3 Extremos, dummys

#== 6.1 An?lise de M?dias # ====================================================

## LONG-SHORT (Verificar se a Anomalia ? mais forte ap?s Alto Sentimento)
# H .:. As anomalias s?o mais fortes ap?s alto sentimento do que baixo
reportAvarege("LongShort", Sent, 12)

## SHORT (Espera-se que seja ainda mais negativo ap?s alto sentimento)
# H .:. Short ? mais baixa (lucrativa) apos alto sentimento do que baixo ( H-L < 0 )
reportAvaregeEW("Short", Sentiment, 12)
reportAvaregeVW("Long", Sentiment, 12)

## LONG (Espera-se que o sentimento nao tenha efeito)
reportAvaregeVW("Long", Sent, 1)

# computeAvarageReturns(ls_TAM, Sentiment, 1)
# computeAvarageReturns(ls_LIQ, Sentiment, 1)
# computeAvarageReturns(ls_VOL, Sentiment, 1)
# computeAvarageReturns(ls_BM , Sentiment, 1)
# computeAvarageReturns(ls_MOM, Sentiment, 1)
# computeAvarageReturns(ls_EBTDA, Sentiment, 1)
# computeAvarageReturns(ls_ENDIV, Sentiment, 1)
# computeAvarageReturns(ls_LP , Sentiment, 1)
# computeAvarageReturns(ls_ROA, Sentiment, 1)
# computeAvarageReturns(LS, Sentiment, 1)

#== 6.2 Predictive Regressions # ===============================================
## model1, modelCAPM, model3F, model4F e model5F
##

reportRegSent(Sent, 1) ## Sentiment and Returns
reportRegCAPM(Sent, 1) ## CAPM
reportReg3F  (Sent, 1) ## FF1993
reportReg4F  (Sent, 1) ## MOMENTO

#== 6.2.3 Extremos, dummys # ===================================================

reportRegDummy("Long", Sentiment, 2)
reportRegDummy("Short", Sentiment, 2)
reportRegDummy("LongShort", Sentiment, 3)

# save.image(paste(getwd(),"/Data/", "20141027_FINAL.RData", sep=""))
# save.image(paste(getwd(),"/Data/", format(Sys.Date(), "%Y%m%d"),
#               "_", format(Sys.time(),"%H%M%S"), ".RData", sep=""))

## INDICE JA TENHO OS DADOS
## PORTFOLIOS
# 
# > x <- NW(dynlm(LS$LONG ~ 0 + dH + FF3F))
# > x
# 
# t test of coefficients:
#     
#     Estimate Std. Error t value  Pr(>|t|)    
# dH      -0.0142685  0.0050717 -2.8134 0.0055297 ** 
#     FF3FMKT  0.9513938  0.0688293 13.8225 < 2.2e-16 ***
#     FF3FSMB  0.7245735  0.1588831  4.5604 1.022e-05 ***
#     FF3FHML -0.3041637  0.0762176 -3.9907 0.0001008 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# H <- dynlm(LS$LONG ~ 0 + dH + FF3F)
# H <- NW(H)["dH","Estimate"]+residuals(H)
# 
# residuals(x)
# L <- dynlm(LS$LONG ~ 0 + dL + FF3F)
# NW(L)["dH","Estimate"]
# NW(L)["dH","Estimate"]+residuals(L)+residuals(x)
# x$coefficients["dH","t value"]
# x$coefficients["dH","Pr(>|t|)"]
# 
# x$coefficients["dH","Estimate"]+residuals(x) - x$coefficients["dH","Estimate"]-residuals(x)