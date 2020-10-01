

# #GET THE DATA (NON LANCIARE QUESTA PARTE DEL CODICE)
# ######
# # Grazie alla libreria alphavantager possiamo scaricare i dati delle serie storiche 
# library(alphavantager)
# av_api_key("YOUR_API_KEY")
# 
# 
# #MICROSOFT (IT)
# 
# MSFT <- av_get(symbol     = "MSFT", #con la funzione av_get otteniamo i dati (TIME_sERIES_DAILY = serie storica giornaliera)
#           av_fun     = "TIME_SERIES_DAILY", 
#           outputsize = "full")[,c(1,5)] # Estraiamo da av_get solamente le colonne timestamp e "close"
# 
# len <- (nrow(MSFT)-365):nrow(MSFT)
# MSFT <- MSFT[len,]
# names(MSFT)[2] <- "MSFT" #Diamo un nome alla colonna del prezzo di chiusura
#  
# #PFIZER (Farmaci e Health Care)
# PFE <- av_get(symbol     = "PFE", 
#                av_fun     = "TIME_SERIES_DAILY", 
#                outputsize = "full")[,c(1,5)]
# 
# len <- (nrow(PFE)-365):nrow(PFE)
# PFE <- PFE[len,]
# 
# names(PFE)[2] <- "PFE"
# 
# # LOCKHEED MARTIN CORP (Industria pesante militare)
# LMT <- av_get(symbol     = "LMT", 
#               av_fun     = "TIME_SERIES_DAILY", 
#               outputsize = "full")[,c(1,5)]
# 
# len <- (nrow(LMT)-365):nrow(LMT)
# LMT <- LMT[len,]
# 
# names(LMT)[2] <- "LMT"
# 
# # UNITED TECHNOLOGIES CORPORATION 
# UTX <- av_get(symbol     = "UTX", 
#               av_fun     = "TIME_SERIES_DAILY", 
#               outputsize = "full")[,c(1,5)]
# 
# len <- (nrow(UTX)-365):nrow(UTX)
# UTX <- UTX[len,]
# 
# names(UTX)[2] <- "UTX"
# 
# # ADVANCED MICRO DEVICE (PRODUZIONE CPU)
# AMD <- av_get(symbol     = "AMD", 
#               av_fun     = "TIME_SERIES_DAILY", 
#               outputsize = "full")[,c(1,5)]
# 
# len <- (nrow(AMD)-365):nrow(AMD)
# AMD <- AMD[len,]
# 
# names(AMD)[2] <- "AMD"
# 
# # newmont mining company (miniera oro e metalli)
# NEM <- av_get(symbol     = "NEM", 
#               av_fun     = "TIME_SERIES_DAILY", 
#               outputsize = "full")[,c(1,5)]
# 
# 
# len <- (nrow(NEM)-365):nrow(NEM)
# NEM <- NEM[len,]
# 
# names(NEM)[2] <- "NEM"
# 
# 
# timestamp <- AMD[,1]
# 
# data <- cbind(AMD[,c(1,2)],LMT[,2],MSFT[,2],NEM[,2],PFE[,2],UTX[,2]) #Cbind unisce le colonne in un solo data.frame
# rm(len)
# 
# ######


# RUNNARE DA QUI

load(".Rdata")

# CALCOLI

calc <- function(data,up.tar=0.5,lo.tar=0.005){


target <- seq(lo.tar,up.tar,by=0.005)  # Crea la sequenza di rendimenti target 
  

R <- diff(as.matrix(log(data[,2:ncol(data)]))) #Rendimenti daily logaritmici 


e.r <- matrix(apply(R,2,mean)) #Vettore dei rendimenti attesi


s.d <-matrix(apply(R,2,sd)) # Deviazione standard rendimenti


Sig <- cov(R) #Matrice varianze covarianze (S nella parte teorica allegata)


S.inv <- solve(Sig)   #Inversa della Matrice delle varianze e covarianze


unit <- matrix(rep(1,length(e.r))) #Vettore unitario di lunghezza pari al numero di STOCK




#Coefficienti del libro
A<-t(e.r)%*%S.inv%*%e.r
B<-t(e.r)%*%S.inv%*%unit
C<-t(unit)%*%S.inv%*%unit


#res <- matrix(0,ncol=ncol(data)-1,nrow=length(target))

all.w <- list() #Lista vuota ove confluiranno i risultati del for loop
var.vec <- vector("numeric",length=length(target))  #vettore vuoto dove confluiranno i risultati del for loop
  
for (i in 1:length(target)){

m <- target[i] #Rendimento target

# CALCOLO DEI PARAMETRI PER OTTENERE IL VETTORE DEI PESI W* CHE MIN LA VARIANZA A PARITà DI REND ATTESO
num <- e.r%*%(m*C-B)+unit%*%(A-m*B)                #Numeratore della formula per il calcolo del vettore dei pesi 
den <- A*C - B^2                                   #Denominatore
w.star <- S.inv%*%num%*%(1/den)    #Vettore dei pesi
#w.star
var.star <- t(w.star)%*%Sig%*%w.star #Calcoliamo la varianza del portafoglio dati i pesi ottimali w*
#var.star
res <- list("weights" =w.star,
            "variance"=var.star)

all.w[[i]] <- res # Conterrà i pesi ottimali e la varianza per ogni possibile rendimento target
var.vec[i] <- var.star #Conterrà solo la varianza per ogni possibile rendimento target


#t(w.star)%*%e.r                               #prova che il vettore ottimale dei pesi trasposto, moltiplicato per i rendimenti attesi, è pari a 0.01, cioè il rendimento target
#sum(w.star)                                 #prova che la somma dei pesi è 1
#t(w.star)%*%unit                             #prova che la somma dei pesi è 1

}

plot.data <- cbind(target,var.vec) #Creare dataframe da cui plottare grafico

names(all.w) <- as.character(target) #Associare nomi alla lista

invisible(list("all.w"=all.w,
               "plot"=plot.data))


}

output <- calc(data)


plot(x=output[["plot"]][,2],y=output[["plot"]][,1], type="l", xlab="Min. Varian",
     ylab="Target Return")






