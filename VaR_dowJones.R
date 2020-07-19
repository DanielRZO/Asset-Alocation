################################
## VaR dos titulos Down Jones  #
##                             #
################################

# 1 eliminar todo o conteudo dados, variveis...
rm(list = ls())

# 2 setar a pasta onde queremos trabalhar (modificar o enderezo)
setwd("C://Users//Daniel//Documents//MESTRADO//III SEMESTRE//ASSET")

## Baixo os pacotes que preciso no trabalho
# install.packages('quantmod')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages("ggthemes")

## Cargo as librarias que preciso no trabalho
library(quantmod)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(moments)
library(reshape2)

## Carregamento dos dados

## Carrego o indice dowJones  (pode ser qualquer base de dados)
Data_Ind_orig <- read.csv("DownJonesIndex_2019-03-30.csv",sep = ",",dec = ".")
Data_Ind <- as.matrix(Data_Ind_orig[,2])

## Cargo os componentes dos indice
Data_orig <- read.csv("DownJones_2019-03-30.csv",sep = ",",dec = ".")
end <- NCOL(Data_orig)
Data <- as.matrix(Data_orig[,2:end])
Date <-  as.Date(Data_orig[,1],"%Y-%m-%d")

# Numeros de componentes do portfolio
nr_componentes <- NCOL(Data)

# Numeros de observacao do portfolio
nobs <- NROW(Data)
nobs_start <- 1


# Pesos do portfolio
#um <- matrix(1,nrow = nr_componentes, ncol= 1)
#W <- um/nr_componentes

# Portfolio
#V <- Data%*%W

R <- log(Data[(nobs_start+1):nobs,]/Data[nobs_start:(nobs-1),])
R_date <- cbind.data.frame(Date=Date[nobs_start:(nobs-1)],R)

RI <- log(Data_Ind[(nobs_start+1):nobs,]/Data_Ind[nobs_start:(nobs-1),])
RI_date <- cbind.data.frame(Date=Date[nobs_start:(nobs-1)],RI)

################
## VaR Indice
################
alpha <-  0.99
VaR_1_I <- NULL

for (k in 1:240){
  VaR_1_I[k] <- -k*mean(RI)-sqrt(k*var(RI))*qnorm(1-alpha)
}

VaR_1_I <- cbind(dia=seq(1,240),VaR_1_I)

ggplot(as.data.frame(VaR_1_I),aes(dia,VaR_1_I))+
  geom_line(color="blue")
  
  
#####################
## VaR Componentes 
#####################
alpha <-  0.99
VaR_1<- matrix(0,ncol=nr_componentes,nrow=240)  
  for(k in 1:240){
    for(c in 1:nr_componentes){
      VaR_1[k,c] <- -k*mean(R[,c])-sqrt(k*var(R[,c]))*qnorm(1-alpha)
    }
  }

VaR_1 <- cbind(seq(1,240),VaR_1)
colnames(VaR_1) <- c("dia",colnames(Data))
VaR_1 <- as.data.frame(VaR_1)

VaR_1_melt = melt(VaR_1, id.vars="dia")
ggplot(data=VaR_1_melt, aes(x=dia, y=value, group=variable,color=variable)) + 
  geom_line()

#####################################
## VaR Indice para diferentes alpha 
######################################
alpha <-  0.99
VaR_1_I <- NULL

for (k in 1:240){
  VaR_1_I[k] <- -k*mean(RI)-sqrt(k*var(RI))*qnorm(1-alpha)
}

alpha <-  0.95
VaR_5_I <- NULL

for (k in 1:240){
  VaR_5_I[k] <- -k*mean(RI)-sqrt(k*var(RI))*qnorm(1-alpha)
}


alpha <-  0.90
VaR_10_I <- NULL

for (k in 1:240){
  VaR_10_I[k] <- -k*mean(RI)-sqrt(k*var(RI))*qnorm(1-alpha)
}

VaR_alpha <- as.data.frame(cbind(dia=seq(1,240),VaR_1_I,VaR_5_I,VaR_10_I))

VaR_alpha_melt = melt(VaR_alpha, id.vars="dia")
ggplot(data=VaR_alpha_melt, aes(x=dia, y=value, group=variable,color=variable)) + 
  geom_line()
 
##########################################
## VaR Componentes para diferentes alpha 
##########################################
alpha <-  0.99
VaR_1<- matrix(0,ncol=nr_componentes,nrow=240)  
for(k in 1:240){
  for(c in 1:nr_componentes){
    VaR_1[k,c] <- -k*mean(R[,c])-sqrt(k*var(R[,c]))*qnorm(1-alpha)
  }
}

alpha <-  0.95
VaR_5<- matrix(0,ncol=nr_componentes,nrow=240)  
for(k in 1:240){
  for(c in 1:nr_componentes){
    VaR_5[k,c] <- -k*mean(R[,c])-sqrt(k*var(R[,c]))*qnorm(1-alpha)
  }
}

alpha <-  0.90
VaR_10<- matrix(0,ncol=nr_componentes,nrow=240)  
for(k in 1:240){
  for(c in 1:nr_componentes){
    VaR_10[k,c] <- -k*mean(R[,c])-sqrt(k*var(R[,c]))*qnorm(1-alpha)
  }
}

VaR_alpha_c <- cbind(seq(1,240),VaR_1,VaR_5,VaR_10)
colnames(VaR_alpha_c) <- c("dia",paste0(colnames(Data),"_VaR_1"),paste0(colnames(Data),"_VaR_5"),paste0(colnames(Data),"_VaR_10"))
VaR_alpha_c  <- as.data.frame(VaR_alpha_c)

VaR_alpha_c_melt = melt(VaR_alpha_c , id.vars="dia")
ggplot(data=VaR_alpha_c_melt, aes(x=dia, y=value, group=variable,color=variable)) + 
  geom_line()

