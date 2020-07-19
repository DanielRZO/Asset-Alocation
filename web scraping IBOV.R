library(RCurl)
library(rlist)

# Web scraping  IBOVESPA
#########################

url <- getURL("https://finance.yahoo.com/quote/%5EBVSP/components/",
              .opts = list(ssl.verifypeer = FALSE))
table <- readHTMLTable(url) 
table <- list.clean(table, fun = is.null, recursive = FALSE)
n.rows <- unlist(lapply(table, function(t) dim(t)[1]))
table <- table[[which.max(n.rows)]]
componentes <- table[,1]  # componentes do IBOVESPA


# IBOVESPA 
###########

k <- 5 # ultimos K anos (time series)
end <- format(Sys.Date(),"%Y-%m-%d")

start <- format(Sys.Date()-(k*252), "%Y-%m-%d") 

componentes.port <- as.character(componentes)
componentes.port <- componentes.port[!(componentes.port %in% c("KLBN11.SA", "656690", "TBLE3.SA"))] #retirando alguns componentes 
componentes.port <- gsub("VALE5.SA", "VALE3.SA", componentes.port)  # para substituir um componente pelo outro
l <- length(componentes.port)
w0 <- NULL

for( i in 1:l){
  dat0 <- getSymbols(componentes.port[i], src = "yahoo", from = start, to = end, auto.assign = F, 
                     warnings = F, symbol.lookup = F)
  dat0 <- na.omit(dat0)
  colnames(dat0) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  dat0 <- dat0[,"Adjusted"]  # escolhendo o valor ajustado
  w0 <- cbind(w0,dat0)
}

time <- as.Date(substr(index(w0), 1,10))
w0 <- as.matrix(w0)
colnames(w0) <- componentes.port

write.csv(w0, file = "IBOV.csv") #salvando o aruivo em .csv

