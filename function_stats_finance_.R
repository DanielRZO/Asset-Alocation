install.packages("devtools")
library(devtools)
install_github("cran/FinTS", dependence = T)
library (moments)

estatisticas <- function(x)
{
y = as.matrix(x[,-1])  
nobs=NROW(y)
N=NCOL(y)
out <- matrix(0, ncol=N, nrow=6)
for(i in 1:N){
  mean = mean(y[,i])
  std.dev = sd(y[,i])
  max = max(y[,i])
  min = min(y[,i])
  skewness = skewness(y[,i])
  kurtosis = kurtosis(y[,i])
  out[,i] = t(cbind(mean, std.dev, max, min, skewness, kurtosis))
  out = cbind(out)
  }
colnames(out) <- colnames(y)
rownames(out) <- c("mean","std.dev","max","min","skewness","kurtosis")
out
}

estatisticas (data)
