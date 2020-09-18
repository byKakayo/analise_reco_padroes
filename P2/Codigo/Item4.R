#Funcoes no arquivo functions.R
#split_signal
#scan_bursts
source("functions.R")
#Variáveis no arquivo automatos.R
#ve: padrao gerado pelo automato 
#nos: nos do automato
source("automatos.R")

A = visibility(as.numeric(ve[,2]), n)

rotate <- function(x) t(apply(x, 2, rev))
image(rotate(A), axes=F)
print(sum(A)/(n*n))

n <-c()
for(i in 1:200){
  n<-c(n, sum(A[i,]))
}

print(mean(n))
print(sd(n))
print(sum(n)/40000)