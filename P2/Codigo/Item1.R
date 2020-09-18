#Funcoes no arquivo functions.R
#split_signal
#scan_bursts
source("functions.R")
#Variáveis no arquivo automatos.R
#ve: padrao gerado pelo automato 
#nos: nos do automato
source("automatos.R")


sp = split_signal(ve, nos)


matrix_item1 <-matrix(nrow=0, ncol=5)
colnames(matrix_item1) <-c("Nº BURSTS", "MÉDIA", "DESVIO PADRÃO", "ENTROPIA", "EVENNESS")


for (i in 1:dim(sp)[1]) {
  bursts = scan_bursts(sp[i,], n)
  #O tamanho dos bursts é a frequencia de cada valor no vetor
  freq_burst = table(bursts)
  #Probabilidade
  pi = freq_burst/n
  #Entropia
  e = -sum(pi*log2(pi))
  #Evenness
  ev = 2**e
  #O numero de bursts é a quantidade de valores diferentes no vetor
  num_burst = dim(freq_burst)
  matrix_item1 <-rbind(matrix_item1, c(num_burst, mean(freq_burst), sd(freq_burst), e, ev))
}

rownames(matrix_item1) <-nos
print(matrix_item1)
