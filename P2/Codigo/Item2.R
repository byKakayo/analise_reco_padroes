#Funcoes no arquivo functions.R
#split_signal
#scan_intersymbols
source("functions.R")
#Variáveis no arquivo automatos.R
#ve: padrao gerado pelo automato 
#nos: nos do automato
source("automatos.R")


sp = split_signal(ve, nos)


matrix_item2 <-matrix(nrow=0, ncol=5)
colnames(matrix_item2) <-c("Nº INTERSÍMBOLOS", "MÉDIA", "DESVIO PADRÃO", "ENTROPIA", "EVENNESS")


for (i in 1:dim(sp)[1]) {
  inter = scan_intersymbols(sp[i,], n)
  dist_intersymbols = diff(inter)
  freq_dist = table(dist_intersymbols)
  #Probabilidade
  pi = freq_dist/n
  #Entropia
  e = -sum(pi*log2(pi))
  #Evenness
  ev = 2**e
  num_dist = length(dist_intersymbols)
  matrix_item2 <-rbind(matrix_item2, c(num_dist, mean(dist_intersymbols), sd(dist_intersymbols), e, ev))
}

rownames(matrix_item2) <-nos
print(matrix_item2)
