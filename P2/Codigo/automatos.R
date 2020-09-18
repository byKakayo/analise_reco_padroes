#Automatos utilizados no Projeto 2


#Funcao responsavel por gerar o padrao que recebe:
#Vetor com os nos do automato
#Matriz de transicao dos nos
#Valor n iteracoes do padrao
#Estado inicial do padrao
generate_pattern <- function(nos, mt, n, e) {
  #Adicionar estado inicial ao vetor
  ve <-matrix(c(0,e), ncol = 2)
  #Loop para gerar o padrao 
  for (i in 1:(n-1)) {
    #Gerar numero aleatorio entre 0 e 1
    x <-runif(1, 0, 1)
    
    #Contador que percorre as linhas do estado atual
    j = 1
    
    #Probabilidade de ir p/ primeiro nó
    p = mt[nos[j],e]
    
    #Teste da probabilidade
    while (x > p) {
      j = j + 1
      p = p + mt[nos[j],e]
    }
    #Atualizar o nó atual e adiciona ao vetor
    e = nos[j]
    ve <-rbind(ve, c(i, e))
  }
  return(ve)
}


#STEMPLOT
#Funcao geradora do plot
stemPlot <- function(x,y,pch=16,linecol=1,clinecol=1,...){
  if (missing(y)){
    y = x
    x = 1:length(x) }
  plot(x,y,pch=pch,...)
  for (i in 1:length(x)){
    lines(c(x[i],x[i]), c(0,y[i]),col=linecol)
  }
  lines(c(x[1]-2,x[length(x)]+2), c(0,0),col=clinecol)
}


#Nós do automato
nos <-c("0", "1", "2", "3")


#Probabilidade de transicao
dt <-c(0.5, 0  , 0  , 0.7,
       0.5, 0.1, 0  , 0  ,
       0  , 0.9, 0.6, 0  , 
       0  , 0  , 0.4, 0.3)


#Transformar probabilidade em matriz de transicao
mt <-matrix(data = dt, byrow = T, ncol = 4, dimnames = list(nos, nos))


#Numero de iteracoes
n = 200


#Estado inicial
e = "0"

ve = generate_pattern(nos, mt, n, e)