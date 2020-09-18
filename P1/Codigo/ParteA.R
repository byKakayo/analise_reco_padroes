#AUTOMATO PROBABILÍSTICO


#Chama arquivo em R com as funcoes utilizadas
source("functions.R")


#Nós do automato (GENERALIZAÇÃO)
nos <-c("0", "1")


#Probabilidade de transicao
dt1 <-c(0.9, 0.9,
        0.1, 0.1)

dt2 <-c(0.2, 0.2,
        0.8, 0.8)

dt3 <-c(0.5, 0.5,
        0.5, 0.5)


#Transformar probabilidade em matriz de transicao
mt1 <-matrix(data = dt1, byrow = T, ncol = 2, dimnames = list(nos, nos))

mt2 <-matrix(data = dt2, byrow = T, ncol = 2, dimnames = list(nos, nos))

mt3 <-matrix(data = dt3, byrow = T, ncol = 2, dimnames = list(nos, nos))


#Quantidade de padroes gerados
qtdP = 200


#Estado inicial
e = "0"


#Inicializacao do plot
plot(NULL, 
     main = "Distribuição das frequências de 1s",
     axes = F,
     ylab = "Density",
     xlab = "Frequency",
     type = "l",
     xlim = c(0,1),
     ylim = c(0,60))
axis(1, pos = 0)
axis(2, pos = 0)


#Loop para gerar os padroes de diferentes numeros de iteracoes
for (n in seq(2000,500,-250)) {
  vfreq1 <-c()
  vfreq2 <-c()
  vfreq3 <-c()
  #Loop para gerar o numero de padroes definido por qtdP
  for (i in 1:qtdP) {
    #Matrix que recebe o padrao gerado
    ve1 = generate_pattern(nos, mt1, n, e)
    ve2 = generate_pattern(nos, mt2, n, e)
    ve3 = generate_pattern(nos, mt3, n, e)
    #Calculo da frequencia de 1s (f = (1s)/n)
    freq1 <- table(ve1[,2])
    vfreq1 <-cbind(vfreq1, as.numeric(freq1[2])/n)
    freq2 <- table(ve2[,2])
    vfreq2 <-cbind(vfreq2, as.numeric(freq2[2])/n)
    freq3 <- table(ve3[,2])
    vfreq3 <-cbind(vfreq3, as.numeric(freq3[2])/n)
  }
  #Plot da desidade da frequencia de 1s
  lines(density(vfreq1, bw=sd(vfreq1)), 
        col="blue")    
  lines(density(vfreq2, bw=sd(vfreq2)), 
        col="red")
  lines(density(vfreq3, bw=sd(vfreq3)), 
        col="green")
}
  
  
#BAR PLOT
barplot(height = freq1,
        names = c(0,1),
        main = "Ocorrência dos nós - Autômato 6(a)",
        ylim = c(0,n),
        col = c("#19AAF8", "#F72C2C"),
        border = "#FFFFFF")
barplot(height = freq2,
        names = c(0,1),
        main = "Ocorrência dos nós - Autômato 6(b)",
        ylim = c(0,n),
        col = c("#19AAF8", "#F72C2C"),
        border = "#FFFFFF")
barplot(height = freq3,
        names = c(0,1),
        main = "Ocorrência dos nós - Autômato 6(c)",
        ylim = c(0,n),
        col = c("#19AAF8", "#F72C2C"),
        border = "#FFFFFF")


barplot(height = as.numeric(ve1[,2]), names.arg="", col= n, border = NA, 
        axes = FALSE, main = "Visualização do padrão gerado - Autômato 6(a)")
rect(par("usr")[1], par("usr")[3], par("usr")[2], 
     par("usr")[4],col = "#19AAF8")
barplot(height = as.numeric(ve1[,2]),
        col = "#ff0043",
        border = "#ff0043",
        main = "Visualização do padrão gerado - Autômato 6(a)",
        axes = FALSE,
        add = TRUE)
barplot(height = as.numeric(ve2[,2]), names.arg="", col= n, border = NA, 
        axes = FALSE,  main = "Visualização do padrão gerado - Autômato 6(b)")
rect(par("usr")[1], par("usr")[3], par("usr")[2], 
     par("usr")[4],col = "#19AAF8")
barplot(height = as.numeric(ve2[,2]),
        col = "#ff0043",
        border = "#ff0043",
        axes = FALSE,
        add = TRUE)
barplot(height = as.numeric(ve3[,2]), names.arg="", col= n, border = NA, 
        axes = FALSE, main = "Visualização do padrão gerado - Autômato 6(c)")
rect(par("usr")[1], par("usr")[3], par("usr")[2], 
     par("usr")[4],col = "#19AAF8")
barplot(height = as.numeric(ve3[,2]),
        col = "#ff0043",
        border = "#ff0043",
        axes = FALSE,
        add = TRUE)


#STEMPLOT
#Passando os valores para a funcao
stemPlot(as.numeric(ve1[,1]), as.numeric(ve1[,2]), main = "Visualização do padrão gerado - Autômato 6(a)")
stemPlot(as.numeric(ve2[,1]), as.numeric(ve2[,2]), main = "Visualização do padrão gerado - Autômato 6(b)")
stemPlot(as.numeric(ve3[,1]), as.numeric(ve3[,2]), main = "Visualização do padrão gerado - Autômato 6(c)")


#SQUARE WAVE PLOT
plot(x = ve1[,1], 
     y = ve1[,2], 
     type='s', 
     axes=F,
     xlab="iteração",
     ylab="valor",
     main = "Visualização do padrão gerado - Autômato 6(a)")
axis(1, pos = -0.03)
axis(2, pos = -8)
plot(x = ve2[,1],
     y = ve2[,2],
     type='s', 
     axes=F,
     xlab="iteração",
     ylab="valor",
     main = "Visualização do padrão gerado - Autômato 6(b)")
axis(1, pos = -0.03)
axis(2, pos = -8)
plot(x = ve3[,1],
     y = ve3[,2],
     type='s',
     axes=F,
     xlab="iteração",
     ylab="valor",
     main = "Visualização do padrão gerado - Autômato 6(c)")
axis(1, pos = -0.03)
axis(2, pos = -8)