#AUTOMATO PROBABILÍSTICO


#Chama arquivo em R com as funcoes utilizadas
source("functions.R")


#Nós do automato
nos <-c("0", "1", "2", "3", "4", "5")


#Probabilidade de transicao
dt <-c(0.9, 0.882, 0  , 0    , 0  , 0.01 ,
        0.1, 0.098, 0  , 0    , 0  , 0    ,
        0  , 0.02 , 0.2, 0.194, 0  , 0    ,
        0  , 0    , 0.8, 0.776, 0  , 0    ,
        0  , 0    , 0  , 0.03 , 0.5, 0.495,
        0  , 0    , 0  , 0    , 0.5, 0.495)


#Transformar probabilidade na matriz de transicao
mt <-matrix(data = dt, byrow = T, ncol = 6, dimnames = list(nos, nos))


#Quantidade de iterações
n = 500


#Estado inicial
e = "0"


#A matrix de transicao dos automatos dos itens 
#6(d) e 6(e) sao iguais
#Cria o padrão e armazena na matrix
ve1 = generate_pattern(nos, mt, n, e)
ve2 = generate_pattern(nos, mt, n, e)


#Square wave plot do automato 6(d)
plot(x = ve1[,1],
     y = ve1[,2],
     type='s', 
     axes=F,
     xlab="iteração",
     ylab="valor",
     xlim=c(0,n),
     ylim=c(0,5),
     main = "Visualização do padrão gerado - Autômato 6(d)")
axis(1, pos = -0.1)
axis(2, pos = -5) 


#Adequando o padrão aos valores assumidos pelo automato
ve2[ve2[,2] == "2",2] <- "0"
ve2[ve2[,2] == "4",2] <- "0"
ve2[ve2[,2] == "3",2] <- "1"
ve2[ve2[,2] == "5",2] <- "1"

plot(x = ve2[,1],
     y = ve2[,2],
     type='s', 
     axes=F,
     xlab="iteração",
     ylab="valor",
     xlim=c(0,n),
     ylim=c(0,1),
     main = "Visualização do padrão gerado - Autômato 6(e)")
axis(1, pos = -0.02)
axis(2, pos = -5) 

