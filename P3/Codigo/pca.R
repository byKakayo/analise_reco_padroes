circlenormaldistribution <- function(n, r=1) {
    # inicializa os vetores x e y
    x <- c()
    y <- c()
    #Loop para preencher os vetores x e y
    while (length(x) < n) {
        #Gera um ponto aleatorio
        p = runif(2, min=-r, max=r)
        #Verifica r
        if ( p[1]**2 + p[2]**2 <= r**2 ) {
            # adiciona o ponto nos vetores x e y
            x <- c(x, p[1])
            y <- c(y, p[2])
        }
    }
    return(data.frame("x"=x, "y"=y))
}

deformation <- function(x, y, xaxis=1, yaxis=1) {
    #Deforma x e y
    x = x*xaxis
    y = y*yaxis
    return(data.frame("x"=x, "y"=y))
}

rotation <- function(x, y, theta=0) {
    #Aplica uma rota��o de angulo theta no sentido anti-hor�rio
    x = x*cos(theta) + y*sin(theta)
    y = y*cos(theta) + x*sin(theta)
    return(data.frame("x"=x, "y"=y))
}

main <- function() {
    # valores iniciais
    n = 500;
    r = 1;
    xaxis = 1;
    yaxis = 0.2;
    theta = pi/6; # 30 graus

    # dados recebe um conjunto de pontos  distribuição normal circular
    dados <- circlenormaldistribution(n, r);
    plot(dados, main="", xlim = c(-1,1), ylim = c(-1,1), col = '#2deb53', pch=20)

    # conjunto de dados recebe uma deformação
    dados <- deformation(dados["x"], dados["y"], xaxis, yaxis);
    plot(dados, main="", xlim = c(-1,1), ylim = c(-1,1), col = '#2deb53', pch=20)

    # conjuto de dados recebe uma rotação
    dados <- rotation(dados["x"], dados["y"], theta);
    plot(dados, main="", xlim = c(-1,1), ylim = c(-1,1), col = '#2deb53', pch=20)

    # matriz K de covariância dos dados
    K_dados <- cov(dados);
    print("Matriz de covariância K:")
    print(K_dados)

    # autovalores/autovetores de K
    eig_dados <- eigen(K_dados);

    # autovalor
    lambda <- as.matrix(eig_dados$values)
    # autovetor
    vectors <- as.matrix(eig_dados$vectors)

    # indices ordenados dos autovalores
    idx <- sort(lambda, index.return=TRUE)$ix
    # ordenação dos autovalores/autovetores
    lambda <- lambda[idx]
    vectors <- vectors[,idx]
    
    print("Autovalores:")
    print(lambda)
    print("Autovetores:")
    print(vectors)
    
    # desenha os autovetores
    arrows(c(0,0), c(0,0), vectors[1,], vectors[2,], col=c("blue","red"), length=0.1, lwd=c(3,3));

}

main()