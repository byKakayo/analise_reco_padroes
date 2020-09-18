dist_norm_polar <- function(n, r=1) {
    x = c()
    y = c()
    while (length(x) < n) {
        p = runif(2, min=-r, max=r)
        
        if ( ( p[1]**2 + p[2]**2 <= r**2) && (length(x) < n) ) {
            x <- c(x, p[1])
            y <- c(y, p[2])
        }
    }
    return(data.frame("x"=x, "y"=y))
}

deformation <- function(x, y, xaxis=1, yaxis=1) {
    x = x*xaxis
    y = y*yaxis
    return(data.frame("x"=x, "y"=y))
}

rotation <- function(x, y, theta=0) {
    x = x*cos(theta) + y*sin(theta)
    y = y*cos(theta) + x*sin(theta)
    return(data.frame("x"=x, "y"=y))
}

pca <- function(n, r=1, xaxis=1, yaxis=1, theta=0) {
    dados <- dist_norm_polar(n, r);
    plot(dados, asp=1)
    #Sys.sleep(3);
    dados <- deformation(dados["x"], dados["y"], xaxis, yaxis);
    plot(dados, asp=1)
    #Sys.sleep(3);
    dados <- rotation(dados["x"], dados["y"], theta);
    plot(dados, asp=1)
    #Sys.sleep(3);
    K_dados <- cov(dados);
    print("Matriz de covariância K:")
    print(K_dados)
    print("")
    eig_dados <- eigen(K_dados);
    print("Autovalores:")
    print(eig_dados$values)
    print("Autovetores:")
    print(eig_dados$vectors)
    arrows(0, 0, x1=eig_dados$vectors[1,1]*0.2, y1=eig_dados$vectors[2,1]*0.2, col="blue", lwd=3);
    arrows(0, 0, x1=eig_dados$vectors[1,2]*0.2, y1=eig_dados$vectors[2,2]*0.2, col="red",  lwd=3);
}
