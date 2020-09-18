#Funcoes no arquivo functions.R
#split_signal
#scan_intersymbols
source("functions.R")
#Variáveis no arquivo automatos.R
#ve: padrao gerado pelo automato 
#nos: nos do automato
source("automatos.R")

library('spectral')


sp = split_signal(ve, nos)

ft1 = fft(sp[1,])
ft2 = fft(sp[2,])
ft3 = fft(sp[3,])
ft4 = fft(sp[4,])

s1 <-spectrum(ft1, log="no", plot=FALSE)
s2 <-spectrum(ft2, log="no", plot=FALSE)
s3 <-spectrum(ft3, log="no", plot=FALSE)
s4 <-spectrum(ft4, log="no", plot=FALSE)

p1 = (Conj(ft1)*ft1)[2:101]
p2 = (Conj(ft2)*ft2)[2:101]
p3 = (Conj(ft3)*ft3)[2:101]
p4 = (Conj(ft4)*ft4)[2:101]

m1 = mean(abs(p1))
m2 = mean(abs(p2))
m3 = mean(abs(p3))
m4 = mean(abs(p4))

d1 = std.error(abs(p1))
d2 = std.error(abs(p2))
d3 = std.error(abs(p3))
d4 = std.error(abs(p4))

par(mfrow=c(2,2))
plot(x = s1$freq*2, y = p1, type = 'h')
plot(x = s2$freq*2, y = p2, type = 'h')
plot(x = s3$freq*2, y = p3, type = 'h')
plot(x = s4$freq*2, y = p4, type = 'h')

