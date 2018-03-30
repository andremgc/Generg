TauAlbedo <- function(Beta, eta.Ar, eta.Vidro, eta.VidroAR) {
  
  theta.1<-c(0:90)
  theta.2<-asin(eta.Ar/eta.VidroAR*sin(theta.1*pi/180))*180/pi
  theta.3<-asin(eta.VidroAR/eta.Vidro*sin(theta.2*pi/180))*180/pi
  
  tau.AR<-1-0.5*(((sin(theta.2*pi/180-theta.1*pi/180))^2)/((sin(theta.2*pi/180+theta.1*pi/180))^2)+((tan(theta.2*pi/180-theta.1*pi/180))^2)/((tan(theta.2*pi/180+theta.1*pi/180))^2))
  tau.AR[1]<-tau.AR[2]
  
  tau.glass<-1-0.5*(((sin(theta.3*pi/180-theta.2*pi/180))^2)/((sin(theta.3*pi/180+theta.2*pi/180))^2)+((tan(theta.3*pi/180-theta.2*pi/180))^2)/((tan(theta.3*pi/180+theta.2*pi/180))^2))
  tau.glass[1]<-tau.glass[2]
  
  tau.cover<-tau.glass*tau.AR
  tau.cover.norm<-tau.cover/max(tau.cover)
  
  angulo.sol<-c(-90:90)
  angulo.incidencia<-acos(sin(Beta*pi/180)*cos(angulo.sol*pi/180))*180/pi
  
  #possivelmente sera possivel melhora esta parte
  resposta.incidencia<-tau.cover.norm[round(angulo.incidencia+1)]
  media<-mean(resposta.incidencia)
  
  return(media)
  
}
