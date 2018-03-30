#' @export
#' 
RadiacaoEfetiva <- function(eta.Ar,eta.VidroAR,eta.Vidro,theta,G.T.b,G.Td,G.T.refl) {
  
  
  theta.1<-ifelse(theta==0,theta+0.00000001,theta)
  theta.2<-asin(eta.Ar/eta.VidroAR*sin(theta.1*pi/180))*180/pi
  theta.3<-asin(eta.VidroAR/eta.Vidro*sin(theta.2*pi/180))*180/pi
  
  tau.AR<-1-0.5*(((sin(theta.2*pi/180-theta.1*pi/180))^2)/((sin(theta.2*pi/180+theta.1*pi/180))^2)+((tan(theta.2*pi/180-theta.1*pi/180))^2)/((tan(theta.2*pi/180+theta.1*pi/180))^2))
  
  tau.glass<-1-0.5*(((sin(theta.3*pi/180-theta.2*pi/180))^2)/((sin(theta.3*pi/180+theta.2*pi/180))^2)+((tan(theta.3*pi/180-theta.2*pi/180))^2)/((tan(theta.3*pi/180+theta.2*pi/180))^2))
  
  tau.cover<-tau.glass*tau.AR  
  
  theta.1b<-0.0000001
  theta.2b<-asin(eta.Ar/eta.VidroAR*sin(theta.1b*pi/180))*180/pi
  theta.3b<-asin(eta.VidroAR/eta.Vidro*sin(theta.2b*pi/180))*180/pi
  
  tau.ARb<-1-0.5*(((sin(theta.2b*pi/180-theta.1b*pi/180))^2)/((sin(theta.2b*pi/180+theta.1b*pi/180))^2)+((tan(theta.2b*pi/180-theta.1b*pi/180))^2)/((tan(theta.2b*pi/180+theta.1b*pi/180))^2))
  
  tau.glassb<-1-0.5*(((sin(theta.3b*pi/180-theta.2b*pi/180))^2)/((sin(theta.3b*pi/180+theta.2b*pi/180))^2)+((tan(theta.3b*pi/180-theta.2b*pi/180))^2)/((tan(theta.3b*pi/180+theta.2b*pi/180))^2))
  
  tau.cover.max<-tau.glassb*tau.ARb 
  
  tau.cover_theta<-tau.cover/tau.cover.max#mudar aqui
  
  #tau.cover.albedo<-TauAlbedo(Beta, eta.Ar, eta.Vidro, eta.VidroAR)#tirar
  
  tau.cover.albedo<-1
  RespAngDif<-1 
  
  G.ef.T.b<-G.T.b*tau.cover_theta
  G.ef.T.d<-G.Td*RespAngDif
  G.ef.T.refl<-G.T.refl*tau.cover.albedo
  
  G.ef.T<-G.ef.T.refl+G.ef.T.d+G.ef.T.b
  
  saida<-list("G.ef.T.b"=G.ef.T.b,"G.ef.T.d"=G.ef.T.d, "G.ef.T.refl"=G.ef.T.refl, "G.ef.T"=G.ef.T)
  
}
