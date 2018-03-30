#' @export
#' 
CeuLimpo <- function(A,TipoDeClima,theta.z,HaSol,G.on, G.o) {
  
  a.0x<-0.4237-0.00821*(6-A)*(6-A)
  a.1x<-0.5055+0.00595*(6.5-A)*(6.5-A)
  k.x<-0.2711+0.01858*(2.5-A)*(2.5-A)
  
  tran<-TransmitanciaAtmosferica(TipoDeClima)
  
  attach(tran)
  a.0<-r.0*a.0x
  a.1<-r.1*a.1x
  k<-r.k*k.x
  detach(tran)
  
  
  tau.b<-(a.0+a.1*exp(-k/cos(theta.z*pi/180)))*HaSol
  #tau.b[cos(theta.z*pi/180)<=0]<-0
  
  tau.d<- (0.271-0.294*tau.b)*HaSol
  #tau.d[tau.b<=0]<-0
  
  G.cnb<-G.on*tau.b
  G.cd<-G.on*tau.d*cos(theta.z*pi/180)
  
  
  G.cb<-G.on*tau.b*cos(theta.z*pi/180)
  G.c<-G.cb+G.cd
  
  K.T.c<-G.c/G.o
  K.T.c[is.nan(K.T.c)]<-0
  
  saida<-list("K.T.c"=K.T.c,"G.c"=G.c,"G.cb"=G.cb,"G.cd"=G.cd,"G.cnb"=G.cnb, "tau_b"=tau.b, "tau_d"=tau.d)
  ########
  return(saida)
}
