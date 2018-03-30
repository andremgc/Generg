#'  essa funcao transforma H.med em Kt.mes. Tambem cria Kt.min, Kt.max, Gama, Csi, caso nao estejam na entrada
#' @export

TrabalhaDados <- function(DadosMensais,lat,G.sc){
  DadosMensais.1<-DadosMensais
  attach(DadosMensais.1)
  if(!"Kt.mes" %in% colnames(DadosMensais))
  {
    Declinacao<-c(-20.90,-13,-2.4,9.4,18.8,23.1,21.2,13.5,2.2,-9.6,-18.9,-23)
    n<-c(17,47,75,105,135,162,198, 228,258,288,318,344)  #numero de dias no mes
    omegas<-180/pi*(acos(-tan(pi/180*(lat))*tan(pi/180*(Declinacao))))
    Ho_med<-(24*3600/pi)*G.sc*(1+0.033*cos(pi/180*(360*n/365)))*(
        cos(pi/180*(lat))*cos(pi/180*(Declinacao))*(sin(pi/180*(omegas)))+
          pi*(omegas)/180*sin(pi/180*(lat))*sin(pi/180*(Declinacao))
    )/3600
    Kt.mes<-DadosMensais$H.med/Ho_med
    DadosMensais<-cbind(DadosMensais, Kt.mes)
  }
  if(!"Kt.min" %in% colnames(DadosMensais))
  {
    Kt.min<-c(0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05)
    DadosMensais<-cbind(DadosMensais, Kt.min)
    }
  if(!"Kt.max" %in% colnames(DadosMensais))
  {
    Kt.max<-(0.6313+0.267*Kt.mes-11.9*(Kt.mes-0.75)^8)/1.1
    DadosMensais<-cbind(DadosMensais, Kt.max)
  }
  if(!"Csi" %in% colnames(DadosMensais))
  {
    Csi<-(Kt.max-Kt.min)/(Kt.max-Kt.mes)
    DadosMensais<-cbind(DadosMensais, Csi)
  }
  if(!"Gama" %in% colnames(DadosMensais))
  {
    Gama<--1.498+(1.184*Csi-27.182*exp(-1.5*Csi))/(Kt.max-Kt.min)
    DadosMensais<-cbind(DadosMensais, Gama)
  }
  
  detach(DadosMensais.1)
  return(DadosMensais)
}