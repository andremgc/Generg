#' This funcion gives as outputs the parameters r.0 r.1 and r.k of atmospheric transmitnace considering the input TipoDeClima

TransmitanciaAtmosferica <-function(TipoDeClima) {
  if (TipoDeClima=="Lat.intermed./Inverno"){
    r.0<-1.03
    r.1<-1.01
    r.k<-1.00
  } else if (TipoDeClima=="Lat.intermed./Verao"){
    r.0<-0.97
    r.1<-0.99
    r.k<-1.02
  } else if (TipoDeClima=="Tropical"){
    r.0<-0.95
    r.1<-0.98
    r.k<-1.01
  } else if (TipoDeClima=="Verao Subartico"){
    r.0<-0.95
    r.1<-0.98
    r.k<-1.01
  } else {
    r.0<-1
    r.1<-1
    r.k<-1
  }
  saida<-list("r.0"=r.0, "r.1"= r.1,"r.k"= r.k)
  
}
