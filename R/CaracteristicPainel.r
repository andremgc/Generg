#' @export
#' 
CaracteristicPainel <-function(TipoDePainel) {
  if (TipoDePainel=="Cristalino Premium"){
    eta.VidroAR<-1.3
    TempCoef<- -0.35/100
  } else if (TipoDePainel=="Cristalino Padrao"){
    eta.VidroAR<-1.526
    TempCoef<- -0.47/100
  } else if (TipoDePainel=="Filme fino"){
    eta.VidroAR<-1.526
    TempCoef<- -0.20/100
  } else {
    eta.VidroAR<-1.3
    TempCoef<- -0.20
  }
  eta.Vidro<-1.526
  eta.Ar<-1
  
  saida<-list("TempCoef"=TempCoef,"eta.Vidro"=eta.Vidro, "eta.Ar"=eta.Ar,"eta.VidroAR"= eta.VidroAR)
  
}
