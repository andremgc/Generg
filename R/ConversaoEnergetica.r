#' @export
#' 
ConversaoEnergetica<- function(perdasTot,passo,TempRef,TempCoef,Pac0,Pdc0,IrrRef,G.ef.T,TA, a, b, DeltaT, VV ,n_non_sobre_n_ref, n_non, Delta.T){
  Tm<-G.ef.T*exp(a+b*VV)+TA
  Tc<-Tm+(G.ef.T/IrrRef)*DeltaT
  Pdc<-G.ef.T/IrrRef*Pdc0*(1+TempCoef*(Tc-TempRef))/Delta.T
  E_no.loss<-Pdc*passo/60*Delta.T
  Pinv_dc<-Pdc*(1-perdasTot)
  E_dc<-Pinv_dc*passo/60*Delta.T
  nu<-ifelse(Pinv_dc<0.01*Pac0,0,n_non_sobre_n_ref*((-0.0162)*(Pinv_dc/Pdc0)-0.0059*Pdc0/Pinv_dc+0.9858))
  Pinv_ac<-ifelse(Pinv_dc>(Pac0/n_non),Pac0,Pinv_dc*nu)
  E<-Pinv_ac*passo/60*Delta.T
  
  saida<-data.frame(Tm,E,E_dc)
  return(saida)
  
}
