#' @export
#' 
Pl.Inc_DirRef<-function(theta,theta.z,HaSol,ro.g,Beta,G.b, G){
  
  R.b<-cos(theta*pi/180)/cos(theta.z*pi/180)
  R.b[is.nan(R.b)]<-0
  R.b[R.b<0]<-0
  R.b[R.b>10]<-10###
  
  #conferir formulacao
  #G.c.T.b<-cos(theta*pi/180)*G.cnb
  G.T.b<-G.b*R.b*HaSol
  
  G.T.refl<-G*ro.g*(1-cos(Beta*pi/180))/2
  
  saida<-list("G.T.b"=G.T.b,"G.T.refl"=G.T.refl, "R.b"=R.b)
  return(saida)
}
