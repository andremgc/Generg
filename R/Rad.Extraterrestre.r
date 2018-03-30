#' @export
#' 
Rad.Extraterrestre<- function(tempos, theta.z, HaSol, G.sc=1367){
  
  G.on<-G.sc*(1+0.033*cos(360*(tempos$yday+1)/365*pi/180)) 
  G.o<-G.sc*(1+0.033*cos(360*(tempos$yday+1)/365*pi/180))*cos(theta.z*pi/180)*HaSol
  
  saida<-list("G.on"=G.on,"G.o"=G.o)
  return(saida)
  
}
