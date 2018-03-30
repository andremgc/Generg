Erbs<-function(Kt, I, theta.z, HaSol){
  Id.I<-ifelse(Kt==0,0,ifelse(Kt<=0.22,1-0.09*Kt,ifelse(Kt<=0.8,0.9511-0.1604*Kt+4.388*Kt^2-16.638*Kt^3+12.336*Kt^4,0.165)))
  Id<-Id.I*I
  Ib<-I-Id
  Ibn<-ifelse(cos(theta.z*pi/180)!=0,Ib/cos(theta.z*pi/180)*HaSol)
  saida<-list("Id"=Id, "Ib"=Ib, "Ibn"=Ibn)
}