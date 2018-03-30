Orgill<-function(Kt, I, theta.z, HaSol){
  Id.I<-ifelse(Kt==0,0,ifelse(Kt<=0.35,1-0.249*Kt,ifelse(Kt<=0.75,1.557-1.84*Kt,0.177)))
  Id<-Id.I*I
  Ib<-I-Id
  Ibn<-ifelse(cos(theta.z*pi/180)!=0,Ib/cos(theta.z*pi/180)*HaSol)
  saida<-list("Id"=Id, "Ib"=Ib, "Ibn"=Ibn)
}
