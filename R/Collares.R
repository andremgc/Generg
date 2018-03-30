#' @export
#' 
Collares<-function(Kt.d,omega.b,omega.sunset,HaSol,H,I,tempos,theta.z){
  rd<-pi/24*((cos(pi/180*omega.b)-cos(pi/180*omega.sunset))/(sin(pi/180*omega.sunset)-(pi/180*omega.sunset*cos(pi/180*omega.sunset))))*HaSol
  rd[rd<0]<-0
  Hd.H<-ifelse(Kt.d==0,0,
               ifelse(Kt.d<=0.17,0.99,
                      ifelse(Kt.d<=0.75,1.188-2.272*Kt.d +9.473*Kt.d^2-21.865*Kt.d^3+14.648*Kt.d^4,
                             ifelse(Kt.d<=0.8,-0.54*Kt.d+0.632,
                                    0.2)))) 
  Hd<-Hd.H[tempos$yday+1]*H
  Id<-Hd*rd
  Ib<-I-Id
  Ib[Ib<0]<-0
  Ibn<-ifelse(cos(theta.z*pi/180)!=0,Ib/cos(theta.z*pi/180)*HaSol)
  saida<-list("Id"=Id, "Ib"=Ib, "Ibn"=Ibn)
}
