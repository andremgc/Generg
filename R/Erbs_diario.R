Erbs.d<-function(Kt.d,omega.b,omega.sunset,HaSol,H,I,tempos,theta.z){
  rd<-pi/24*((cos(pi/180*omega.b)-cos(pi/180*omega.sunset))/(sin(pi/180*omega.sunset)-(pi/180*omega.sunset*cos(pi/180*omega.sunset))))*HaSol
  rd[rd<0]<-0
  
  Kt.df<-Kt.d[tempos$yday+1]
  Hd.H<-ifelse(Kt.df==0,0,
               ifelse((omega.sunset<=81.4 & Kt.df<0.715),
                      1-0.2727*Kt.df+2.4495*Kt.df^2-11.9514*Kt.df^3+9.3879*Kt.df^4,
                      ifelse((omega.sunset<=81.4 & Kt.df>=0.715),
                             0.143,
                             ifelse((omega.sunset>81.4 & Kt.df<0.715),
                                    1+0.2832*Kt.df-2.5557*Kt.df^2+0.8448*Kt.df^3,
                                    ifelse((omega.sunset>81.4 & Kt.df>=0.715),
                                           0.175,100000)))))
  Hd<-Hd.H*H
  Id<-Hd*rd
  Ib<-I-Id
  Ib[Ib<0]<-0
  Ibn<-ifelse(cos(theta.z*pi/180)!=0,Ib/cos(theta.z*pi/180)*HaSol)
  saida<-list("Id"=Id, "Ib"=Ib, "Ibn"=Ibn)
}
