CoeficientesPerez<- function(epsilon1){
  
  f11<-double(length=length(epsilon1))
  f12<-double(length=length(epsilon1))
  f13<-double(length=length(epsilon1))
  f21<-double(length=length(epsilon1))
  f22<-double(length=length(epsilon1))
  f23<-double(length=length(epsilon1))
  
  for (A in 1:length(epsilon1)){
    epsilon<-epsilon1[A]
    if (epsilon>6.2){
      f11[A]<-0.678
      f12[A]<--0.327
      f13[A]<--0.25
      f21[A]<-0.156
      f22[A]<--1.377
      f23[A]<-0.251
    } else if (epsilon>4.5){
      f11[A]<-1.060
      f12[A]<--1.6
      f13[A]<--0.359
      f21[A]<-0.264
      f22[A]<--1.127
      f23[A]<-0.131 
    } else if (epsilon>2.8){
      f11[A]<-1.132
      f12[A]<--1.237
      f13[A]<--0.412
      f21[A]<-0.288
      f22[A]<--0.823
      f23[A]<-0.056
    } else if (epsilon>1.95){
      f11[A]<-0.873
      f12[A]<--0.392
      f13[A]<--0.362
      f21[A]<-0.226
      f22[A]<--0.462
      f23[A]<-0.001
    } else if (epsilon>1.5){
      f11[A]<-0.568
      f12[A]<-0.187
      f13[A]<--0.295
      f21[A]<-0.109
      f22[A]<--0.152
      f23[A]<-0.014
    } else if (epsilon>1.23){
      f11[A]<-0.33
      f12[A]<-0.487
      f13[A]<--0.221
      f21[A]<-0.055
      f22[A]<--0.064
      f23[A]<--0.026
    } else if (epsilon>1.065){
      f11[A]<-0.13
      f12[A]<-0.683
      f13[A]<--0.151
      f21[A]<--0.019
      f22[A]<-0.066
      f23[A]<--0.029
    } else if (epsilon>1){
      f11[A]<--0.008
      f12[A]<-0.588
      f13[A]<--0.062
      f21[A]<--0.060
      f22[A]<-0.072
      f23[A]<--0.022
    } else {
      f11[A]<--0.0
      f12[A]<-0.0
      f13[A]<--0.0
      f21[A]<--0.0
      f22[A]<-0.0
      f23[A]<--0.0
    }
  }
  saida<-list("f11"=f11,"f12"=f12,"f13"=f13,"f21"=f21,"f22"=f22, "f23"=f23)
}
