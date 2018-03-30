#' @export
#' 
Sintetico<-function(omega.sunset,omega.t,omega.b_delta.t,DadosMensais, G.on, delta,
                    omega.1, omega.2, phi, HaSol, tempos, ModeloComposicao, ModeloSintese,
                    omega.t_delta.t, theta.z, tau_b,tau_d, K.T.c){
  
  ### Calculo de r_t
  
  # Parametros a e b
  a<-.409+.5016*sin((omega.sunset-60)*pi/180)
  b<-.6609-.4767*sin((omega.sunset-60)*pi/180)
  
  # Omega horario "bruto"
  omega.b<-(omega.t_delta.t+omega.t)/2  
  
  # r_t pre correcao
  r.t<-pi/24*
    (a+b*cos(omega.b*pi/180))*
      (cos(omega.b*pi/180)-cos(omega.sunset*pi/180))/
        (sin(omega.sunset*pi/180)-omega.sunset*pi/180*cos(omega.sunset*pi/180))
  r.t[r.t<0]<-0
  
  # Fator normalizador aux
   aux=vector(length=length(tempos))
   for (N in 1:length(tempos)){
     if (tempos[N]$mday==1 && tempos[N]$hour==0)
           aux[N]<-sum(r.t[N:(N+23)])
     else aux[N]<-aux[N-1]
   }
  
  
  #Normaliza r.t para garantir sum(r.t[tempos$yday[Dia]])==1 Para todos os dias       
  r.t<-r.t/aux
  
  Kt.dm<-DadosMensais$Kt.mes[tempos$mon+1]
  seq.aux<-seq(1,8760, by=24)
  Kt.dm<-Kt.dm[seq.aux]
  
  if (ModeloSintese=="KTmedio"){
    Kt.d<-Kt.dm
  } else if (ModeloSintese=="LiuJordanModificado"){
    Kt.d<-Mes2DiaModificado(DadosMensais)
  } else {
    Kt.d<-Mes2Dia(DadosMensais)  #Kt Diário obtido com Liu e Jordan
  }

  # Irradiancia horaria 
  I.o<-12*3600/pi*G.on*(cos(phi*pi/180)*cos(delta*pi/180)*(sin(omega.2*pi/180)-sin(omega.1*pi/180))+pi*(omega.2-omega.1)/180*sin(phi*pi/180)*sin(delta*pi/180))*HaSol/3600
  
  # Irradiancia diaria extraterrestre
  H.o<-24*3600/pi*G.on*(cos(phi*pi/180)*cos(delta*pi/180)*sin(omega.sunset*pi/180)+pi*(omega.sunset)/180*sin(phi*pi/180)*sin(delta*pi/180))/3600
  
  # Irradiancia diaria
  H<-H.o*Kt.d[tempos$yday+1]
  Hm<-H.o*Kt.dm[tempos$yday+1]
  
  ktm<-r.t*Kt.d[tempos$yday+1]*H.o/I.o   #kt Liu e Jordan (ou KTmedio, ou Liu e Jordan Modificado)
  ktm[is.nan(ktm)]<-0
  ktm[I.o==0]<-0
  
  I<-I.o*ktm
  
  ktmm<-r.t*Kt.dm[tempos$yday+1]*H.o/I.o   #kt Liu e Jordan (ou KTmedio, ou Liu e Jordan Modificado)
  ktmm[is.nan(ktmm)]<-0
  ktmm[I.o==0]<-0
  
  Imm<-I.o*ktmm
  
  Kt<-ktm
  
  # aux=vector(length=length(tempos))
  # for (N in 1:length(tempos)){
  #   if (tempos[N]$mday==1 && tempos[N]$hour==0)
  #     aux[N]<-mean(Imm[tempos$mon == tempos[N]$mon])
  #   else aux[N]<-aux[N-1]
  # }
 
  
# to do <- fazer com que o goal seja sempre relativo à entrada de dados. 
  
  aux.b<-DadosMensais$H.med[tempos$mon+1]/24
#  aux.b<-aux
  
  aux2=vector(length=length(tempos))
  for (N in 1:length(tempos)){
    if (tempos[N]$mday==1 && tempos[N]$hour==0)
      aux2[N]<-mean(I[tempos$mon == tempos[N]$mon])
    else aux2[N]<-aux2[N-1]
  }
  
  I<-I*aux.b/aux2
  Kt<-ifelse(I.o==0,0,I/I.o)
  
  # Knight
  
 if (ModeloSintese=="Knight"){
   Y<-vector(length=length(tempos))
   Y[1]<-0
   for (i in 2:length(tempos)){
     chi<-qnorm(runif(1,0,1))
     Y[i]<-Y[i-1]*0.54+chi
   }
   sigma.kt<-0.1557*sin(pi*Kt.d[tempos$yday+1]/0.933)
   Delta.kt<-sigma.kt/1.58*log(1/(0.5*(1+erf(Y/2^0.5)))-1)
   Kt_<-ifelse(ktm==0,0,ktm-Delta.kt) #kt estocastico
   
   I_<-I.o*Kt_
   
   aux=vector(length=length(tempos))
   for (N in 1:length(tempos)){
     if (tempos[N]$mday==1 && tempos[N]$hour==0)
       aux[N]<-mean(I_[tempos$mon == tempos[N]$mon])
     else aux[N]<-aux[N-1]
   }
   
   aux2=vector(length=length(tempos))
   for (N in 1:length(tempos)){
     if (tempos[N]$mday==1 && tempos[N]$hour==0)
       aux2[N]<-mean(I[tempos$mon == tempos[N]$mon])
     else aux2[N]<-aux2[N-1]
   }
   
   
   I.e<-I_*aux2/aux
   
   I<-I.e
   Kt<-ifelse(I.o==0,0,I/I.o)
 }

  
 if (ModeloSintese=="Whillier" || ModeloSintese=="WhillierModificado"){

    f<-runif(length(tempos))
    
    Kt_min<-0.05
    Kt_max<-(0.6313+0.267*Kt.d[tempos$yday+1]-11.9*(Kt.d[tempos$yday+1]-0.75)^8)
    Csi<-(Kt_max-Kt_min)/(Kt_max-Kt.d[tempos$yday+1])
    Gama<--1.498+(1.184*Csi-27.182*exp(-1.5*Csi))/(Kt_max-Kt_min)
    Gama.Rad<-log(exp(Gama*Kt_min)-f*(exp(Gama*Kt_min)-exp(Gama*Kt_max)))/Gama
    
    # fator de correcao do angulo zenital
    Fc<-(tau_b + tau_d)    #tau_b e tau_d vem de dia de ceu limpo
    
    Kt_<-Gama.Rad*Fc
    
    I_<-I.o*Kt_
    
    if (ModeloSintese=="WhillierModificado"){
      K.T.c.MAX<-vector(length=length(tempos))
          for (N in 1:length(tempos)){
            if (tempos[N]$mday==1 && tempos[N]$hour==0)
              K.T.c.MAX[N]<-max(K.T.c[tempos$mon == tempos[N]$mon])
          else K.T.c.MAX[N]<-K.T.c.MAX[N-1]
          }
      Kt_<-Kt_*K.T.c/K.T.c.MAX
      I_<-I.o*Kt_
    }
    
    aux=vector(length=length(tempos))
    for (N in 1:length(tempos)){
      if (tempos[N]$mday==1 && tempos[N]$hour==0)
        aux[N]<-mean(I_[tempos$mon == tempos[N]$mon])
      else aux[N]<-aux[N-1]
    }

    aux2=vector(length=length(tempos))
    for (N in 1:length(tempos)){
      if (tempos[N]$mday==1 && tempos[N]$hour==0)
        aux2[N]<-mean(I[tempos$mon == tempos[N]$mon])
      else aux2[N]<-aux2[N-1]
    }
    
    
    I.e<-I_*aux2/aux
    
    I<-I.e
    Kt<-ifelse(I.o==0,0,I/I.o)
  }
  
  
  # tempos$yday
  # como fazer a entrada?
  if(ModeloComposicao=="Orgill"){
    saida<-Orgill(Kt, I, theta.z, HaSol)
  }else if(ModeloComposicao=="Erbs"){
    saida<-Erbs(Kt, I, theta.z, HaSol)
  }else if(ModeloComposicao=="Collares"){
    saida<-Collares(Kt.d,omega.b,omega.sunset,HaSol,H,I,tempos,theta.z)
  }else {
    saida<-Erbs.d(Kt.d,omega.b,omega.sunset,HaSol,H,I,tempos,theta.z)
  }
  
  #  saida<-list("Id"=Id, "Ib"=Ib, "Ibn"=Ibn)
    return(saida)    
  
}
