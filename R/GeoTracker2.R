#' Função geométrico com tracker.
#' 
#' Esta funcao recebe como entradas informacoes da posicao do posicionamento do painel
#' e de tempo de simulacao e
#' retorna informacoes sobre os angulos solares por hora, como angulos horarios, declinacao, angulo azimutal. 
#' retorna tambem o vetor de tempo.
#' 
#' Esta funcao e o pilar do programa, varias de suas saidas sao utilizadas por quase todas as outras funcoes
#' 
#' @param lat Latidude
#' @param long Longitude
#' @param long.st Longitudo da hora padrao
#' @param Beta Declinacao do painel
#' @param gama Orientacao
#' @param passo tamanho do passo em minutos. default para 60
#' @param hora_do_passo informa o momento em que as medicoes para cada passo sao levadas em conta. Aceita "Mais_tarde", "exata". Default para "Mais_tarde. Qualquer outro input considera como mais cedo
#' @param tempo.inicial,tempo.final strings no formato "dd/mm/aaaa hh:mm" para definir o tempo final e inicial. Default para "01/01/2015 00:00"
#' 
#' @export
geo.trackerNREL <- function(lat, long, long.st, Beta, gama, passo, Hora_do_passo, tempo.inicial, tempo.final,tracker) {

  phi<-lat # outra notacao para lat.
  
  
  ## Gera POSIXlt tempos, que contem dias horas, etc.
  T1<-as.POSIXct(strptime(tempo.inicial, "%d/%m/%Y %H:%M"), tz="GMT") #tempo inicial
  TF<-as.POSIXct(strptime(tempo.final, "%d/%m/%Y %H:%M"), tz="GMT") #tempo final
 
  intervalo<-paste(as.character(passo), 'mins')
  
  tempos<-seq(T1,TF,  intervalo)
  tempos<-as.POSIXlt(tempos)
  
  #sempre considerar dia 1 como primeiro de janeiro?
  #em tempos$yday, primeiro de janeiro e dia 0, assim o 
  #(n-1) da formula fica como (n)
  B<-(tempos$yday+tempos$hour/24)*360/365
  
  E<-229.2*(0.000075+0.001868*cos(B*pi/180)-0.032077*sin(B*pi/180)-0.014615*cos(2*B*pi/180)-0.04089*sin(2*B*pi/180))
  
  long=ifelse(long<0,abs(long),-long+360)
  long.st=ifelse(long.st<0,abs(long.st),-long.st+360)
  
  ####
  Hs_Hp<-4*(long.st-long)+E #Hora Solar-Hora Padrão, min
  
  tempos.solar<-as.POSIXlt(tempos+(Hs_Hp)*60) #Data e hora solar
  
  #h<-tempos.solar$yday*24+tempos.solar$hour+tempos.solar$min/60+tempos.solar$sec/60/60 
  
  h.t<-tempos.solar$hour+tempos.solar$min/60+round(tempos.solar$sec)/60/60 
  # h.t<-tempos.solar$hour+tempos.solar$min/60+(tempos.solar$sec)/60/60
  
  if (Hora_do_passo=="Mais_tarde"){
    h.t<-h.t+passo/60
  } else if (Hora_do_passo=="Exata"){
    h.t<-h.t+passo/60/2
  }
  
  h.t_delta.t <- h.t-1
  
  omega.t<-h.t*15-180
  omega.t_delta.t<-h.t_delta.t*15-180
  
  omega.1<-omega.t_delta.t
  omega.2<-omega.t
  
  #delta<-23.45*sin(360*(284+tempos.solar$yday+1)/365*pi/180)
  delta<-23.45*sin(360*(284+tempos$yday+1)/365*pi/180)
  omega.sunset<-acos((-tan(phi*pi/180)*tan(delta*pi/180)))*180/pi
  omega.sunrise<- -omega.sunset
  
  HaSol<-(omega.t>omega.sunrise & omega.t_delta.t<omega.sunset) #Ha Sol? binaria, um pouco diferente
  
  omega<-(h.t+h.t_delta.t)/2*15-180
  omega[omega==0]<-0.001
  
  for (N in 1:length(omega)){
    if (omega.t_delta.t[N]<omega.sunrise[N] && omega.t[N]>omega.sunrise[N]){
      omega[N]<-(omega.sunrise[N]+omega.t[N])/2
      omega.1[N]<-omega.sunrise[N]
    }
    else if (omega.t_delta.t[N]<omega.sunset[N] && omega.t[N]>omega.sunset[N]){
      omega[N]<-(omega.sunset[N]+omega.t_delta.t[N])/2
      omega.2[N]<-omega.sunset[N]
    }
  }
  
  Delta.T<-(omega.2-omega.1)/15
  
  cos.theta.z<-cos(phi*pi/180)*cos(delta*pi/180)*cos(omega*pi/180)+sin(phi*pi/180)*sin(delta*pi/180)
  #cos.theta.z[cos.theta.z<=0]<-0
  
  theta.z<-acos(cos.theta.z)*180/pi
  
  gama.s<- sign(omega*pi/180)*(abs(acos((cos.theta.z*sin(phi*pi/180)-sin(delta*pi/180))/
                                          (sin(theta.z*pi/180)*cos(phi*pi/180)))))*180/pi
  gama.s[is.nan(gama.s)]<-0
  
  gama.s<-gama.s+180
  
  X<-(sin(theta.z*pi/180)*sin(pi/180*(gama.s-gama)))/
    (sin(theta.z*pi/180)*cos(pi/180*(gama.s-gama))*sin(Beta*pi/180)+cos(theta.z*pi/180)*cos(Beta*pi/180))
  
  gama.s_gama<-ifelse(gama.s-gama<(-180),gama.s-gama+360,ifelse(gama.s-gama>180,gama.s-gama-360,gama.s-gama))
  
  Psi<-ifelse(X<0 & (gama.s_gama)>0, 180, ifelse(X>0 & (gama.s_gama)<0, -180, 0 )) 
  
  R<-ifelse(atan(X)*180/pi+Psi>45,45,ifelse(atan(X)*180/pi+Psi< -45, -45, atan(X)*180/pi+Psi))

  
  Beta.out=acos(cos(R*pi/180) * cos(Beta*pi/180))*180/pi
  
  aux=asin(sin(R*pi/180) / sin(Beta.out*pi/180))*180/pi
  
  gama.out=ifelse((-180 <= R & R< -90),gama-aux-180, ifelse((90 < R & R <= 180),gama-aux+180, gama+aux ))

  cos.theta<-cos(theta.z*pi/180)*cos(Beta*pi/180)+sin(theta.z*pi/180)*sin(Beta*pi/180)*cos(gama.s*pi/180-gama*pi/180)
  cos.theta[cos.theta<=0]<-0
  
  theta<-acos(cos.theta)*180/pi
  
  cos.theta.out<-cos(theta.z*pi/180)*cos(Beta.out*pi/180)+sin(theta.z*pi/180)*sin(Beta.out*pi/180)*cos(gama.s*pi/180-gama.out*pi/180)
 # cos.theta.out[abs(theta.z)>90]<-0

  theta.out<-acos(cos.theta.out)*180/pi
  
  cos.theta.out2<-cos(R*pi/180)*
    (cos(theta.z*pi/180)*cos(Beta*pi/180)+sin(theta.z*pi/180)*sin(Beta*pi/180)*cos(gama.s*pi/180-gama*pi/180)) +
    sin(R*pi/180)*sin(theta.z*pi/180)*sin(gama.s*pi/180-gama*pi/180)
  
  cos.theta.out2[abs(theta.z)>90]<-0
  theta.out2<-acos(cos.theta.out)*180/pi
  
  
  
  
  lista<-list("theta"=theta,"gama.s"=gama.s,"theta.z"=theta.z,"omega"=omega,"tempos.solar"=tempos.solar,"h.t"=h.t,"phi"=phi,"HaSol"=HaSol, "tempos"=tempos, "omega.1"=omega.1, "omega.2"=omega.2,"Delta.T"=Delta.T, "omega.sunset"=omega.sunset, "omega.sunrise"=omega.sunrise , "omega.t"=omega.t, "omega.t_delta.t"=omega.t_delta.t, "delta"=delta)
  return (lista)
}

