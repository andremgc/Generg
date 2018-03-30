#'Temperatura Sintetico
#'
#' Essa funcao recebe informacoes mensais (sao necessarias Kt.mes e TA) e o conjunto de tempos, gerando temperaturas com intervalo de uma hora
#' @export


TemperaturaSintetico<-function(tempos,DadosMensais,tempos.solar,TemperaturaConstante){
  
  h.t<-tempos.solar$hour+tempos.solar$min/60+round(tempos.solar$sec)/60/60 
  
  t<-ifelse(h.t>1, 2*pi*(h.t-1)/24, 2*pi*(h.t+24-1)/24)
  Th_T_A<-0.4632*cos(t-3.805)+0.0984*cos(2*t-0.36)+ 0.0168*cos(3*t-0.822)+0.0138*cos(4*t-3.513)
  A<-25.8*DadosMensais$Kt.mes[tempos$mon+1]-5.21
  TA<-DadosMensais$TA[tempos$mon+1]
  Th<-Th_T_A*A+TA
  
  if(TemperaturaConstante){
    Temp<-Th
  }
  else{
  
    Y<-vector(length=length(tempos))
    Y[1]<-0
    Y[2]<-qnorm(runif(1,0,1))+Y[1]
    if(TemperaturaConstante){
      Desvio_temp_mes<-0
    }
    
    chi<-qnorm(runif(length(tempos),0,1))
    for (i in 3:length(tempos)){
      Y[i]<-Y[i-1]*1.178-Y[i-2]*0.202+chi[i]
    }
   # Desvio_temp_mes<-1.99722565168691
    Desvio_temp_mes<-0.486183407
  
    sigma.m<-1.45-0.029*TA+0.0664*Desvio_temp_mes
    Temp<-ifelse(Y< -7.8,     Th-(sigma.m*sqrt(2/24)/3.396)     *   34.21,
                 ifelse(Y > 7.8,       Th-(sigma.m*sqrt(2/24)/3.396)     *   -33.41,
                        Th-(sigma.m*sqrt(2/24)/3.396)     *   log(1/((0.5*(1+erf(Y/sqrt(2)))))-1)))
    sum(Temp)
  

  aux=vector(length=length(tempos))
  for (N in 1:length(tempos))
    aux[N]<-mean(Temp[tempos$mon == tempos[N]$mon])
  
  Temp.e<-Temp*DadosMensais$TA[tempos$mon+1]/aux
  }
  
}
