#' Funcao que pega dados mensais e retorna valores de Kt medios diarios. Baseado em Liu e Jordan.
#' A entrada DadosMensais deve conter as colunas:
#' n.Dias, com o numero de dias do referido mes,
#' Gama (que é funcao do Kt.min, Kt.max e Kt.mes)
#' Kt.min
#' Kt.max
#' Kt.mes
#' 
#' A Função retorna um vetor com tamanho 365(ou diferente, se a soma de n.dias for outra)
#' Este vetor contem os valores gerados de Kt diario
#' 
#' 
#' Observação: para fazer a permutação dos dias em meses com menos de 31 dias, o dia 31 foi desconsiderado
#' Observação: Pode ser interessante criar
#' @export

Mes2DiaModificado<-function(DadosMensais){
  permutar<-function(vetor,ndias){
    vetor <- vetor[vetor<=ndias]
    aleatorio<-sample(1:ndias,1)
    vetormodificado<-vetor[c(aleatorio:ndias,1:aleatorio-1)]
  }
  
  #Diferentes padroes semi-estocasticos, para se utilizar
  #se Kt<45, utiliza vetorkt45
  vetorkt45<-c(24, 28, 11, 19, 18, 3, 2, 4, 9, 20, 14, 23, 8, 16, 21, 26,
  15, 10, 22, 17, 5, 1, 6, 29, 12, 7, 31, 30, 27, 13, 25)
  #se 45<Kt<55, utiliza vetorkt55
  vetorkt55<-c(24, 27, 11, 19, 18, 3, 2, 4, 9, 20, 14, 23, 8, 16,21,7,
               22, 10, 28, 6, 5, 1, 26, 29, 12, 17, 31, 30, 15, 13, 25)
  #se Kt>55, utiliza vetorktmax
  vetorktmax<-c(24, 27, 11, 4, 18, 3, 2, 19, 9, 25, 14, 23, 8, 16, 21, 26,
                22, 10, 15, 17, 5, 1, 6, 29, 12, 7, 31, 20, 28, 13, 30)
  
  
  f<-matrix(data=NA, nrow=31, ncol=length(DadosMensais[,1]))
  Kt<-matrix(data=NA, nrow=31, ncol=length(DadosMensais[,1]))
  Kt1<-Kt
  Kt2<-Kt
  colunaf<-Kt
  #laco nos meses
  consulta<-read.csv2("ConsultaLiuModificado.csv")
  
  for (m in 1:length(DadosMensais[,1])){
    bin<-findInterval(DadosMensais$Kt.mes[m],consulta[,1])
    for (d in 1:DadosMensais$n.Dias[m]){
      f[d,m]<-(2*d-1)/(2*DadosMensais$n.Dias[m])
      #determina em qual coluna esta o f
      colunaf[d,m]<-findInterval(f[d,m],consulta[1,]) 
      
      Kt1[d,m]<-consulta[bin,colunaf[d,m]]
      Kt2[d,m]<-consulta[bin+1,colunaf[d,m]]
      
      Kt[d,m] <- Kt1[d,m]+(Kt2[d,m]-Kt1[d,m])*
        (DadosMensais$Kt.mes[m]-consulta[bin,1])/(consulta[bin+1,1]-consulta[bin,1])
    }
    
    media<-mean(Kt[1:DadosMensais$n.Dias[m],m])
    #      erro<-(media-DadosMensais$Kt.mes[m])/DadosMensais$Kt.mes[m]
    Kt[,m]<-Kt[,m]/media*DadosMensais$Kt.mes[m]
    if(DadosMensais$Kt.mes[m]<=0.45){
      Kt[1:DadosMensais$n.Dias[m],m]<-Kt[permutar(vetorkt45,DadosMensais$n.Dias[m]),m]
    } else if (DadosMensais$Kt.mes[m]>0.45 && DadosMensais$Kt.mes[m]>0.55){
        Kt[1:DadosMensais$n.Dias[m],m]<-Kt[permutar(vetorkt55,DadosMensais$n.Dias[m]),m]
    } else{
      Kt[1:DadosMensais$n.Dias[m],m]<-Kt[permutar(vetorktmax,DadosMensais$n.Dias[m]),m]
    }
    #Kt[,m]<-sample(Kt[,m])
  }
  Kt.d<-as.vector(Kt[!is.na(Kt)])
}


  

  