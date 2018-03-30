#' esta função, com base nas entradas modo_radiacao, TipoAlbedo, TipoTA, TipoVV, retorna
#' os valores de radiacao, albedo, temperatura ambiente e velocidade do vento
#' @export
Ambientais_loop<- function(modo_radiacao, G.cnb, G.cd, theta.z, G.o, DadosAmbientais, HaSol, AlbedoFixo, TipoAlbedo, VVFixo, TipoVV, TAFixo, TipoTA, Ibn, Id, T.Sintetico, tempos, DadosMensais){ #podem ser adicionadas mais entradas de acordo com outras entradas de dado
  if(modo_radiacao=="ceu_limpo"){
    G.nb<-G.cnb 
    G.b<-G.nb*cos(theta.z*pi/180)
    G.d<-G.cd
    G<-G.d+G.b
    K.T<-G/G.o
  } else if (modo_radiacao=="Kt_mes"){
    G.nb<-Ibn
    G.b<-G.nb*cos(theta.z*pi/180)
    G.d<-Id
    G<-G.d+G.b
    K.T<-G/G.o
  } else if (modo_radiacao=="H_med"){
    G.nb<-Ibn
    G.b<-G.nb*cos(theta.z*pi/180)
    G.d<-Id
    G<-G.d+G.b
    K.T<-G/G.o
  } else {
    G.nb<-DadosAmbientais$G.nb
    G.b<-G.nb*cos(theta.z*pi/180)*HaSol
    G.d<-DadosAmbientais$G.d
    G<-G.d+G.b
    K.T<-G/G.o
  }
  
  if(TipoAlbedo=="horario"){
    ro.g<-DadosAmbientais$ro.g
  } else if (TipoAlbedo=="fixo"){
    ro.g<-AlbedoFixo
  }  else {
    ro.g<-DadosMensais$Albedo[tempos$mon+1]
  }

  if(TipoTA=="horario"){
    TA<-DadosAmbientais$TA
    TA<-TA[1:length(G.cnb)]
  } else if (TipoTA=="fixo"){
    TA<-TAFixo
  }  else {
    TA<-T.Sintetico
  }
  #albedo sera entrada. se for ou vetorial, sera albedo horario. Se for so um numero, albedo fixo
  #Ta e WS serao entrada nao estarao nesta funcao
  saida<-list("K.T"=K.T,"G"=G,"G.d"=G.d,"G.b"=G.b,"G.nb"=G.nb, "VV"=VV,"TA"=TA, "ro.g"=ro.g)
  return(saida)
}

