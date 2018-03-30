#' @export

Calculos_Horarios<-function(long,long.st,lat,A,
                                         DadosAmbientais,Pdc0,Beta,gama,
                                         TipoDePainel,pot_inv,n_inv,n_non,perdasTot,G.sc=1367,TempRef=25,IrrRef=1000,n_ref=0.9637,
                                         tempo.inicial="01/01/2015 00:00", tempo.final="31/12/2015 23:00", passo=60, Hora_do_passo="Mais_tarde",
                                         AlbedoFixo=0.2,TipoAlbedo="fixo",VVFixo=1.7,TipoVV="fixo",TaFixo=25,TipoTA="fixo",modo_radiacao="ceu_limpo", ModeloSintese="estocastico",ModeloComposicao="Collares",
                                         TipoDeClima,TipoModulo_Montagem,DadosMensais, Rastreamento="fixo"){
   
  g<-geo.trackerNREL(lat, long, long.st, Beta, gama, passo, Hora_do_passo, tempo.inicial, tempo.final, Rastreamento) #saida ok
  
  r<-Rad.Extraterrestre(g$tempos, g$theta.z, g$HaSol, G.sc) #saida ok
  
  c<-CeuLimpo(A,TipoDeClima,g$theta.z,g$HaSol, r$G.on, r$G.o) #saida ok

  s<-Sintetico(g$omega.sunset,g$omega.t,g$omega.b_delta.t,
               DadosMensais, r$G.on, g$delta, g$omega.1, g$omega.2,
               lat, g$HaSol, g$tempos, ModeloComposicao,
               ModeloSintese, g$omega.t_delta.t, g$theta.z,
               c$tau_b, c$tau_d, c$K.T.c) # Orgill Estocastico semi-validado
  
  T.Sintetico<-TemperaturaSintetico(g$tempos,DadosMensais)

  # Possivelmente alterar
   #podem ser adicionadas mais entradas de acordo com outras entradas de dado
    
  a<-Ambientais(modo_radiacao, c$G.cnb, c$G.cd, g$theta.z, r$G.o,   #K.T tem NaNs. To Do: Corrigir.
                DadosAmbientais, g$HaSol,
                AlbedoFixo, TipoAlbedo, VVFixo, TipoVV, TaFixo, TipoTA,
                s$Ibn, s$Id, T.Sintetico, g$tempos, DadosMensais)
  
  pl<-Pl.Inc_DirRef(g$theta,g$theta.z,g$HaSol,a$ro.g,Beta, a$G.b, a$G)
  
#  hdkr<-HDKR(g$theta,g$theta.z,a$G.nb,a$G.b,a$G.d,a$G, Beta, pl$R.b,r$G.on,r$G.o) # to do #' @export
#  liu<-Isotropico(a$G.d,Beta)
  i<-Perez(a$G.d, a$G.nb, g$theta, g$theta.z, r$G.on, Beta)
  
  G.t<-i$G.Td+pl$G.T.b+pl$G.T.refl
  
  #write.table(G.t, file="perez_R.csv", append="FALSE",sep=";", dec=",")
 
  p<-CaracteristicPainel(TipoDePainel)

  e<-RadiacaoEfetiva(p$eta.Ar,p$eta.VidroAR, p$eta.Vidro,g$theta,pl$G.T.b,i$G.Td,pl$G.T.refl)
  
  m<-ModelagemTermica(TipoModulo_Montagem)
  
  ###Pac!!?!
  ce<-ConversaoEnergetica(perdasTot,passo,TempRef,p$TempCoef,n_inv*pot_inv,Pdc0,IrrRef,e$G.ef.T,a$TA, m$a, m$b, m$DeltaT, a$VV ,n_non/n_ref, n_non, g$Delta.T)
  
 # saida<-list("G.t"=G.t,"E"=E, "s"=s, "a"=a, "pl"=pl, "i"=i,"e"=e, "T.Sin"=T.Sintetico)
  saida2<-data.frame("tempos"=g$tempos,
                     "HaSol"=g$HaSol,
                     "theta"=g$theta,
                     "G.on"=r$G.on,
                     "G.nb"=a$G.nb,
                     "G.d"=a$G.d,
                     "G"=a$G,
                     "G.t"=G.t,
                     "Ta"=a$TA,
                     "WS"=a$VV,
                     "G.ef"=e$G.ef.T,
                     "Tm"=ce$Tm,
                     "E"=ce$E,
    #                 "liu"=liu,
#                  "hay"=hdkr,
                     "perez"=i,
                     "E.dc"=ce$E_dc)
  

  
  return(saida2)
  
}

#' @export
head.list <- function(obj, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  origN <- n
  n <- if (n < 0L)
    max(length(obj) + n, 0L)
  else min(n, length(obj))
  lapply(obj[seq_len(n)], function(x)
  {
    tryCatch({
      head(x, origN, ...)
    }, error = function(e) {
      x
    })
  })
}
environment(head.list) <- asNamespace('utils')