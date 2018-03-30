#' @export

Otimo<-function(long,long.st,lat,A,
                                        DadosAmbientais,Pdc0,Beta,gama,
                                         TipoDePainel,pot_inv,n_inv,n_non,perdasTot,G.sc=1367,TempRef=25,IrrRef=1000,n_ref=0.9637,
                                         tempo.inicial="01/01/2015 00:00", tempo.final="31/12/2015 23:00", passo=60, Hora_do_passo="Mais_tarde",
                                         AlbedoFixo=0.2,TipoAlbedo="fixo",VVFixo=1.7,TipoVV="fixo",TaFixo=25,TipoTA="fixo",modo_radiacao="ceu_limpo", ModeloSintese="estocastico",ModeloComposicao="Collares",
                                         TipoDeClima,TipoModulo_Montagem,DadosMensais,PE){
  
  p<-CaracteristicPainel(TipoDePainel)
  
  m<-ModelagemTermica(TipoModulo_Montagem)
   
  g<-geometrico(lat, long, long.st, Beta, gama, passo, Hora_do_passo, tempo.inicial, tempo.final) #saida ok
  
  r<-Rad.Extraterrestre(g$tempos, g$theta.z, g$HaSol, G.sc) #saida ok
  
  c<-CeuLimpo(A,TipoDeClima,g$theta.z,g$HaSol, r$G.on, r$G.o) #saida ok

  s<-Sintetico(g$omega.sunset,g$omega.t,g$omega.b_delta.t,
               DadosMensais, r$G.on, g$delta, g$omega.1, g$omega.2,
               lat, g$HaSol, g$tempos, ModeloComposicao,
               ModeloSintese, g$omega.t_delta.t, g$theta.z,
               c$tau_b, c$tau_d) # Orgill Estocastico semi-validado
  
  T.Sintetico<-TemperaturaSintetico(g$tempos,DadosMensais)

  a<-Ambientais(modo_radiacao, c$G.cnb, c$G.cd, g$theta.z, r$G.o,   #K.T tem NaNs. To Do: Corrigir.
                DadosAmbientais, g$HaSol,
                AlbedoFixo, TipoAlbedo, VVFixo, TipoVV, TaFixo, TipoTA,
                s$Ibn, s$Id, T.Sintetico, g$tempos, DadosMensais)
  
  Beta_gama<-c(Beta,gama)
  
  Energia<-fCusto(Beta_gama,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE)  
  
  ans<-optim(par=c(lat,180),fn=fCusto,g=g,a=a,r=r,p=p,m=m,perdasTot=perdasTot,passo=passo,TempRef=TempRef,
              n_inv=n_inv,pot_inv=pot_inv,Pdc0=Pdc0,IrrRef=IrrRef,n_non=n_non,n_ref=n_ref,PE=PE)
  
  return(Energia)
  
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