#' Geracao_de_Energia_Rastreada
#'
#' This function executes the whole simulation of the sun generation
#' It can take many forms of inputs
#'
#' @author André Costa
#'
#' @param long longitude in degrees.
#' Negative values for south. Acceped values in between -180 and 180
#' @param long.st standard longitude of the timezone.
#' @param lat latitde in degrees.
#' Negative values for west. Accepted values in betwwen -60 and 60
#' @param A Altitude. Used to calculate clear-sky radiation
#' @param DadosAmbientais Data frame containing the hourly
#' environmental input data, if any is used
#' @param Pdc0 nominal dc power of the system
#' @param Beta Declination angle in degrees.
#' Accepted values in betwwen 0 and 90
#' @param gama Orientation angle. N=0, E=90, S=180, W=270
#' @param TipoDePainel Type of pannel. Accepted inputs:
#' "Cristalino Premium"; "Cristalino Padrao";"Filme fino"
#' Other values will be considered as "outro". Used in
#' CaracteristicPainel(TipoDePainel)
#' @param pot_inv Inverter nominal power
#' @param n_inv number of inverters
#' @param n_nom Nominal Efficiency
#' @param perdasTot Total external losses.
#' @param G.sc Solar Constant
#' @param TempRef Reference temperature of modules
#' @param IrrRef Reference irradiation on the module
#' @param n_ref efficiency reference
#' @param tempo.inicial Initial time of simulation
#' @param tempo.final Final time of simulation
#' @param passo simulation step in minutes
#' @param Hora_do_passo when to consider the value for the
#' simultion step. Available options:
#' "Mais_tarde", "Mais_cedo", "Exata"
#' @param AlbedoFixo Value for albedo if it is set as fixed
#' @param TipoAlbedo Type of albedo.
#' Available options:
#' "fixo", "mensal", "horario"
#' @param VVFixo Value for wind-speed if it is set as fixed
#' @param TipoVV Type of wind-speed.
#' Available options:
#' "fixo", "mensal", "horario"
#' @param TAFixo Value for Environment Temperature if it is set as fixed
#' @param TipoAlbedo Type of Environment Temperature.
#' Available options:
#' "fixo", "mensal", "horario"
#' @param modo_radiacao Type of radiation data used.
#' Accepted values:
#' "ceu_limpo", "Kt_mes", "H_med", "horario"
#' @param ModeloSintese
#' "KTmedio","LiuJordanModificado","Liu","Knight","Whillier","WhillierModificado"
#' @param ModeloComposicao
#' "Orgill","Erbs","Collares","CollaresD"
#' @param TipoDeCilma clime type, used to create clear sky radiations.
#' "Lat.intermed./Inverno","Lat.intermed./Verao","Tropical","Verao Subartico"
#' @param TipoModulo_Montagem type of mount of system. used in thermal moddeling
#' "Polimero/filme-fino/aco_Estrutura","Vidro/celula/polimero_Estrutura",
#' "Vidro/celula/polimero_Telhado","Vidro/celula/vidro_Estrutura","Vidro/celula/vidro_Telhado"
#' @param DadosMensais Monthly dada input dataframe, if used
#' @param TemperaturaConstante 1 if there is no sthochastic component on sinthetic temperature
#' @param Rastreamento type of tracking.
#' "fixo","1eixo","2eixos","azimutal"
#'
#' @export


Geracao_de_Energia_Rastreada<-function(long,long.st,lat,A,
                                         DadosAmbientais,Pdc0,Beta,gama,
                                         TipoDePainel,pot_inv,n_inv=1L,
                                         n_non,perdasTot,G.sc=1367,TempRef=25,IrrRef=1000,n_ref=0.9637,
                                         tempo.inicial="01/01/2015 00:00", tempo.final="31/12/2015 23:00",
                                         passo=60, Hora_do_passo="Mais_tarde",
                                         AlbedoFixo=0.2,TipoAlbedo="fixo",VVFixo=1.7,TipoVV="fixo",
                                         TaFixo=25,TipoTA="fixo",modo_radiacao="ceu_limpo",
                                         ModeloSintese="Liu",ModeloComposicao="Collares",
                                         TipoDeClima,TipoModulo_Montagem,DadosMensais,
                                         TemperaturaConstante, Rastreamento){

  g<-geo.trackerNREL (lat, long, long.st, Beta, gama, passo, Hora_do_passo, tempo.inicial, tempo.final,Rastreamento) #saida ok

  r<-Rad.Extraterrestre(g$tempos, g$theta.z, g$HaSol, G.sc) #saida ok

  c<-CeuLimpo(A,TipoDeClima,g$theta.z,g$HaSol, r$G.on, r$G.o) #saida ok

  s<-Sintetico(g$omega.sunset,g$omega.t,g$omega.b_delta.t,
               DadosMensais, r$G.on, g$delta, g$omega.1, g$omega.2,
               lat, g$HaSol, g$tempos, ModeloComposicao,
               ModeloSintese, g$omega.t_delta.t, g$theta.z,
               c$tau_b, c$tau_d, c$K.T.c) # Orgill Estocastico semi-validado

  T.Sintetico<-TemperaturaSintetico(g$tempos,DadosMensais,g$tempos.solar,TemperaturaConstante)

  # Possivelmente alterar

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
                     "G.t.refl"=pl$G.T.refl,
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
