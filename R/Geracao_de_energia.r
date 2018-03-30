Geracao_de_Energia<-function(long,long.st,lat,A,DadosAmbientais,Pdc0,Beta,gama,TipoDePainel,pot_inv,n_inv,n_non,perdasTot,Pac0,G.sc,TempRef,IrrRef,n_ref,tempo.inicial,tempo.final,passo,Hora_do_passo,AlbedoFixo,TipoAlbedo,VVFixo,TipoVV,TaFixo,TipoTA,modo_radiacao,TipoDeClima,TipoModulo_Montagem){
  
  saida.g<-geometrico(lat, long, long.st, Beta, gama, passo, Hora_do_passo, tempo.inicial, tempo.final)
  
  attach(saida.g)
  
  saida.r<-Rad.Extraterrestre(tempos, theta.z, HaSol)
  
  attach(saida.r)
  
  saida.c<-CeuLimpo(A,TipoDeClima,theta,theta.z)
  
  attach(saida.c)
  
  saida.a<-Ambientais(modo_radiacao, G.cnb, G.cd, theta.z, G.o, DadosAmbientais, HaSol, AlbedoFixo, TipoAlbedo, VVFixo, TipoVV, TAFixo, TipoTA)
  
  attach(saida.a)
  
  saida.pl<-Pl.Inc_DirRef(theta,theta.z,HaSol,ro.g,Beta)
  
  attach(saida.pl)
  
  #hdkr<-HDKR(theta,theta.z,G.nb,G.b,G.d,G, Beta, R.b)
  #liu<-Isotropico(G.d,Beta)
  perez<-Perez(G.d, G.nb, theta, theta.z,G.on, Beta)
  
  G.t<-perez$G.Td+G.T.b+G.T.refl
  
  #write.table(G.t, file="perez_R.csv", append="FALSE",sep=";", dec=",")
  
  
  attach(perez)  
  saida.p<-CaracteristicPainel(TipoDePainel)
  
  attach(saida.p)
  
  saida.e<-RadiacaoEfetiva(eta.Ar,theta,G.c.T.b,G.c.T.d.cs,G.c.T.d.iso,G.c.T.d.hz,G.c.T.refl)
  
  attach(saida.e)
  
  saida.m<-ModelagemTermica(TipoModulo_Montagem)
  
  attach (saida.m)

  E<-ConversaoEnergetica(perdasTot,passo,TempRef,TempCoef,Pac0,Pdc0,IrrRef,G.ef.T,TA, a, b, DeltaT, VV ,n_non_sobre_n_ref, n_non, Delta.T)
  
  detach(f)
  detach(saida.m)
  detach(saida.e)
  detach(saida.p)
  detach(perez)
  detach(saida.pl)
  detach(saida.a)
  detach(saida.c)
  detach(saida.r)
  detach(saida.g)
  
  return(E)
  
}