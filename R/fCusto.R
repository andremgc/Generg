#' The difference between fCusto and fCusto2, is that fCusto2 returns, also, the value of generated energy
#'  @export

fCusto<-function(Beta_gama,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE){  
  
  Beta<-Beta_gama[1]
  gama<-Beta_gama[2]
  
  theta<-ftheta(Beta_gama,g$theta.z,g$gama.s)
  
  pl<-Pl.Inc_DirRef(theta,g$theta.z,g$HaSol,a$ro.g,Beta, a$G.b, a$G)

  i<-Perez(a$G.d, a$G.nb, theta, g$theta.z, r$G.on, Beta)
  
  G.t<-i$G.Td+pl$G.T.b+pl$G.T.refl
  
  e<-RadiacaoEfetiva(p$eta.Ar,p$eta.VidroAR, p$eta.Vidro,theta,pl$G.T.b,i$G.Td,pl$G.T.refl)
  
  ce<-ConversaoEnergetica(perdasTot,passo,TempRef,p$TempCoef,n_inv*pot_inv,Pdc0,IrrRef,e$G.ef.T,a$TA, m$a, m$b, m$DeltaT, a$VV ,n_non/n_ref, n_non, g$Delta.T)
  
  gerado<-sum(PE*ce$E)
  
}
