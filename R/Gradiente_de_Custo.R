#' @export

gradiente<-function(Beta_gama,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE){  

# Gradiente analítico da função 

delta = 1e-5;
n <- 2;
E = diag(n);
Fx= fCusto(Beta_gama,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE);
gr<-vector(length=2)

  gr[1] = (fCusto(Beta_gama+c(delta,0),g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE) - Fx)/delta;
  gr[2] = (fCusto(Beta_gama+c(0,delta),g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE) - Fx)/delta;
  
return(gr);

}