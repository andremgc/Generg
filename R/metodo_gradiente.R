#' Gradient method
#' 
#' This function uses the gradient method to find an opmal x =c(Beta, gama), minimizes fcusto
#' @export

grad<-function(Beta_gama0,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE){
    Beta_gama0=c(10,180)
  # Contador de iterações do método
  k = 0
  
  # estima o gradiente no ponto inicial
  gr = gradiente(Beta_gama0,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE);    
  
  # Determina a direção de busca, como quer-se maximizar, d=g ao inves do usual d=-g
  d = gr;                     
  d = d#/sqrt(sum(d^2))
  
  x = Beta_gama0;
  alpha<-1
                    
  for (i in 1:40){            
  x = x + alpha*d;            # Determina a nova solução  
  gr = gradiente(x,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE)  
  d = gr;
  if(sqrt(sum(d^2))<0.5)
    d = d/(sqrt(sum(d^2))+0.15)
  #alpha<-alpha/(0.2)
  }
}
