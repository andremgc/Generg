#' Brute Force
#' 
#' This function makes a sweep trough gama and Beta
#' and stores value of energy and value generated. No output yet.
#' Until this point, this function is beeing used mannualy.
#' 
#' @export

fbruta<-function(g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE){ 
  Melhor<-0
  cont<-0
  

  x=seq(-179,179, by=2)
  y=seq(0,50, by=2)
  Resultado=matrix(nrow=length(x),ncol=length(y))
  ResultadoE=matrix(nrow=length(x),ncol=length(y))
  
  for(Beta in y){
    for(gama in x){
      Beta_gama<-c(Beta,gama)
      
      Gerado <-fCusto2(Beta_gama,g,a,r,p,m,perdasTot,passo,TempRef,n_inv,pot_inv,Pdc0,IrrRef,n_non,n_ref,PE)  
      
      Valor<-Gerado$V
      Energia<-Gerado$E
      
      if (Valor>Melhor) {
        Melhor<-Valor
        param<-Beta_gama
      }
      
      Resultado[which(x==gama),which(y==Beta)]=Valor
      ResultadoE[which(x==gama),which(y==Beta)]=Energia
      
      cont<-cont+1
      print(cont)
      print(cont/length(Dados[,1]))
      print(proc.time() )
      
    }
  }
  
}

# win.metafile('BH_E_3d.wmf')
# persp3D(x=x,y=y,z=ResultadoE,xlab="\n \n azimute [graus]",ylab="\n \n Inclinação", clab = "Energia [kWh]",
#         zlab= "\n \n Energia gerada anual [kWh]", theta = 140, phi = 40, ticktype = "detailed")
# dev.off()
# 
# win.metafile('BH_V_3d.wmf')
# persp3D(x=x,y=y,z=Resultado,xlab="\n \n azimute [graus]",ylab="\n \n Inclinação", clab = "Valor [R$]",
#         zlab= "\n \n Valor da energia anual [R$]", theta = 140, phi = 40, ticktype = "detailed")
# dev.off()
# 
# win.metafile('PV_E_2d.wmf')
# image2D(x=x,y=y,z=ResultadoE,xlab="\n \n azimute [graus]",ylab="\n \n Inclinação", clab = "Energia [kWh]",
#         ticktype = "detailed")
# dev.off()
# 
# win.metafile('BH_V_2d.wmf')
# image2D(x=x,y=y,z=Resultado,xlab="\n \n azimute [graus]",ylab="\n \n Inclinação", clab = "Valor [R$]",
#         ticktype = "detailed")
# dev.off()
# 
# 
# 
# PVelhoE<-ResultadoE
# PVelho<-Resultado
# PVM<-Melhor
# PVp<-param