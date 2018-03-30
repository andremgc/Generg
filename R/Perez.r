#' @export
#' 
Perez <- function(G.d, G.nb, theta, theta.z,G.on, Beta){
  epsilon<-(((G.d+G.nb)/G.d)+(5.535e-6)*(theta*pi/180)^3)/(1+(5.535e-6)*(theta*pi/180)^3)
  epsilon[is.nan(epsilon)]<-0
  
  f<-CoeficientesPerez(epsilon)
  attach(f)
  
  Delta<-1/(cos(theta.z*pi/180))*G.d/G.on
  F1<-f11+f12*Delta+theta.z*pi/180*f13
  F1[F1<0]<-0
  F2<-f21+f22*Delta+theta.z*pi/180*f23
  b<-cos(theta.z*pi/180)
  b[b<(cos(85*pi/180))]<-(cos(85*pi/180))
  G.Td.cs<-G.d*(1-F1)*(1+cos(Beta*pi/180))/2
  G.Td.iso<-G.d*F1*cos(theta*pi/180)/b
  G.Td.hz<-G.d*F2*sin(Beta*pi/180)
  G.Td<-G.Td.hz+G.Td.iso+G.Td.cs
  
  saida<-list("G.Td"=G.Td,"G.Td.hz"=G.Td.hz,"G.Td.iso"=G.Td.iso,"G.Td.cs"=G.Td.cs)
  detach(f)
  return(saida)
  
}
