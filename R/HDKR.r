HDKR <- function(theta,theta.z,G.nb,G.b,G.d,G, Beta, R.b, G.on, G.o) {
  
  A.i.c<-G.b/G.o
  A.i.c[is.nan(A.i.c)]<-0
  
  f<-sqrt(G.b/G)
  f[is.nan(f)]<-0
  
  G.Td.cs<-G.d*A.i.c*R.b
  G.Td.iso<-G.d*(1-A.i.c)*(1+cos(Beta*pi/180))/2*(1+f*sin(Beta/2*pi/180)*sin(Beta/2*pi/180)*sin(Beta/2*pi/180))
  G.Td.hz<-0
  
  G.Td<-G.Td.hz+G.Td.iso+G.Td.cs
  
  saida<-list("G.Td"=G.Td,"G.Td.hz"=G.Td.hz,"G.Td.iso"=G.Td.iso,"G.Td.cs"=G.Td.cs)
  return(saida)
  
}
