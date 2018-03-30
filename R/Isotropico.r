Isotropico <- function(G.d, Beta) {
  
  G.Td.cs<-0
  G.Td.iso<-G.d*(1+cos(Beta*pi/180))/2
  G.Td.hz<-0
  
  G.Td<-G.Td.hz+G.Td.iso+G.Td.cs
  
  saida<-list("G.Td"=G.Td,"G.Td.hz"=G.Td.hz,"G.Td.iso"=G.Td.iso,"G.Td.cs"=G.Td.cs)
  return(saida)
}
