#' @export
#' 
ModelagemTermica <-function(TipoModulo_Montagem) {
  if (TipoModulo_Montagem=="Polimero/filme-fino/aco_Estrutura"){
    a<- -3.58
    b<- -0.113
    DeltaT<-3
  } else if (TipoModulo_Montagem=="Vidro/celula/polimero_Estrutura"){
    a<- -3.56
    b<- -0.075
    DeltaT<-3
  } else if (TipoModulo_Montagem=="Vidro/celula/polimero_Telhado"){
    a<- -2.81
    b<- -0.0455
    DeltaT<-0
  } else if (TipoModulo_Montagem=="Vidro/celula/vidro_Estrutura"){
    a<- -3.47
    b<- -0.0594
    DeltaT<-3
  } else if (TipoModulo_Montagem=="Vidro/celula/vidro_Telhado"){
    a<- -2.98
    b<- -0.0471
    DeltaT<-1
  }
  
  saida<-list("a"=a,"b"=b, "DeltaT"=DeltaT)
  
}
