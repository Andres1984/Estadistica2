coronavirus$nuevo=ifelse(coronavirus$Estado=="Fallecido",1,0) ### Columna Fallecido
ma=list() ## Creación de Lista
for(i in 1:100) {
  
  tmp=list(coronavirus[sample(nrow(coronavirus), 10),])
  ma=append(ma,tmp)
  
}  


library(dplyr)
new_list <- lapply(ma, function(x) x%>% select(nuevo))### Extraemos la lista
df <- data.frame(matrix(unlist(new_list), ncol=length(new_list)))### Se convierte en Data Frame
df1=colSums(df)/10## Se obtiene la proporción de cada muestra. 
