if (!require("openxlsx")) {install.packages("openxlsx")}
library(openxlsx)
paquetes <- c("tidyr","emayili")
cargar_paquetes(paquetes)


procesar_gs1 <- function(id_archivo){
  archivo <- airtable_getrecordslist("archivos",Sys.getenv("AIRTABLE_RIR_BASE"),paste0("id_archivo='",id_archivo,"'"))[[1]]
  if(archivo$fields$tipo=="GS1"){
    data <- read.xlsx(archivo$fields$archivo[[1]]$url)
    if(length(data)==25){
      for(i in 1:length(data$ID)){
        
        producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_RIR_BASE"),
                                            paste0("sku=",data[i,]$Código.Interno),c("sku"))[[1]]
        #producto <- airtable_getrecordslist("productos",Sys.getenv("AIRTABLE_CES_BASE"),paste0("sku=",data[i,]$Código.Interno),c("sku"))[[1]]
        fields <- list(
          'gs1_code' =paste0(data[i,]$Consecutivo,data[i,]$Dígito.Verificador),
          #'codigo_consecutivo_gs1'=paste0("0",data[i,]$Consecutivo),
          'codigo_completo_gs1'=data[i,]$Código.Completo
        )
        airtable_updatesinglerecord(fields,"productos",Sys.getenv("AIRTABLE_CES_BASE"),producto$id)
        
      }
      airtable_updatesinglerecord(list('Status'="Procesado"),"archivos",Sys.getenv("AIRTABLE_RIR_BASE"),archivo$id)
    }
  }
  
} 