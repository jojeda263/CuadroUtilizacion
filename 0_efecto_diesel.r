# EFECTO CUADRO UTILIZACIÓN CUENTAS NACIONALES

library(dplyr)
library(survey)
library(haven)
library(janitor)
library(openxlsx)
library(gglorenz)
library(purrr)

#Llamar cuadro

setwd("D:/JROO/Data/PIB/Sectores/CuadroUtilizacion")

#llamamos cuadro utilización
cou <- read.xlsx("4.1.2.xlsx", sheet = "mod",cols = 2:198,rows=13:229,colNames = TRUE)  |> 
        tidyr::pivot_longer(cols = -c(1:2), names_to = "industria", values_to = "valor")  |>
        mutate(cod_ind=substr(industria, 1, 6),industria_n=substr(industria, 8,max(nchar(industria))))  |> 
        mutate(industria_n=gsub("\\.", " ", industria_n))  |> 
        select(-industria)  |> 
        filter(valor!=0)

#guardamos totales de consumo intermedio en otro objetos
ConsInter <- read.xlsx("4.1.2.xlsx", sheet = "mod",cols = 4:198,rows=c(13,230),colNames = F) |> t() |> as.data.frame() 
 names(ConsInter)<- c("industria","total")
ConsInter <- ConsInter  |> mutate(cod_ind=substr(industria, 1, 6),industria_n=substr(industria, 8,max(nchar(industria))))  |>  select(-industria)  |>  mutate(total=as.numeric(total))
        
#juntamos totales de consumo intermedio con el cuadro de utilización
cou <- cou  |> left_join(select(ConsInter,total,cod_ind), by="cod_ind")  |> 
        mutate(ef_diesel=ifelse(cod=="180103",valor,0))  |> 
        mutate(ef_transporte=ifelse(cod=="280301",valor*0.563341,0))  |>
        mutate(ef_parcial=ef_diesel+ef_transporte) 

#calculamos el efecto una primera ronda de insumo que no son diesel o transporte
matriz_insumos  <- cou  |>  select(cod_ind,total,ef_parcial)  |> group_by(cod_ind)  |> summarise(cod_ind=first(cod_ind),total=mean(total),ef_parcial=sum(ef_parcial))  |> 
        mutate(efecto=ef_parcial/total)

cou  <- cou |> left_join(select(matriz_insumos,cod_ind,efecto), by=c("cod"="cod_ind")) |>
    mutate(ef_parcial=case_when(
        efecto>0 & ef_parcial==0 & cod!=cod_ind ~ efecto*valor,
        ef_parcial!=0 ~ ef_parcial,
        .default = 0)) |> 
    mutate(act_eco=substr(cod_ind, 1, 2))

efecto_intermedio <- cou  |> group_by(cod_ind)  |> summarise(efecto_int=sum(ef_parcial),act_eco=first(act_eco),cod_ind=first(cod_ind),total=first(total),industria_n=first(industria_n))  |> mutate(efecto_diesel=efecto_int/total*100)

efecto_final <- efecto_intermedio |> group_by(act_eco)  |> summarise(efecto_int=sum(efecto_int),act_eco=first(act_eco),total=sum(total))  |> mutate(efecto_diesel=efecto_int/total*100)  |> 
mutate(industria_n=case_when(
act_eco=="01" ~ "Agricultura y actividades de servicios conexos",
act_eco=="02" ~ "Agricultura de la coca",
act_eco=="03" ~ "Ganadería y actividades de servicios conexos",
act_eco=="04" ~ "Silvicultura,  pesca, caza y actividades de servicios conexos",
act_eco=="05" ~ "Extracción de petróleo crudo y gas natural, actividades de apoyo para la extraccion de petroleo y gas natural",
act_eco=="06" ~ "Extracción minera",
act_eco=="07" ~ "Elaboracion de carnes frescas y elaboradas",
act_eco=="08" ~ "Elaboración de productos lácteos",
act_eco=="09" ~ "Elaboracion de productos de molinería, panadería y beneficiado",
act_eco=="10" ~ "Elaboracion azucar y melazas",
act_eco=="11" ~ "Elaboración de aceites de origen vegetal y animal; grasas no comestibles de origen animal",
act_eco=="12" ~ "Elaboración de productos alimenticios diversos",
act_eco=="13" ~ "Elaboración de bebidas y productos de tabaco",
act_eco=="14" ~ "Fabricación de productos textiles",
act_eco=="15" ~ "Producción de madera y fabricación de productos de madera y corcho, excepto muebles; fabricación de artículos de paja y de materiales trenzables",
act_eco=="16" ~ "Fabricación  de papel y productos de papel; impresión",
act_eco=="17" ~ "Fabricación  de productos de refinación del petróleo y otros combustibles.",
act_eco=="18" ~ "Fabricación de sustancias y productos químicos, productos farmacéuticos, productos de caucho y plásticos",
act_eco=="19" ~ "Fabricación de productos de minerales no metálicos",
act_eco=="20" ~ "Fabricación de metales comunes",
act_eco=="21" ~ "Fabricación de maquinaria y equipo",
act_eco=="22" ~ "Fabricación de productos manufacturados diversos",
act_eco=="23" ~ "Generacion de eléctricidad y servicios de distribucion de electricidad,  agua y recolección de desechos sólidos",
act_eco=="24" ~ "Construcción de edificios y actividades especializadas de construccion",
act_eco=="25" ~ "Construcción de obras civiles",
act_eco=="26" ~ "Comercio, reparacion de vehiculos automotores y motocicletas",
act_eco=="27" ~ "Transporte y almacenamiento",
act_eco=="28" ~ "Actividades de alojamiento y de servicio de comidas y bebidas",
act_eco=="29" ~ "Comunicación e información",
act_eco=="30" ~ "Actividades financieras y de seguros",
act_eco=="31" ~ "Actividades inmobiliarias ",
act_eco=="32" ~ "Actividades profesionales, cientificas y tecnicas, actividades de servicios administrativos y de apoyo",
act_eco=="33" ~ "Administracion publica",
act_eco=="34" ~ "Educación de mercado",
act_eco=="35" ~ "Educación de no mercado",
act_eco=="36" ~ "Salud de mercado",
act_eco=="37" ~ "Salud de no mercado",
act_eco=="38" ~ "Actividades comunales, sociales y personales",
act_eco=="39" ~ "Actividades de los hogares como empleadores de personal domestico"))

write.xlsx(efecto_final, "efecto_acteco_agregado.xlsx", rowNames = F)
write.xlsx(efecto_intermedio, "efecto_acteco_desagregado.xlsx", rowNames = F)