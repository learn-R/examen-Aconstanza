#Procesamiento de datos examen 

#1. Carga de paquetes
pacman::p_load(tidyverse,
               sjmisc,
               haven,
               magrittr,
               car,
               sjlabelled) #recodificación
    

#2. Carga de datos
datos <- read_sav("input/data/base-usuario-17-enusc-2020-sav.sav")

#3. Exploración de datos
View(datos)
str(datos)

find_var(datos, "Identidad")
frq(datos$rph_idgen)

find_var(datos, "Edad")
frq(datos$rph_edad)
descr(datos$rph_edad)

find_var(datos, "Victimización")
frq(datos$VP_DC) #Victimización personal delitos consumados
find_var(datos, "comuna")
frq(datos$P1_2_1) #Percepción de aumento de delincuencia en su comuna
find_var(datos, "delito")
frq(datos$P3_1_1) #Cree que será victima de algún delito
find_var(datos, "region")

frq(datos$enc_region)

descr(datos$Fact_Pers)

#4. Selección de variables
datos %>% 
  filter(enc_region %in% c(3, 6, 8, 9, 13) & Fact_Pers != "NA") %>% 
  select(identidad_gen = rph_idgen, edad = rph_edad, cvd = P3_1_1, VP_DC, percepcion = P1_2_1, Fact_Pers) %>% 
  mutate(identidad_gen = as_factor(car::recode(identidad_gen, c("c(88,96,99)=NA"))),
         edad = case_when(edad <=3 ~ "Joven",
                            edad >= 4 & edad <= 6 ~ "Adulto",
                            edad >= 7 ~ "Adulto mayor",
                            TRUE ~ NA_character_),
         cvd = as_factor(car::recode(cvd, c("c(88,96,99)=NA"))),
         percepcion = as_factor(car::recode(percepcion, c("c(88,96,99)=NA")))) %>% 
  mutate_at(vars(identidad_gen, edad, percepcion, VP_DC), funs(forcats::as_factor(.)))
# 5. Recodificación de variables
datos_proc <- datos %>% 
  filter(enc_region %in% c(3, 6, 8, 9, 13) & Fact_Pers != "NA") %>% 
  select(identidad_gen = rph_idgen, edad = rph_edad, cvd = P3_1_1, VP_DC, percepcion = P1_2_1, Fact_Pers) %>% 
  mutate(identidad_gen = as_factor(car::recode(identidad_gen, c("c(88,96,99)=NA"))),
         edad = case_when(edad <=3 ~ "Joven",
                          edad >= 4 & edad <= 6 ~ "Adulto",
                          edad >= 7 ~ "Adulto mayor",
                          TRUE ~ NA_character_),
         cvd = as.numeric(car::recode(cvd, c("c(88,96,99)=NA"))),
         percepcion = as.numeric(car::recode(percepcion, c("c(88,96,99)=NA"))),
         VP_DC = as.numeric(car::recode(VP_DC, c("c(96)=NA")))) %>% 
  mutate_at(vars(identidad_gen, edad), funs(forcats::as_factor(.)))

summary(datos_proc)

#6. Etiquetado de variables
datos_proc$identidad_gen <- set_label(datos_proc$identidad_gen, 'Identidad de género')
datos_proc$edad <- set_label(datos_proc$edad, 'Edad. 3 categorías')
datos_proc$cvd <- set_label(datos_proc$cvd, 'Cree que será victima de algún delito durante los próximos 12 meses')
datos_proc$VP_DC <- set_label(datos_proc$VP_DC, 'Victimización Personal de Delitos Consumados')
datos_proc$percepcion <- set_label(datos_proc$percepcion, 'percepción de aumento de los delitos en la comuna') 
str(datos_proc)
#7. Exportar datos
saveRDS(datos_proc, file= "output/data/datos_proc.rds")
