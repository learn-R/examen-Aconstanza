#1. Carga de paquetes y datos
pacman::p_load(tidyverse, 
               sjPlot, 
               sjmisc, 
               survey, 
               srvyr, 
               magrittr)
datos_proc <- readRDS(file ="output/data/datos_proc.rds")
enusc_design <- as_survey_design(datos_proc, ids = 1, weights = Fact_Pers)

frq(datos_proc$cvd)
datos_proc %>% 
  select(VP_DC, percepcion) %>% 
  sjPlot::tab_corr(., corr.method =  "pearson", 
                   title = "Tabla N°1. Matriz de Correlación",
                   triangle = "lower",
                   var.labels = c(c("Victimización","Percepción de inseguridad")),  
                   digits = 3,
                   encoding = "UTF-8",
                   file = "output/figures/matrizcorrelación.doc")
plot_scatter(
  datos_proc,
  identidad_gen,
  VP_DC,
  cvd,
  title = "Grafico N°1: temor a ser víctima de algún delito según identidad de género",
  jitter = 0.05)

modelo1 <- lm(percepcion ~ VP_DC + cvd + edad,
              data = datos_proc,
              weights = Fact_Pers)
modelo1survey <- svyglm(percepcion ~ VP_DC + cvd + edad,
                        design = enusc_design,
                        family = gaussian,
                        weights = Fact_Pers)

modelo2 <- lm(percepcion ~ VP_DC + cvd + identidad_gen,
              data = datos_proc,
              weights = Fact_Pers)
modelo2survey <- svyglm(percepcion ~ VP_DC + cvd + identidad_gen,
                        design = enusc_design,
                        family = gaussian,
                        weights = Fact_Pers)
modelo3 <- glm(percepcion ~ VP_DC + cvd + identidad_gen + edad,
               data = datos_proc, 
               family = gaussian)
sjPlot::tab_model(list(modelo1,modelo2),
                  string.intercept = "(Intercepto)",
                  string.pred = "Predictores",
                  string.est = "Estimación",
                  p.style = "stars",
                  show.ci = F,
                  digits = 3,
                  dv.labels = c("Modelo 1", "Modelo 2"),
                  show.reflvl = T,
                  encoding = "UTF-8")
datos_proc %>% 
  filter(percepcion <= 1) %>% 
  plot_frq(identidad_gen,
           title = "Gráfico N°2: Percepción de aumento de inseguridad según identidad de género",
           type = c("bar"))


sjPlot::plot_model(modelo2, type = c("est"),
                   show.intercept = T,
                   show.values = T,
                   show.p = T,
                   show.ci= F,
                   df.method = 'wald',
                   digits = 3,
                   encoding = "UTF-8",
                   title = "Gráfico N°3: Modelo de regresión lineal para estimar la percepcion de inseguridad según identidad de género")

