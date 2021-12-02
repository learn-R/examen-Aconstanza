# "Examen. Proyecto de investigaci?n final"

# Descripci?n general

# Examen Final

La base de datos utilizada para este estudio corresponde a la ENUSC 2020 realizada por el INE [ENUSC hacer click aquí](https://ine.cl/docs/default-source/seguridad-ciudadana/bbdd/2020/base-usuario-17-enusc-2020-sav.sav?sfvrsn=5352415b_4&amp;download=true)

## Objetivo

Los resultados de este estudio sirven como insumo para la redacción de un análisis comparado de seguridad pública de mis actividades de práctica en el departamento de seguridad pública de la municipalidad de Rancagua, por esta razón sole se incluiran datos de personas residentes en las regiones de Atacama, Metropolitana, O'Higgins, Concepción y La Araucanía. 

De este modo las variables a seleccionar se basan en la dimensión subjetiva de la inseguridad ciudadana: esto incluye: factores individuales (mujeres y personas mayores), factores físicos-territoriales (la presencia de incivilidades y violencias en el entorno), factores socio-barriales (la llegada de “otros” ajenos al barrio o de otros que se ven como diferentes), factores institucionales (la desconfianza en las autoridades, justicia y policías), factores culturales (el individualismo, la falta de empatía y la desconfianza en el otro) y, factores delictuales (presencia de delitos violentos y la presencia de tráfico de drogas en los barrios). 

Al no tener todas las variables que coincidan con estos factores, se escogeran aquellas relacionadas a factores individuales.

#Flujo
Existen 3 carpetas dentro de este R.proj. ´input´, ´output`, `R`. 
La carpeta input a su vez contiene la carpeta "data", donde se aloja la base de datos original y el manual de usuario de la base de datos.
En la carpeta ´R´se encuentran los archivos ´datos_proc.R´ que configura el procesamiento de los datos.
En la carpeta "output", se encuentran dos carpetas más: "data" y "figures". La carpeta "data" aloja la base de datos procesada.
Volviendo a la carpeta "R", podemos encontrar el archivo "02analisis.R" que configura el paso inicial para generar el reporte.
Y finalmente encontramos "02-analisis.Rmd" que es el reporte final y "02-analisis.html" que se configura como su salida en formato html.  

