# =====================================================================
# Construccion de indicadores sinteticos - el IDHM en las 10 etapas de la OCDE
# Script R (version ejecutable del tutorial)
#
# Caio Cesar Soares Goncalves - Departamento de Demografia / CEDEPLAR-UFMG
# Workshop DEMOPOP 2026 - Licencia CC BY 4.0
# Site: https://demopop.github.io/indicadores-sinteticos/es/
#
# Requiere: readxl, dplyr, tidyr, ggplot2, sf, ggspatial (instalados
# automaticamente por el primer bloque). Mantenga la carpeta data/ en su
# directorio de trabajo y corra de arriba hacia abajo. Los comentarios explican cada paso.
# ---------------------------------------------------------------------
# ANTES DE CORRER: apunte el directorio de trabajo a la carpeta que contiene
# la subcarpeta data/ (con adh_radar_base_2012_2024.xlsx y uf_brasil.geojson).
#   En RStudio: menu Session > Set Working Directory > To Source File Location
#   (con este .R guardado al lado de la carpeta data/).
# O descomente la linea de abajo y ajuste la ruta:
# setwd("C:/ruta/a/indicadores-sinteticos")
# =====================================================================

# c() crea un vector con los nombres de los paquetes que vamos a usar:
# readxl (leer Excel), dplyr/tidyr (manipular tablas), ggplot2 (gráficos), sf (mapas),
# ggspatial (escala y rosa de los vientos en los mapas, Etapa 10)
pacotes <- c("readxl", "dplyr", "tidyr", "ggplot2", "sf", "ggspatial")

# setdiff() devuelve los paquetes de la lista que aún NO están instalados...
faltando <- setdiff(pacotes, rownames(installed.packages()))
# ...y, si hay alguno faltando, lo instala automáticamente
if (length(faltando) > 0) install.packages(faltando)

# carga (library) cada paquete de la lista; invisible() solo suprime la salida en la consola
invisible(lapply(pacotes, library, character.only = TRUE))

# read_excel() lee la planilla; sheet = "Total" elige la pestaña. El resultado se guarda
# (con la flecha <-, el operador de asignación de R) en el objeto 'radar': una tabla (data frame)
# con el panel 2012–2024 y varios recortes geográficos
radar <- read_excel("data/adh_radar_base_2012_2024.xlsx", sheet = "Total")

# filter() mantiene solo las filas que satisfacen las condiciones (año = 2024 Y recorte = UF);
# '==' prueba igualdad (note los dos signos de igual). Quedan las 27 UF de 2024
ufs <- filter(radar, ANO == 2024, AGREGACAO == "Unidade da Federação")

# vector con los códigos de las 4 columnas de FLUJO escolar, que reutilizaremos más adelante
cols_fluxo <- c("T_FREQ5A6", "T_FUND11A13", "T_FUND15A17", "T_MED18A20")

# glimpse() muestra un resumen de las columnas elegidas. Dentro de ufs[, c(...)], la coma
# separa [filas, columnas]: dejando vacío el lado de las filas, pedimos TODAS las filas y solo
# las columnas listadas
glimpse(ufs[, c("NOME", "ESPVIDA", "T_FUND18M", cols_fluxo, "RDPC", "IDHM")])

# dim() devuelve las dimensiones de la tabla: número de filas (UF) y de columnas (variables)
dim(ufs)

# lo que importa aquí son los indicadores que ALIMENTAN el índice — no las decenas de columnas
# de la base. Reunimos los 8 en un vector: longevidad, escolaridad, los 4 de flujo, ingreso y el
# IDHM oficial (este último para la reproducción en la Etapa 9)
cols_calc <- c("ESPVIDA", "T_FUND18M", cols_fluxo, "RDPC", "IDHM")

# is.na() marca como TRUE cada celda vacía; colSums() suma esos TRUE columna a columna
# (en R, TRUE cuenta como 1 y FALSE como 0), dando el nº de ausencias por variable.
# ufs[, cols_calc] restringe la verificación a esas 8 columnas
na_por_coluna <- colSums(is.na(ufs[, cols_calc]))

# filtra el resultado para mostrar solo las columnas (entre las del cálculo) con alguna ausencia
na_por_coluna[na_por_coluna > 0]

# cada regla es un vector lógico (TRUE donde la UF satisface la condición, FALSE donde falla).
# el & combina condiciones (Y lógico); >= y <= prueban los límites
regras <- list(
  renda_positiva    = ufs$RDPC > 0,                              # ingreso > 0: exigencia del log (Etapa 5)
  espvida_plausivel = ufs$ESPVIDA >= 50 & ufs$ESPVIDA <= 90,     # esperanza de vida en franja plausible
  escol_percentual  = ufs$T_FUND18M >= 0 & ufs$T_FUND18M <= 100, # escolaridad (%) dentro de [0,100]
  # apply(..., 1, all) verifica, fila a fila (margen 1 = UF), si TODOS los 4 indicadores
  # de flujo caen en [0,100]; all() exige que la condición valga para los cuatro a la vez
  fluxo_percentual  = apply(ufs[, cols_fluxo] >= 0 & ufs[, cols_fluxo] <= 100, 1, all)
)

# sapply recorre las reglas y cuenta, en cada una, cuántas UF FALLAN: !ok invierte el lógico
# (TRUE pasa a FALSE y viceversa) y sum() suma los TRUE. Cero en todas = base consistente
sapply(regras, function(ok) sum(!ok))

# función que devuelve TRUE para los valores fuera de la cerca del boxplot
eh_outlier <- function(x) {
  q <- quantile(x, c(.25, .75))     # 1er y 3er cuartiles (Q1 y Q3)
  iqr <- q[2] - q[1]                # amplitud intercuartílica
  x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr
}

# aplica la regla a cada indicador del cálculo y cuenta cuántas UF caen fuera de la cerca
indic <- data.frame(longevidade  = ufs$ESPVIDA,
                    escolaridade = ufs$T_FUND18M,
                    fluxo        = rowMeans(ufs[, cols_fluxo]),
                    renda        = ufs$RDPC)
sapply(indic, function(x) sum(eh_outlier(x)))

# ¿cuál UF es el único caso atípico (en longevidad)?
ufs$NOME[eh_outlier(ufs$ESPVIDA)]

# El análisis multivariado examina los indicadores SEPARADOS — incluso los 4 de flujo, que solo
# se combinarán en la Etapa 6. Tomar el promedio del flujo ya aquí escondería la redundancia entre ellos.
# El operador |> ("pipe") pasa ufs como 1er argumento de transmute(), que monta una tabla en la que
# cada columna es un indicador bruto (a la izquierda del '=', el nombre; a la derecha, la columna de la base):
indicadores <- ufs |>
  transmute(
    longevidade  = ESPVIDA,
    escolaridade = T_FUND18M,
    fluxo_5a6    = T_FREQ5A6,
    fluxo_11a13  = T_FUND11A13,
    fluxo_15a17  = T_FUND15A17,
    fluxo_18a20  = T_MED18A20,
    renda        = RDPC
  )

# cor() calcula la matriz de correlación entre las columnas; round(..., 2) redondea a 2 decimales.
# use = "complete.obs" manda ignorar eventuales ausencias en el cálculo
round(cor(indicadores, use = "complete.obs"), 2)

# alfa de Cronbach: k/(k-1) * (1 - suma de las varianzas de los ítems / varianza de la suma de los ítems)
alpha <- function(itens) {
  k <- ncol(itens)
  (k / (k - 1)) * (1 - sum(apply(itens, 2, var)) / var(rowSums(itens)))
}

# dimensión educación (stock + 4 flujo, todos en %), con y sin el flujo saturado de 5–6 años
c(com_5a6 = alpha(ufs[, c("T_FUND18M", cols_fluxo)]),
  sem_5a6 = alpha(ufs[, c("T_FUND18M", "T_FUND11A13", "T_FUND15A17", "T_MED18A20")])) |> round(3)

# prcomp() hace la ACP; scale.=TRUE estandariza los indicadores antes (si no, el ingreso, de mayor escala,
# dominaría). Reaprovecha el objeto 'indicadores' (las 7 columnas de la matriz de correlación)
pca <- prcomp(indicadores, scale. = TRUE)

round(summary(pca)$importance[2, 1:3], 3)   # proporción de la varianza resumida por PC1, PC2, PC3
round(pca$rotation[, 1], 2)                  # cargas (pesos) de los indicadores en el 1er componente

# scale() estandariza; dist() calcula las distancias entre las UF; hclust() las agrupa de forma
# jerárquica. En el dendrograma, cuanto más baja la unión, más parecidas son las UF
d  <- dist(scale(indicadores))
hc <- hclust(d, method = "ward.D2")
plot(hc, labels = ufs$NOME, main = NULL, xlab = "", ylab = "Distancia", sub = "", cex = 0.75)

# el signo de dólar ($) accede a una columna de la tabla por el nombre: ufs$ESPVIDA es el vector de esperanzas de vida
ev <- ufs$ESPVIDA

# las tres cuentas de abajo operan sobre el vector entero de una vez (R es "vectorizado":
# no hace falta bucle/loop para aplicar la fórmula a cada UF)
fixo     <- (ev - 25) / (85 - 25)                 # min-max con referencia fija (balizas del IDHM)
amostral <- (ev - min(ev)) / (max(ev) - min(ev))  # min-max con el mín/máx observados en la muestra
zscore   <- (ev - mean(ev)) / sd(ev)              # estandarización z-score (mean = promedio, sd = desviación estándar)

# data.frame() junta los tres vectores en columnas; summary() resume cada una (mín, promedio, máx...)
summary(data.frame(fixo, amostral, zscore))

# crea una NUEVA columna en ufs (ufs$i.longev) con la longevidad normalizada.
# min-max con referencia fija: baliza inferior 25, superior 85 años
ufs$i.longev <- (ufs$ESPVIDA - 25) / (85 - 25)

# misma idea que la longevidad, pero en escala logarítmica: log() es el logaritmo natural (ln).
# referencias (ya en log) R$ 8 y R$ 4.033, en reales constantes
ufs$i.renda <- (log(ufs$RDPC) - log(8)) / (log(4033) - log(8))

# cada subindicador ya está en porcentaje (0 a 100), así que dividir por 100 lo pone en [0,1]
ufs$si.escol <- ufs$T_FUND18M / 100                  # stock: escolaridad de la población adulta
ufs$si.fluxo <- rowMeans(ufs[, cols_fluxo]) / 100    # flujo: promedio de las 4 columnas de asistencia

# summary() de los cuatro subíndices ya normalizados, para verificar que quedaron todos en [0,1]
summary(ufs[, c("i.longev", "i.renda", "si.escol", "si.fluxo")])

# el acento circunflejo (^) es la potencia. Promedio geométrico ponderado = producto de los términos
# elevados a los pesos, con el exponente final 1/(suma de los pesos): aquí pesos 1 y 2, luego raíz cúbica
ufs$i.educ <- (ufs$si.escol^1 * ufs$si.fluxo^2)^(1/3)

# para comparar, el mismo promedio geométrico pero con pesos iguales (1 y 1 → raíz cuadrada)
educ_iguais <- (ufs$si.escol^1 * ufs$si.fluxo^1)^(1/2)

# monta una tabla con las dos versiones y la diferencia entre ellas...
data.frame(uf = ufs$NOME,
           educ_idhm = round(ufs$i.educ, 3),
           educ_iguais = round(educ_iguais, 3),
           dif = round(ufs$i.educ - educ_iguais, 3)) |>
  # ...ordena por la diferencia en valor absoluto (abs), de mayor a menor (desc), y muestra el top 8.
  # head(8) devuelve las 8 primeras filas
  arrange(desc(abs(dif))) |> head(8)

# Definimos NUESTRA propia función. function(...) lista los argumentos de entrada; los que tienen
# '=' ya traen un valor por defecto, usado cuando no los informamos en la llamada. Así, variar
# normalización, agregación y pesos se vuelve solo cambiar argumentos.
construir_indice <- function(dados,
                             agregacao = "geometrica",  # "geometrica" o "aritmetica"
                             pesos = c(1, 1, 1)) {       # pesos de longevidad, educación, ingreso
  # apodos cortos para los tres subíndices (L, E, R), solo para que la fórmula quede legible
  L <- dados$i.longev; E <- dados$i.educ; R <- dados$i.renda

  # normaliza los pesos para que sumen 1 (w[1] es el 1er elemento del vector w, w[2] el 2º, etc.)
  w <- pesos / sum(pesos)

  # if/else elige la regla: producto de potencias (geométrica) o suma ponderada (aritmética)
  indice <- if (agregacao == "geometrica") L^w[1] * E^w[2] * R^w[3]
            else                            w[1]*L + w[2]*E + w[3]*R

  # la función devuelve una tabla con UF, índice y posición en el ranking.
  # rank(-indice) ordena de forma decreciente (el '-' invierte: mayor índice = 1er lugar);
  # ties.method = "min" da la misma posición (la menor) a los empates
  data.frame(uf = dados$NOME,
             indice = round(indice, 4),
             rank = rank(-indice, ties.method = "min"))
}

# llamada con los valores por defecto (geométrica, pesos iguales) = el IDHM oficial; guardamos solo la columna 'indice'
ufs$idhm <- construir_indice(ufs)$indice

# calcula el índice en las dos reglas de agregación, cambiando solo el argumento 'agregacao'
geo <- construir_indice(ufs, agregacao = "geometrica")
ari <- construir_indice(ufs, agregacao = "aritmetica")

comparar <- geo |>
  rename(rank_geo = rank, idx_geo = indice) |>          # renombra las columnas para distinguir las versiones
  # left_join() pega las dos tablas lado a lado, casando las filas por la columna "uf" (by = "uf")
  left_join(ari |> rename(rank_ari = rank, idx_ari = indice), by = "uf") |>
  mutate(mudanca = rank_geo - rank_ari)  # mutate() agrega una columna; positivo = sube al pasar a aritmética

# ordena por el cambio absoluto (mayor primero) y, con [, c(...)], selecciona las columnas a mostrar
arrange(comparar, desc(abs(mudanca)))[, c("uf", "rank_geo", "rank_ari", "mudanca")]

# PCA sobre los 3 subíndices de las dimensiones. Ya están normalizados en [0,1] (Etapa 5), por
# eso NO los reestandarizamos: scale. = FALSE preserva la dispersión que el min-max dejó — la PCA
# corre sobre la covarianza de los subíndices como el índice de hecho los usa. (scale. = TRUE impondría
# varianza 1 a todos, deshaciendo la normalización ya hecha.)
pca_dim <- prcomp(ufs[, c("i.longev", "i.educ", "i.renda")], scale. = FALSE)

# usa las cargas (en módulo) del 1er componente como pesos, normalizados para sumar 1
pesos_pca <- abs(pca_dim$rotation[, 1])
pesos_pca <- pesos_pca / sum(pesos_pca)
round(pesos_pca, 3)

# compara el ranking con pesos data-driven al del IDHM (pesos iguales), vía construir_indice()
iguais <- construir_indice(ufs)
datad  <- construir_indice(ufs, pesos = pesos_pca)
cor(iguais$indice, datad$indice, method = "spearman")   # ≈ 1 = ordenamiento casi idéntico

# índice de referencia (el IDHM estándar), contra el cual compararemos cada escenario
idx_padrao <- construir_indice(ufs)$indice

# list() guarda escenarios de naturaleza mixta; cada ítem es, él mismo, una lista con
# [regla de agregación, vector de pesos]. Los nombres a la izquierda del '=' rotulan cada escenario
cenarios <- list(
  "renda dobrada"            = list("geometrica", c(1, 1, 2)),
  "educação dobrada"         = list("geometrica", c(1, 2, 1)),
  "índice social (s/ renda)" = list("geometrica", c(1, 1, 0)),
  "aritmética (iguais)"      = list("aritmetica",  c(1, 1, 1))
)

# sapply() aplica la misma función a cada escenario de la lista y junta los resultados en un vector.
# Dentro de la función, cen[[1]] es la regla y cen[[2]] el vector de pesos de aquel escenario
# (los corchetes dobles [[ ]] extraen UN elemento de una lista)
spearman <- sapply(cenarios, function(cen) {
  idx <- construir_indice(ufs, agregacao = cen[[1]], pesos = cen[[2]])$indice
  # correlación de Spearman entre el ranking estándar y el del escenario (1 = ordenamiento idéntico)
  round(cor(idx_padrao, idx, method = "spearman"), 3)
})
sort(spearman, decreasing = TRUE)   # ordena del más estable (≈1) al menos estable

# fija la semilla del generador aleatorio: garantiza que los "sorteos" salgan iguales en cada ejecución
# (reproducibilidad). Cualquier número sirve; usamos 2026
set.seed(2026)

# replicate(1000, {...}) repite el bloque entre llaves 1.000 veces y apila los resultados
# en columnas — cada columna es el ranking de las 27 UF en una simulación
sim <- replicate(1000, {
  w   <- runif(3, 0.5, 2)                            # 3 pesos sorteados al azar entre 0,5 y 2
  agr <- sample(c("geometrica", "aritmetica"), 1)    # sortea 1 de las 2 reglas de agregación
  construir_indice(ufs, agregacao = agr, pesos = w)$rank
})
rownames(sim) <- ufs$NOME   # nombra las filas de la matriz con las UF

# prepara los datos para el gráfico: transforma la matriz (UF × 1.000 simulaciones) en formato
# "largo" (una fila por par UF–simulación), que es lo que ggplot espera
mc <- as.data.frame(sim) |>
  mutate(uf = ufs$NOME) |>
  # pivot_longer apila todas las columnas de simulación en una sola columna "rank"; el -uf significa
  # "todas las columnas, excepto uf"
  pivot_longer(-uf, values_to = "rank") |>
  # reordena las UF por la posición MEDIANA, para que el gráfico salga ordenado del mejor al peor
  mutate(uf = reorder(uf, rank, median))

# ggplot construye el gráfico en capas, sumadas con '+'. aes() mapea variables a ejes;
# geom_boxplot() dibuja, para cada UF, la caja que resume la distribución de las 1.000 posiciones
ggplot(mc, aes(rank, uf)) +
  geom_boxplot(outlier.size = 0.4, fill = "#d6e4f0", color = "#2980b9") +
  labs(x = "Posición en el ranking (1 = mejor)", y = NULL) +   # rótulos de los ejes
  theme_minimal()                                              # tema visual limpio

# rehace la normalización del ingreso SIN el logaritmo (min-max lineal), para aislar el efecto del log
renda_sem_log <- (ufs$RDPC - 8) / (4033 - 8)
# recompone el índice con ese ingreso alternativo, manteniendo longevidad y educación
i_sem_log <- (ufs$i.longev * ufs$i.educ * renda_sem_log)^(1/3)

# compara las posiciones con y sin log lado a lado
data.frame(uf = ufs$NOME,
           rank_com_log = rank(-ufs$idhm, ties.method = "min"),
           rank_sem_log = rank(-i_sem_log, ties.method = "min")) |>
  mutate(mudanca = rank_com_log - rank_sem_log) |>   # cuántas posiciones se desplaza cada UF
  arrange(desc(mudanca)) |> head(10)                 # las 10 que más suben al retirar el log

ufs |>
  select(idhm, i.longev, i.educ, i.renda) |>          # mantiene solo el índice y los tres subíndices
  # apila las tres columnas de dimensión en dos: "dimensao" (el nombre) y "valor" (el número)
  pivot_longer(c(i.longev, i.educ, i.renda), names_to = "dimensao", values_to = "valor") |>
  # recode() cambia los códigos internos por rótulos legibles para la leyenda del gráfico
  mutate(dimensao = recode(dimensao,
                           i.longev = "Longevidad", i.educ = "Educación", i.renda = "Ingreso")) |>
  ggplot(aes(valor, idhm)) +                           # eje x = valor de la dimensión, y = IDHM
  geom_point(color = "#2980b9") +                      # un punto por UF
  geom_smooth(method = "lm", se = FALSE, color = "#c0392b") +  # recta de regresión (lm), sin franja de error
  facet_wrap(~dimensao) +                              # un panel separado para cada dimensión
  labs(x = "Valor de la dimensión", y = "IDHM") +
  theme_minimal()

# selecciona las dos UF y apila sus tres subíndices para el gráfico
perfil <- ufs |>
  filter(NOME %in% c("Rio Grande do Norte", "Roraima")) |>
  select(NOME, i.longev, i.educ, i.renda) |>
  pivot_longer(-NOME, names_to = "dimensao", values_to = "valor") |>
  mutate(dimensao = recode(dimensao,
                           i.longev = "Longevidad", i.educ = "Educación", i.renda = "Ingreso"))

# barras agrupadas (position = "dodge") comparan, dimensión a dimensión, las dos UF
ggplot(perfil, aes(dimensao, valor, fill = NOME)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Subíndice (0–1)", fill = NULL) +
  theme_minimal()

# vuelve a la base completa 'radar' para comparar dos años
delta <- radar |>
  # %in% prueba si ANO es uno de los valores del vector c(2012, 2024) — guarda solo esos dos años
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  select(NOME, ANO, IDHM_L, IDHM_E, IDHM_R) |>
  # pivot_wider hace lo opuesto de pivot_longer: "ensancha" la tabla, creando columnas separadas por
  # año (IDHM_L_2012, IDHM_L_2024, ...), lo que permite restar un año del otro en la misma fila
  pivot_wider(names_from = ANO, values_from = c(IDHM_L, IDHM_E, IDHM_R)) |>
  transmute(uf      = NOME,
            d_long  = IDHM_L_2024 - IDHM_L_2012,   # variación 2012→2024 de cada dimensión
            d_educ  = IDHM_E_2024 - IDHM_E_2012,
            d_renda = IDHM_R_2024 - IDHM_R_2012)

# colMeans() saca el promedio de cada columna — aquí, la variación promedio entre las 27 UF por dimensión
round(colMeans(delta[, c("d_long", "d_educ", "d_renda")]), 4)

# redondeo comercial (la mitad hacia arriba), en vez del estándar de R (la mitad hacia el par)
arred <- function(x) floor(x * 1000 + 0.5) / 1000

# replica la convención del Atlas: cada subíndice redondeado a 3 decimales antes de combinar;
# en la educación, los subcomponentes (stock y flujo) también se redondean
L <- arred(ufs$i.longev)
R <- arred(ufs$i.renda)
E <- arred((arred(ufs$si.escol)^1 * arred(ufs$si.fluxo)^2)^(1/3))
idhm_repro <- arred((L * E * R)^(1/3))

# dispersión del oficial (eje x) contra el reproducido (eje y)
ggplot(ufs, aes(IDHM, idhm_repro)) +
  geom_point(color = "#2980b9", size = 2) +
  # recta de 45° (y = x): si la reproducción coincide con el oficial, los puntos caen sobre ella
  geom_abline(slope = 1, intercept = 0, color = "#c0392b", linetype = "dashed") +
  labs(x = "IDHM oficial (Atlas/Radar)", y = "IDHM reproducido") +
  theme_minimal()

# concordancia cuantificada: correlación, mayor diferencia y cuántas UF coinciden exacto (de 27)
c(correlacao        = cor(ufs$IDHM, idhm_repro),
  maior_dif         = max(abs(ufs$IDHM - idhm_repro)),
  iguais_ao_oficial = sum(idhm_repro == ufs$IDHM)) |> round(4)

# cut() convierte un número continuo en categorías, cortándolo en los puntos de corte informados.
# -Inf e Inf son "menos/más infinito"; right = FALSE hace que el intervalo cierre a la izquierda
faixa <- function(x) cut(x, c(-Inf, .5, .6, .7, .8, Inf), right = FALSE,
                         labels = c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto"))

# table() cruza las franjas del oficial con las del reproducido: la diagonal muestra las concordancias
table(oficial = faixa(ufs$IDHM), reproduzido = faixa(idhm_repro))

# indicadores EXTERNOS, relacionados pero NO usados en el índice, venidos de la propia base:
# proporción de pobres (PMPOB), mortalidad infantil (MORT1) e índice de Gini (GINI).
# El vector de abajo guarda, para cada apodo, el nombre real de la columna en la base
externos <- c(pobreza = "PMPOB", mort_infantil = "MORT1", gini = "GINI")

# sapply() recorre ese vector y, para cada columna, correlaciona nuestro índice con ella.
# ufs[[col]] usa el nombre guardado en 'col' para tomar la columna correspondiente
sapply(externos, function(col) round(cor(ufs$idhm, ufs[[col]], method = "spearman"), 3))

# monta una tabla con el IDHM, el IDHMAD (ajustado a la desigualdad), la pérdida (%) y la posición
# de cada UF en los dos rankings; 'desloca' = cuántas posiciones cambia la UF al pasar de uno al otro
ad <- ufs |>
  transmute(uf = NOME, idhm = IDHM, idhmad = IDHMAD, perda = IDHMAD_PERDA,
            rank_idhm   = rank(-IDHM,   ties.method = "min"),
            rank_idhmad = rank(-IDHMAD, ties.method = "min"),
            desloca     = rank_idhm - rank_idhmad)   # positivo = sube al ajustar por la desigualdad

# dos resúmenes: la correlación entre los dos rankings y la pérdida promedio (mean = promedio) entre las UF
c(spearman   = round(cor(ad$idhm, ad$idhmad, method = "spearman"), 3),
  perda_media = round(mean(ad$perda), 1))            # pérdida promedio (%) por desigualdad

# las 8 UF que más se desplazan (en valor absoluto) al ajustar el índice por la desigualdad
arrange(ad, desc(abs(desloca))) |> head(8)

# st_read() (paquete sf) lee el archivo geográfico con los contornos de las UF; quiet = TRUE silencia mensajes
uf_geo <- st_read("data/uf_brasil.geojson", quiet = TRUE)

# proyecta a SIRGAS 2000 / Polyconic (EPSG:5880), el sistema oficial para mapas de Brasil en metros:
# vuelve la barra de escala correcta (en lat/long la escala variaría con la latitud)
uf_geo <- st_transform(uf_geo, 5880)

# as.integer() convierte el código del IBGE en número entero, para casar con la base por valor numérico
# (más seguro que casar por nombre, que varía en acento y grafía)
uf_geo$CODIGO <- as.integer(uf_geo$codigo_ibg)
# left_join() pega los datos del IDHM al mapa, casando por la columna CODIGO
mapa <- left_join(uf_geo, mutate(ufs, CODIGO = as.integer(CODIGO)), by = "CODIGO")

ggplot(mapa) +
  # geom_sf() dibuja los polígonos de las UF; fill = idhm pinta cada una conforme su índice
  geom_sf(aes(fill = idhm), color = "white", linewidth = 0.2) +
  # escala de color continua viridis (legible p/ daltónicos); direction = -1 invierte (oscuro = mayor)
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "IDHM") +
  # elementos cartográficos obligatorios, además del título y la leyenda (arriba):
  annotation_scale(location = "bl", style = "ticks") +                         # escala
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),       # orientación (norte)
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  labs(title = "IDHM — Unidades de la Federación, 2024",
       caption = "Fuente: Atlas del Desarrollo Humano / Radar IDHM (PNUD, FJP, IBGE).") +  # fuente
  theme_void()   # tema sin ejes ni grillas, adecuado a mapas

# paleta oficial del Atlas/PNUD: rojo → naranja → amarillo → verde → azul
cores_idhm <- c("Muy Bajo" = "#C0392B", "Bajo" = "#E67E22", "Medio" = "#F1C40F",
                "Alto"     = "#1E8449", "Muy Alto" = "#5DADE2")

# IDHM oficial de las UF en 2012 y 2024, discretizado en las 5 franjas con cut()
faixas_anos <- radar |>
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  transmute(CODIGO = as.integer(CODIGO), ANO,
            faixa = cut(IDHM, c(-Inf, .5, .6, .7, .8, Inf), right = FALSE,
                        labels = names(cores_idhm)))

# junta al mapa: cada UF aparece dos veces (una por año), para los dos mapas lado a lado
mapa_anos <- left_join(uf_geo, faixas_anos, by = "CODIGO")

ggplot(mapa_anos) +
  geom_sf(aes(fill = faixa), color = "white", linewidth = 0.2) +
  facet_wrap(~ANO) +                                          # un mapa por año, lado a lado
  # scale_fill_manual fija el color de cada franja; drop = FALSE mantiene las 5 en la leyenda,
  # incluso las ausentes (Muy Bajo, Bajo), para que la escala quede comparable entre años
  scale_fill_manual(values = cores_idhm, drop = FALSE, name = "Franja IDHM") +
  labs(caption = "Fuente: Atlas del Desarrollo Humano / Radar IDHM (PNUD, FJP, IBGE).") +
  theme_void()

# panel con los dos años; ANO como factor se vuelve eje categórico (dos puntos en el x)
evol <- radar |>
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  mutate(ANO = factor(ANO))

# group = NOME liga los dos años de cada UF por una línea — la "inclinación" de cada trayectoria
ggplot(evol, aes(ANO, IDHM, group = NOME)) +
  geom_line(color = "#2980b9", alpha = 0.6) +
  geom_point(color = "#2980b9", size = 1.6) +
  labs(x = NULL, y = "IDHM") +
  theme_minimal()

# # cambie la agregación y/o los pesos de abajo para definir SU escenario
# meu <- construir_indice(ufs, agregacao = "geometrica", pesos = c(1, 1, 1))
# 
# # compara el ranking oficial con el de su escenario y muestra quién más se desplaza
# ufs |>
#   transmute(uf           = NOME,
#             rank_oficial = rank(-IDHM, ties.method = "min"),
#             rank_meu     = meu$rank,
#             mudanca      = rank_oficial - rank_meu) |>
#   arrange(desc(abs(mudanca)))
# 
# # mapa del índice modificado: pega su índice (meu$indice) al mapa y pinta las UF por él
# # (uf_geo ya viene proyectado de la Etapa 10, así que la escala queda correcta)
# mapa_meu <- left_join(uf_geo, mutate(ufs, CODIGO = as.integer(CODIGO), idhm_meu = meu$indice), by = "CODIGO")
# ggplot(mapa_meu) +
#   geom_sf(aes(fill = idhm_meu), color = "white", linewidth = 0.2) +
#   scale_fill_viridis_c(option = "plasma", direction = -1, name = "Mi índice") +
#   annotation_scale(location = "bl", style = "ticks") +                         # escala
#   annotation_north_arrow(location = "tr", style = north_arrow_minimal(),       # norte
#                          height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
#   labs(caption = "Fuente: Atlas del Desarrollo Humano / Radar IDHM (PNUD, FJP, IBGE).") +  # fuente
#   theme_void()
