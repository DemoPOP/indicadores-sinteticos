# =====================================================================
# Building composite indicators - the MHDI in the OECD's 10 steps
# R script (executable version of the tutorial)
#
# Caio Cesar Soares Goncalves - Department of Demography / CEDEPLAR-UFMG
# Workshop DEMOPOP 2026 - License CC BY 4.0
# Site: https://demopop.github.io/indicadores-sinteticos/en/
#
# Requires: readxl, dplyr, tidyr, ggplot2, sf, ggspatial (installed
# automatically by the first block). Keep the data/ folder in your working
# directory and run from top to bottom. The comments explain each step.
# ---------------------------------------------------------------------
# BEFORE RUNNING: point the working directory to the folder that contains
# the data/ subfolder (with adh_radar_base_2012_2024.xlsx and uf_brasil.geojson).
#   In RStudio: menu Session > Set Working Directory > To Source File Location
#   (with this .R saved next to the data/ folder).
# Or uncomment the line below and adjust the path:
# setwd("C:/path/to/indicadores-sinteticos")
# =====================================================================

# c() creates a vector with the names of the packages we will use:
# readxl (read Excel), dplyr/tidyr (manipulate tables), ggplot2 (graphics), sf (maps),
# ggspatial (scale bar and north arrow on maps, Step 10)
pacotes <- c("readxl", "dplyr", "tidyr", "ggplot2", "sf", "ggspatial")

# setdiff() returns the packages from the list that are NOT yet installed...
faltando <- setdiff(pacotes, rownames(installed.packages()))
# ...and, if any are missing, installs them automatically
if (length(faltando) > 0) install.packages(faltando)

# loads (library) each package from the list; invisible() merely suppresses console output
invisible(lapply(pacotes, library, character.only = TRUE))

# read_excel() reads the spreadsheet; sheet = "Total" chooses the tab. The result is stored
# (with the arrow <-, R's assignment operator) in the object 'radar': a table (data frame)
# with the 2012–2024 panel and several geographic breakdowns
radar <- read_excel("data/adh_radar_base_2012_2024.xlsx", sheet = "Total")

# filter() keeps only the rows that satisfy the conditions (year = 2024 AND breakdown = state);
# '==' tests equality (note the double equals sign). The 27 Federative Units of 2024 remain
ufs <- filter(radar, ANO == 2024, AGREGACAO == "Unidade da Federação")

# vector with the codes of the 4 school-FLOW columns, which we will reuse later
cols_fluxo <- c("T_FREQ5A6", "T_FUND11A13", "T_FUND15A17", "T_MED18A20")

# glimpse() shows a summary of the chosen columns. Inside ufs[, c(...)], the comma
# separates [rows, columns]: leaving the rows side empty, we ask for ALL rows and only
# the listed columns
glimpse(ufs[, c("NOME", "ESPVIDA", "T_FUND18M", cols_fluxo, "RDPC", "IDHM")])

# dim() returns the table's dimensions: number of rows (states) and columns (variables)
dim(ufs)

# what matters here are the indicators that FEED the index — not the dozens of columns
# in the dataset. We gather the 8 in a vector: longevity, schooling, the 4 flow ones, income and the
# official MHDI (this last one for the reproduction in Step 9)
cols_calc <- c("ESPVIDA", "T_FUND18M", cols_fluxo, "RDPC", "IDHM")

# is.na() marks each empty cell as TRUE; colSums() sums those TRUE column by column
# (in R, TRUE counts as 1 and FALSE as 0), giving the number of absences per variable.
# ufs[, cols_calc] restricts the check to these 8 columns
na_por_coluna <- colSums(is.na(ufs[, cols_calc]))

# filters the result to show only the columns (among those used in the calculation) with some absence
na_por_coluna[na_por_coluna > 0]

# each rule is a logical vector (TRUE where the state satisfies the condition, FALSE where it fails).
# the & combines conditions (logical AND); >= and <= test the limits
regras <- list(
  renda_positiva    = ufs$RDPC > 0,                              # income > 0: requirement of the log (Step 5)
  espvida_plausivel = ufs$ESPVIDA >= 50 & ufs$ESPVIDA <= 90,     # life expectancy in a plausible range
  escol_percentual  = ufs$T_FUND18M >= 0 & ufs$T_FUND18M <= 100, # schooling (%) within [0,100]
  # apply(..., 1, all) checks, row by row (margin 1 = states), whether ALL 4 flow
  # indicators fall in [0,100]; all() requires the condition to hold for the four at once
  fluxo_percentual  = apply(ufs[, cols_fluxo] >= 0 & ufs[, cols_fluxo] <= 100, 1, all)
)

# sapply runs through the rules and counts, in each one, how many states FAIL: !ok inverts the logical
# (TRUE becomes FALSE and vice versa) and sum() adds up the TRUE. Zero in all = consistent dataset
sapply(regras, function(ok) sum(!ok))

# function that returns TRUE for values outside the boxplot fence
eh_outlier <- function(x) {
  q <- quantile(x, c(.25, .75))     # 1st and 3rd quartiles (Q1 and Q3)
  iqr <- q[2] - q[1]                # interquartile range
  x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr
}

# applies the rule to each calculation indicator and counts how many states fall outside the fence
indic <- data.frame(longevidade  = ufs$ESPVIDA,
                    escolaridade = ufs$T_FUND18M,
                    fluxo        = rowMeans(ufs[, cols_fluxo]),
                    renda        = ufs$RDPC)
sapply(indic, function(x) sum(eh_outlier(x)))

# which state is the single atypical case (in longevity)?
ufs$NOME[eh_outlier(ufs$ESPVIDA)]

# Multivariate analysis examines the indicators SEPARATELY — including the 4 flow ones, which will
# only be combined in Step 6. Taking the flow mean already here would hide the redundancy among them.
# The operator |> ("pipe") passes ufs as the 1st argument of transmute(), which builds a table in which
# each column is a raw indicator (to the left of '=', the name; to the right, the dataset column):
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

# cor() computes the correlation matrix across the columns; round(..., 2) rounds to 2 places.
# use = "complete.obs" tells it to ignore any absences in the computation
round(cor(indicadores, use = "complete.obs"), 2)

# Cronbach's alpha: k/(k-1) * (1 - sum of item variances / variance of the item sum)
alpha <- function(itens) {
  k <- ncol(itens)
  (k / (k - 1)) * (1 - sum(apply(itens, 2, var)) / var(rowSums(itens)))
}

# education dimension (stock + 4 flow, all in %), with and without the saturated 5–6 flow
c(com_5a6 = alpha(ufs[, c("T_FUND18M", cols_fluxo)]),
  sem_5a6 = alpha(ufs[, c("T_FUND18M", "T_FUND11A13", "T_FUND15A17", "T_MED18A20")])) |> round(3)

# prcomp() does the PCA; scale.=TRUE standardizes the indicators first (otherwise income, of larger scale,
# would dominate). It reuses the object 'indicadores' (the 7 columns of the correlation matrix)
pca <- prcomp(indicadores, scale. = TRUE)

round(summary(pca)$importance[2, 1:3], 3)   # proportion of variance summarized by PC1, PC2, PC3
round(pca$rotation[, 1], 2)                  # loadings (weights) of the indicators on the 1st component

# scale() standardizes; dist() computes the distances among the states; hclust() groups them
# hierarchically. In the dendrogram, the lower the junction, the more alike the states are
d  <- dist(scale(indicadores))
hc <- hclust(d, method = "ward.D2")
plot(hc, labels = ufs$NOME, main = NULL, xlab = "", ylab = "Distance", sub = "", cex = 0.75)

# the dollar sign ($) accesses a table column by name: ufs$ESPVIDA is the vector of life expectancies
ev <- ufs$ESPVIDA

# the three computations below operate on the whole vector at once (R is "vectorized":
# no loop is needed to apply the formula to each state)
fixo     <- (ev - 25) / (85 - 25)                 # min-max with fixed reference (MHDI goalposts)
amostral <- (ev - min(ev)) / (max(ev) - min(ev))  # min-max with the min/max observed in the sample
zscore   <- (ev - mean(ev)) / sd(ev)              # z-score standardization (mean = mean, sd = standard deviation)

# data.frame() joins the three vectors into columns; summary() summarizes each (min, mean, max...)
summary(data.frame(fixo, amostral, zscore))

# creates a NEW column in ufs (ufs$i.longev) with the normalized longevity.
# min-max with fixed reference: lower goalpost 25, upper 85 years
ufs$i.longev <- (ufs$ESPVIDA - 25) / (85 - 25)

# same idea as longevity, but on a logarithmic scale: log() is the natural logarithm (ln).
# references (already in log) R$ 8 and R$ 4,033, in constant reais
ufs$i.renda <- (log(ufs$RDPC) - log(8)) / (log(4033) - log(8))

# each subindicator is already in percentage (0 to 100), so dividing by 100 puts it in [0,1]
ufs$si.escol <- ufs$T_FUND18M / 100                  # stock: schooling of the adult population
ufs$si.fluxo <- rowMeans(ufs[, cols_fluxo]) / 100    # flow: mean of the 4 attendance columns

# summary() of the four already-normalized subindices, to check that they all ended up in [0,1]
summary(ufs[, c("i.longev", "i.renda", "si.escol", "si.fluxo")])

# the caret (^) is the power. Weighted geometric mean = product of the terms
# raised to the weights, with the final exponent 1/(sum of weights): here weights 1 and 2, hence cube root
ufs$i.educ <- (ufs$si.escol^1 * ufs$si.fluxo^2)^(1/3)

# for comparison, the same geometric mean but with equal weights (1 and 1 → square root)
educ_iguais <- (ufs$si.escol^1 * ufs$si.fluxo^1)^(1/2)

# builds a table with the two versions and the difference between them...
data.frame(uf = ufs$NOME,
           educ_idhm = round(ufs$i.educ, 3),
           educ_iguais = round(educ_iguais, 3),
           dif = round(ufs$i.educ - educ_iguais, 3)) |>
  # ...orders by the absolute difference (abs), from largest to smallest (desc), and shows the top 8.
  # head(8) returns the first 8 rows
  arrange(desc(abs(dif))) |> head(8)

# We define OUR own function. function(...) lists the input arguments; those with
# '=' already carry a default value, used when we do not inform them in the call. Thus, varying
# normalization, aggregation and weights becomes just swapping arguments.
construir_indice <- function(dados,
                             agregacao = "geometrica",  # "geometrica" or "aritmetica"
                             pesos = c(1, 1, 1)) {       # weights of longevity, education, income
  # short aliases for the three subindices (L, E, R), only so the formula reads cleanly
  L <- dados$i.longev; E <- dados$i.educ; R <- dados$i.renda

  # normalizes the weights so they sum to 1 (w[1] is the 1st element of vector w, w[2] the 2nd, etc.)
  w <- pesos / sum(pesos)

  # if/else chooses the rule: product of powers (geometric) or weighted sum (arithmetic)
  indice <- if (agregacao == "geometrica") L^w[1] * E^w[2] * R^w[3]
            else                            w[1]*L + w[2]*E + w[3]*R

  # the function returns a table with state, index and rank position.
  # rank(-indice) orders in descending fashion (the '-' inverts: higher index = 1st place);
  # ties.method = "min" gives the same position (the lowest) to ties
  data.frame(uf = dados$NOME,
             indice = round(indice, 4),
             rank = rank(-indice, ties.method = "min"))
}

# call with the defaults (geometric, equal weights) = the official MHDI; we keep only the 'indice' column
ufs$idhm <- construir_indice(ufs)$indice

# computes the index under the two aggregation rules, changing only the 'agregacao' argument
geo <- construir_indice(ufs, agregacao = "geometrica")
ari <- construir_indice(ufs, agregacao = "aritmetica")

comparar <- geo |>
  rename(rank_geo = rank, idx_geo = indice) |>          # renames the columns to distinguish the versions
  # left_join() pastes the two tables side by side, matching rows by the "uf" column (by = "uf")
  left_join(ari |> rename(rank_ari = rank, idx_ari = indice), by = "uf") |>
  mutate(mudanca = rank_geo - rank_ari)  # mutate() adds a column; positive = rises when switching to arithmetic

# orders by the absolute change (largest first) and, with [, c(...)], selects the columns to display
arrange(comparar, desc(abs(mudanca)))[, c("uf", "rank_geo", "rank_ari", "mudanca")]

# PCA over the 3 dimension subindices. They are ALREADY normalized in [0,1] (Step 5), so
# we do NOT re-standardize them: scale. = FALSE preserves the dispersion the min-max left — the PCA
# runs over the covariance of the subindices as the index actually uses them. (scale. = TRUE would impose
# variance 1 on all, undoing the normalization already done.)
pca_dim <- prcomp(ufs[, c("i.longev", "i.educ", "i.renda")], scale. = FALSE)

# uses the loadings (in absolute value) of the 1st component as weights, normalized to sum to 1
pesos_pca <- abs(pca_dim$rotation[, 1])
pesos_pca <- pesos_pca / sum(pesos_pca)
round(pesos_pca, 3)

# compares the ranking with data-driven weights to that of the MHDI (equal weights), via construir_indice()
iguais <- construir_indice(ufs)
datad  <- construir_indice(ufs, pesos = pesos_pca)
cor(iguais$indice, datad$indice, method = "spearman")   # ≈ 1 = nearly identical ordering

# reference index (the standard MHDI), against which we will compare each scenario
idx_padrao <- construir_indice(ufs)$indice

# list() holds scenarios of mixed nature; each item is itself a list with
# [aggregation rule, weights vector]. The names to the left of '=' label each scenario
cenarios <- list(
  "renda dobrada"            = list("geometrica", c(1, 1, 2)),
  "educação dobrada"         = list("geometrica", c(1, 2, 1)),
  "índice social (s/ renda)" = list("geometrica", c(1, 1, 0)),
  "aritmética (iguais)"      = list("aritmetica",  c(1, 1, 1))
)

# sapply() applies the same function to each scenario in the list and joins the results into a vector.
# Inside the function, cen[[1]] is the rule and cen[[2]] the weights vector of that scenario
# (double brackets [[ ]] extract ONE element from a list)
spearman <- sapply(cenarios, function(cen) {
  idx <- construir_indice(ufs, agregacao = cen[[1]], pesos = cen[[2]])$indice
  # Spearman correlation between the standard ranking and that of the scenario (1 = identical ordering)
  round(cor(idx_padrao, idx, method = "spearman"), 3)
})
sort(spearman, decreasing = TRUE)   # orders from the most stable (≈1) to the least stable

# fixes the random generator's seed: ensures the "draws" come out the same on every run
# (reproducibility). Any number works; we use 2026
set.seed(2026)

# replicate(1000, {...}) repeats the block between braces 1,000 times and stacks the results
# in columns — each column is the ranking of the 27 states in one simulation
sim <- replicate(1000, {
  w   <- runif(3, 0.5, 2)                            # 3 weights drawn at random between 0.5 and 2
  agr <- sample(c("geometrica", "aritmetica"), 1)    # draws 1 of the 2 aggregation rules
  construir_indice(ufs, agregacao = agr, pesos = w)$rank
})
rownames(sim) <- ufs$NOME   # names the matrix rows with the states

# prepares the data for the plot: turns the matrix (states × 1,000 simulations) into "long"
# format (one row per state–simulation pair), which is what ggplot expects
mc <- as.data.frame(sim) |>
  mutate(uf = ufs$NOME) |>
  # pivot_longer stacks all the simulation columns into a single "rank" column; the -uf means
  # "all columns, except uf"
  pivot_longer(-uf, values_to = "rank") |>
  # reorders the states by the MEDIAN position, so the plot comes out ordered from best to worst
  mutate(uf = reorder(uf, rank, median))

# ggplot builds the plot in layers, added with '+'. aes() maps variables to axes;
# geom_boxplot() draws, for each state, the box that summarizes the distribution of the 1,000 positions
ggplot(mc, aes(rank, uf)) +
  geom_boxplot(outlier.size = 0.4, fill = "#d6e4f0", color = "#2980b9") +
  labs(x = "Ranking position (1 = best)", y = NULL) +      # axis labels
  theme_minimal()                                          # clean visual theme

# redoes the income normalization WITHOUT the logarithm (linear min-max), to isolate the log's effect
renda_sem_log <- (ufs$RDPC - 8) / (4033 - 8)
# recomposes the index with this alternative income, keeping longevity and education
i_sem_log <- (ufs$i.longev * ufs$i.educ * renda_sem_log)^(1/3)

# compares the positions with and without log side by side
data.frame(uf = ufs$NOME,
           rank_com_log = rank(-ufs$idhm, ties.method = "min"),
           rank_sem_log = rank(-i_sem_log, ties.method = "min")) |>
  mutate(mudanca = rank_com_log - rank_sem_log) |>   # how many positions each state shifts
  arrange(desc(mudanca)) |> head(10)                 # the 10 that rise the most when the log is removed

ufs |>
  select(idhm, i.longev, i.educ, i.renda) |>          # keeps only the index and the three subindices
  # stacks the three dimension columns into two: "dimensao" (the name) and "valor" (the number)
  pivot_longer(c(i.longev, i.educ, i.renda), names_to = "dimensao", values_to = "valor") |>
  # recode() swaps the internal codes for readable labels for the chart legend
  mutate(dimensao = recode(dimensao,
                           i.longev = "Longevity", i.educ = "Education", i.renda = "Income")) |>
  ggplot(aes(valor, idhm)) +                           # x-axis = dimension value, y = MHDI
  geom_point(color = "#2980b9") +                      # one point per state
  geom_smooth(method = "lm", se = FALSE, color = "#c0392b") +  # regression line (lm), without error band
  facet_wrap(~dimensao) +                              # a separate panel for each dimension
  labs(x = "Dimension value", y = "MHDI") +
  theme_minimal()

# selects the two states and stacks their three subindices for the chart
perfil <- ufs |>
  filter(NOME %in% c("Rio Grande do Norte", "Roraima")) |>
  select(NOME, i.longev, i.educ, i.renda) |>
  pivot_longer(-NOME, names_to = "dimensao", values_to = "valor") |>
  mutate(dimensao = recode(dimensao,
                           i.longev = "Longevity", i.educ = "Education", i.renda = "Income"))

# grouped bars (position = "dodge") compare, dimension by dimension, the two states
ggplot(perfil, aes(dimensao, valor, fill = NOME)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Subindex (0–1)", fill = NULL) +
  theme_minimal()

# returns to the full base 'radar' to compare two years
delta <- radar |>
  # %in% tests whether ANO is one of the values in the vector c(2012, 2024) — keeps only those two years
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  select(NOME, ANO, IDHM_L, IDHM_E, IDHM_R) |>
  # pivot_wider does the opposite of pivot_longer: it "widens" the table, creating separate columns by
  # year (IDHM_L_2012, IDHM_L_2024, ...), which allows subtracting one year from the other in the same row
  pivot_wider(names_from = ANO, values_from = c(IDHM_L, IDHM_E, IDHM_R)) |>
  transmute(uf      = NOME,
            d_long  = IDHM_L_2024 - IDHM_L_2012,   # 2012→2024 variation of each dimension
            d_educ  = IDHM_E_2024 - IDHM_E_2012,
            d_renda = IDHM_R_2024 - IDHM_R_2012)

# colMeans() takes the mean of each column — here, the mean variation across the 27 states by dimension
round(colMeans(delta[, c("d_long", "d_educ", "d_renda")]), 4)

# commercial rounding (half up), instead of R's default (half to even)
arred <- function(x) floor(x * 1000 + 0.5) / 1000

# replicates the Atlas convention: each subindex rounded to 3 places before combining;
# in education, the subcomponents (stock and flow) are also rounded
L <- arred(ufs$i.longev)
R <- arred(ufs$i.renda)
E <- arred((arred(ufs$si.escol)^1 * arred(ufs$si.fluxo)^2)^(1/3))
idhm_repro <- arred((L * E * R)^(1/3))

# scatter of the official (x-axis) against the reproduced (y-axis)
ggplot(ufs, aes(IDHM, idhm_repro)) +
  geom_point(color = "#2980b9", size = 2) +
  # 45° line (y = x): if the reproduction matches the official, the points fall on it
  geom_abline(slope = 1, intercept = 0, color = "#c0392b", linetype = "dashed") +
  labs(x = "Official MHDI (Atlas/Radar)", y = "Reproduced MHDI") +
  theme_minimal()

# quantified agreement: correlation, largest difference and how many states match exactly (of 27)
c(correlacao        = cor(ufs$IDHM, idhm_repro),
  maior_dif         = max(abs(ufs$IDHM - idhm_repro)),
  iguais_ao_oficial = sum(idhm_repro == ufs$IDHM)) |> round(4)

# cut() converts a continuous number into categories, slicing it at the informed cut points.
# -Inf and Inf are "minus/plus infinity"; right = FALSE makes the interval close on the left
faixa <- function(x) cut(x, c(-Inf, .5, .6, .7, .8, Inf), right = FALSE,
                         labels = c("Very Low", "Low", "Medium", "High", "Very High"))

# table() cross-tabulates the official bands with the reproduced ones: the diagonal shows the agreements
table(oficial = faixa(ufs$IDHM), reproduzido = faixa(idhm_repro))

# EXTERNAL indicators, related but NOT used in the index, from the dataset itself:
# proportion of poor (PMPOB), infant mortality (MORT1) and Gini index (GINI).
# The vector below holds, for each alias, the real name of the column in the dataset
externos <- c(pobreza = "PMPOB", mort_infantil = "MORT1", gini = "GINI")

# sapply() runs through this vector and, for each column, correlates our index with it.
# ufs[[col]] uses the name held in 'col' to grab the corresponding column
sapply(externos, function(col) round(cor(ufs$idhm, ufs[[col]], method = "spearman"), 3))

# builds a table with the MHDI, the MHDI-AD (inequality-adjusted), the loss (%) and the position
# of each state in the two rankings; 'desloca' = how many positions the state changes from one to the other
ad <- ufs |>
  transmute(uf = NOME, idhm = IDHM, idhmad = IDHMAD, perda = IDHMAD_PERDA,
            rank_idhm   = rank(-IDHM,   ties.method = "min"),
            rank_idhmad = rank(-IDHMAD, ties.method = "min"),
            desloca     = rank_idhm - rank_idhmad)   # positive = rises when adjusting for inequality

# two summaries: the correlation between the two rankings and the mean loss (mean = mean) across the states
c(spearman   = round(cor(ad$idhm, ad$idhmad, method = "spearman"), 3),
  perda_media = round(mean(ad$perda), 1))            # mean loss (%) due to inequality

# the 8 states that shift the most (in absolute value) when the index is adjusted for inequality
arrange(ad, desc(abs(desloca))) |> head(8)

# st_read() (package sf) reads the geographic file with the states' outlines; quiet = TRUE silences messages
uf_geo <- st_read("data/uf_brasil.geojson", quiet = TRUE)

# projects to SIRGAS 2000 / Polyconic (EPSG:5880), the official system for maps of Brazil in meters:
# makes the scale bar correct (in lat/long the scale would vary with latitude)
uf_geo <- st_transform(uf_geo, 5880)

# as.integer() converts the IBGE code into an integer, to match the dataset by numeric value
# (safer than matching by name, which varies in accent and spelling)
uf_geo$CODIGO <- as.integer(uf_geo$codigo_ibg)
# left_join() pastes the MHDI data onto the map, matching by the CODIGO column
mapa <- left_join(uf_geo, mutate(ufs, CODIGO = as.integer(CODIGO)), by = "CODIGO")

ggplot(mapa) +
  # geom_sf() draws the states' polygons; fill = idhm paints each one according to its index
  geom_sf(aes(fill = idhm), color = "white", linewidth = 0.2) +
  # continuous viridis color scale (legible for the color-blind); direction = -1 inverts (dark = higher)
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "MHDI") +
  # mandatory cartographic elements, besides the title and the legend (above):
  annotation_scale(location = "bl", style = "ticks") +                         # scale
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),       # orientation (north)
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  labs(title = "MHDI — Federative Units, 2024",
       caption = "Source: Atlas of Human Development / Radar IDHM (UNDP, FJP, IBGE).") +  # source
  theme_void()   # theme without axes or grids, suitable for maps

# official Atlas/UNDP palette: red → orange → yellow → green → blue
cores_idhm <- c("Very Low" = "#C0392B", "Low" = "#E67E22", "Medium" = "#F1C40F",
                "High"     = "#1E8449", "Very High" = "#5DADE2")

# official MHDI of the states in 2012 and 2024, discretized into the 5 bands with cut()
faixas_anos <- radar |>
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  transmute(CODIGO = as.integer(CODIGO), ANO,
            faixa = cut(IDHM, c(-Inf, .5, .6, .7, .8, Inf), right = FALSE,
                        labels = names(cores_idhm)))

# joins to the map: each state appears twice (once per year), for the two maps side by side
mapa_anos <- left_join(uf_geo, faixas_anos, by = "CODIGO")

ggplot(mapa_anos) +
  geom_sf(aes(fill = faixa), color = "white", linewidth = 0.2) +
  facet_wrap(~ANO) +                                          # one map per year, side by side
  # scale_fill_manual fixes the color of each band; drop = FALSE keeps the 5 in the legend,
  # even the absent ones (Very Low, Low), so the scale stays comparable between years
  scale_fill_manual(values = cores_idhm, drop = FALSE, name = "MHDI band") +
  labs(caption = "Source: Atlas of Human Development / Radar IDHM (UNDP, FJP, IBGE).") +
  theme_void()

# panel with the two years; ANO as a factor becomes a categorical axis (two points on the x)
evol <- radar |>
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  mutate(ANO = factor(ANO))

# group = NOME links each state's two years by a line — the "slope" of each trajectory
ggplot(evol, aes(ANO, IDHM, group = NOME)) +
  geom_line(color = "#2980b9", alpha = 0.6) +
  geom_point(color = "#2980b9", size = 1.6) +
  labs(x = NULL, y = "MHDI") +
  theme_minimal()

# # swap the aggregation and/or the weights below to define YOUR scenario
# meu <- construir_indice(ufs, agregacao = "geometrica", pesos = c(1, 1, 1))
# 
# # compares the official ranking with that of your scenario and shows who shifts the most
# ufs |>
#   transmute(uf           = NOME,
#             rank_oficial = rank(-IDHM, ties.method = "min"),
#             rank_meu     = meu$rank,
#             mudanca      = rank_oficial - rank_meu) |>
#   arrange(desc(abs(mudanca)))
# 
# # map of the modified index: pastes your index (meu$indice) onto the map and paints the states by it
# # (uf_geo already comes projected from Step 10, so the scale stays correct)
# mapa_meu <- left_join(uf_geo, mutate(ufs, CODIGO = as.integer(CODIGO), idhm_meu = meu$indice), by = "CODIGO")
# ggplot(mapa_meu) +
#   geom_sf(aes(fill = idhm_meu), color = "white", linewidth = 0.2) +
#   scale_fill_viridis_c(option = "plasma", direction = -1, name = "My index") +
#   annotation_scale(location = "bl", style = "ticks") +                         # scale
#   annotation_north_arrow(location = "tr", style = north_arrow_minimal(),       # north
#                          height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
#   labs(caption = "Source: Atlas of Human Development / Radar IDHM (UNDP, FJP, IBGE).") +  # source
#   theme_void()
