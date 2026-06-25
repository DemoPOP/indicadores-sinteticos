# =====================================================================
# Producao de indicadores sinteticos - o IDHM nas 10 etapas da OCDE
# Script R (versao executavel do tutorial)
#
# Caio Cesar Soares Goncalves - Departamento de Demografia / CEDEPLAR-UFMG
# Workshop DEMOPOP 2026 - Licenca CC BY 4.0
# Site: https://demopop.github.io/indicadores-sinteticos/
#
# Requer: readxl, dplyr, tidyr, ggplot2, sf, ggspatial (instalados
# automaticamente pelo primeiro bloco). Mantenha a pasta data/ no diretorio
# de trabalho e rode de cima para baixo. Os comentarios explicam cada passo.
# ---------------------------------------------------------------------
# ANTES DE RODAR: aponte o diretorio de trabalho para a pasta que contem
# a subpasta data/ (com adh_radar_base_2012_2024.xlsx e uf_brasil.geojson).
#   No RStudio: menu Session > Set Working Directory > To Source File Location
#   (com este .R salvo ao lado da pasta data/).
# Ou descomente a linha abaixo e ajuste o caminho:
# setwd("C:/caminho/para/indicadores-sinteticos")
# =====================================================================

######################################################################
## Etapa 2 · Seleção de dados
######################################################################

# c() cria um vetor com os nomes dos pacotes que vamos usar:
# readxl (ler Excel), dplyr/tidyr (manipular tabelas), ggplot2 (gráficos), sf (mapas),
# ggspatial (escala e rosa-dos-ventos nos mapas, Etapa 10)
pacotes <- c("readxl", "dplyr", "tidyr", "ggplot2", "sf", "ggspatial")

# setdiff() devolve os pacotes da lista que ainda NÃO estão instalados...
faltando <- setdiff(pacotes, rownames(installed.packages()))
# ...e, se houver algum faltando, instala-o automaticamente
if (length(faltando) > 0) install.packages(faltando)

# carrega (library) cada pacote da lista; invisible() apenas suprime a saída no console
invisible(lapply(pacotes, library, character.only = TRUE))

# read_excel() lê a planilha; sheet = "Total" escolhe a aba. O resultado é guardado
# (com a seta <-, o operador de atribuição do R) no objeto 'radar': uma tabela (data frame)
# com o painel 2012–2024 e vários recortes geográficos
radar <- read_excel("data/adh_radar_base_2012_2024.xlsx", sheet = "Total")

# filter() mantém apenas as linhas que satisfazem as condições (ano = 2024 E recorte = UF);
# '==' testa igualdade (note os dois sinais de igual). Sobram as 27 UFs de 2024
ufs <- filter(radar, ANO == 2024, AGREGACAO == "Unidade da Federação")

# vetor com os códigos das 4 colunas de FLUXO escolar, que reutilizaremos adiante
cols_fluxo <- c("T_FREQ5A6", "T_FUND11A13", "T_FUND15A17", "T_MED18A20")

# glimpse() mostra um resumo das colunas escolhidas. Dentro de ufs[, c(...)], a vírgula
# separa [linhas, colunas]: deixando o lado das linhas vazio, pedimos TODAS as linhas e só
# as colunas listadas
glimpse(ufs[, c("NOME", "ESPVIDA", "T_FUND18M", cols_fluxo, "RDPC", "IDHM")])

######################################################################
## Etapa 3 · Tratamento e preparação dos dados
######################################################################

# --- A sequência é um fluxo, não um script ---

# dim() devolve as dimensões da tabela: número de linhas (UFs) e de colunas (variáveis)
dim(ufs)

# o que importa aqui são os indicadores que ALIMENTAM o índice — não as dezenas de colunas
# da base. Reunimos os 8 num vetor: longevidade, escolaridade, os 4 de fluxo, renda e o
# IDHM oficial (este último para a reprodução na Etapa 9)
cols_calc <- c("ESPVIDA", "T_FUND18M", cols_fluxo, "RDPC", "IDHM")

# is.na() marca como TRUE cada célula vazia; colSums() soma esses TRUE coluna a coluna
# (no R, TRUE conta como 1 e FALSE como 0), dando o nº de ausências por variável.
# ufs[, cols_calc] restringe a checagem a essas 8 colunas
na_por_coluna <- colSums(is.na(ufs[, cols_calc]))

# filtra o resultado para exibir apenas as colunas (dentre as do cálculo) com alguma ausência
na_por_coluna[na_por_coluna > 0]

# cada regra é um vetor lógico (TRUE onde a UF satisfaz a condição, FALSE onde falha).
# o & combina condições (E lógico); >= e <= testam os limites
regras <- list(
  renda_positiva    = ufs$RDPC > 0,                              # renda > 0: exigência do log (Etapa 5)
  espvida_plausivel = ufs$ESPVIDA >= 50 & ufs$ESPVIDA <= 90,     # esperança de vida em faixa plausível
  escol_percentual  = ufs$T_FUND18M >= 0 & ufs$T_FUND18M <= 100, # escolaridade (%) dentro de [0,100]
  # apply(..., 1, all) verifica, linha a linha (margem 1 = UFs), se TODOS os 4 indicadores
  # de fluxo caem em [0,100]; all() exige que a condição valha para os quatro de uma vez
  fluxo_percentual  = apply(ufs[, cols_fluxo] >= 0 & ufs[, cols_fluxo] <= 100, 1, all)
)

# sapply percorre as regras e conta, em cada uma, quantas UFs FALHAM: !ok inverte o lógico
# (TRUE vira FALSE e vice-versa) e sum() soma os TRUE. Zero em todas = base consistente
sapply(regras, function(ok) sum(!ok))

# função que devolve TRUE para os valores fora da cerca do boxplot
eh_outlier <- function(x) {
  q <- quantile(x, c(.25, .75))     # 1º e 3º quartis (Q1 e Q3)
  iqr <- q[2] - q[1]                # amplitude interquartílica
  x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr
}

# aplica a regra a cada indicador do cálculo e conta quantas UFs caem fora da cerca
indic <- data.frame(longevidade  = ufs$ESPVIDA,
                    escolaridade = ufs$T_FUND18M,
                    fluxo        = rowMeans(ufs[, cols_fluxo]),
                    renda        = ufs$RDPC)
sapply(indic, function(x) sum(eh_outlier(x)))

# qual UF é o único caso atípico (em longevidade)?
ufs$NOME[eh_outlier(ufs$ESPVIDA)]

######################################################################
## Etapa 4 · Análise multivariada
######################################################################

# A análise multivariada examina os indicadores SEPARADOS — inclusive os 4 de fluxo, que só
# serão combinados na Etapa 6. Tomar a média do fluxo já aqui esconderia a redundância entre eles.
# O operador |> ("pipe") passa ufs como 1º argumento de transmute(), que monta uma tabela em que
# cada coluna é um indicador bruto (à esquerda do '=', o nome; à direita, a coluna da base):
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

# cor() calcula a matriz de correlação entre as colunas; round(..., 2) arredonda para 2 casas.
# use = "complete.obs" manda ignorar eventuais ausências no cálculo
round(cor(indicadores, use = "complete.obs"), 2)

# alpha de Cronbach: k/(k-1) * (1 - soma das variâncias dos itens / variância da soma dos itens)
alpha <- function(itens) {
  k <- ncol(itens)
  (k / (k - 1)) * (1 - sum(apply(itens, 2, var)) / var(rowSums(itens)))
}

# dimensão educação (estoque + 4 fluxo, todos em %), com e sem o fluxo saturado de 5–6 anos
c(com_5a6 = alpha(ufs[, c("T_FUND18M", cols_fluxo)]),
  sem_5a6 = alpha(ufs[, c("T_FUND18M", "T_FUND11A13", "T_FUND15A17", "T_MED18A20")])) |> round(3)

# prcomp() faz a ACP; scale.=TRUE padroniza os indicadores antes (senão a renda, de maior escala,
# dominaria). Reaproveita o objeto 'indicadores' (as 7 colunas da matriz de correlação)
pca <- prcomp(indicadores, scale. = TRUE)

round(summary(pca)$importance[2, 1:3], 3)   # proporção da variância resumida por PC1, PC2, PC3
round(pca$rotation[, 1], 2)                  # cargas (pesos) dos indicadores no 1º componente

# scale() padroniza; dist() calcula as distâncias entre as UFs; hclust() as agrupa de forma
# hierárquica. No dendrograma, quanto mais baixa a junção, mais parecidas são as UFs
d  <- dist(scale(indicadores))
hc <- hclust(d, method = "ward.D2")
plot(hc, labels = ufs$NOME, main = NULL, xlab = "", ylab = "Distância", sub = "", cex = 0.75)

######################################################################
## Etapa 5 · Normalização
######################################################################

# o cifrão ($) acessa uma coluna da tabela pelo nome: ufs$ESPVIDA é o vetor de esperanças de vida
ev <- ufs$ESPVIDA

# as três contas abaixo operam sobre o vetor inteiro de uma vez (R é "vetorizado":
# não é preciso laço/loop para aplicar a fórmula a cada UF)
fixo     <- (ev - 25) / (85 - 25)                 # min-max com referência fixa (balizas do IDHM)
amostral <- (ev - min(ev)) / (max(ev) - min(ev))  # min-max com o mín/máx observados na amostra
zscore   <- (ev - mean(ev)) / sd(ev)              # padronização z-score (mean = média, sd = desvio-padrão)

# data.frame() junta os três vetores em colunas; summary() resume cada uma (mín, média, máx...)
summary(data.frame(fixo, amostral, zscore))

# --- Aplicando as normalizações do IDHM ---

# cria uma NOVA coluna em ufs (ufs$i.longev) com a longevidade normalizada.
# min-max com referência fixa: baliza inferior 25, superior 85 anos
ufs$i.longev <- (ufs$ESPVIDA - 25) / (85 - 25)

# mesma ideia da longevidade, mas em escala logarítmica: log() é o logaritmo natural (ln).
# referências (já em log) R$ 8 e R$ 4.033, em reais constantes
ufs$i.renda <- (log(ufs$RDPC) - log(8)) / (log(4033) - log(8))

# cada subindicador já está em percentual (0 a 100), então dividir por 100 o põe em [0,1]
ufs$si.escol <- ufs$T_FUND18M / 100                  # estoque: escolaridade da população adulta
ufs$si.fluxo <- rowMeans(ufs[, cols_fluxo]) / 100    # fluxo: média das 4 colunas de frequência

# summary() dos quatro subíndices já normalizados, para conferir que ficaram todos em [0,1]
summary(ufs[, c("i.longev", "i.renda", "si.escol", "si.fluxo")])

######################################################################
## Etapa 6 · Ponderação e agregação
######################################################################

# --- Ponderação e agregação na dimensão educação ---

# o acento circunflexo (^) é a potência. Média geométrica ponderada = produto dos termos
# elevados aos pesos, com o expoente final 1/(soma dos pesos): aqui pesos 1 e 2, logo raiz cúbica
ufs$i.educ <- (ufs$si.escol^1 * ufs$si.fluxo^2)^(1/3)

# para comparar, a mesma média geométrica mas com pesos iguais (1 e 1 → raiz quadrada)
educ_iguais <- (ufs$si.escol^1 * ufs$si.fluxo^1)^(1/2)

# monta uma tabela com as duas versões e a diferença entre elas...
data.frame(uf = ufs$NOME,
           educ_idhm = round(ufs$i.educ, 3),
           educ_iguais = round(educ_iguais, 3),
           dif = round(ufs$i.educ - educ_iguais, 3)) |>
  # ...ordena pela diferença em valor absoluto (abs), do maior p/ o menor (desc), e mostra o topo 8.
  # head(8) devolve as 8 primeiras linhas
  arrange(desc(abs(dif))) |> head(8)

# --- Agregação das três dimensões ---

# Definimos NOSSA própria função. function(...) lista os argumentos de entrada; os que têm
# '=' já trazem um valor padrão, usado quando não os informamos na chamada. Assim, variar
# normalização, agregação e pesos vira só trocar argumentos.
construir_indice <- function(dados,
                             agregacao = "geometrica",  # "geometrica" ou "aritmetica"
                             pesos = c(1, 1, 1)) {       # pesos de longevidade, educação, renda
  # apelidos curtos para os três subíndices (L, E, R), só para a fórmula ficar legível
  L <- dados$i.longev; E <- dados$i.educ; R <- dados$i.renda

  # normaliza os pesos para que somem 1 (w[1] é o 1º elemento do vetor w, w[2] o 2º, etc.)
  w <- pesos / sum(pesos)

  # if/else escolhe a regra: produto de potências (geométrica) ou soma ponderada (aritmética)
  indice <- if (agregacao == "geometrica") L^w[1] * E^w[2] * R^w[3]
            else                            w[1]*L + w[2]*E + w[3]*R

  # a função devolve uma tabela com UF, índice e posição no ranking.
  # rank(-indice) ordena de forma decrescente (o '-' inverte: maior índice = 1º lugar);
  # ties.method = "min" dá a mesma posição (a menor) a empates
  data.frame(uf = dados$NOME,
             indice = round(indice, 4),
             rank = rank(-indice, ties.method = "min"))
}

# chamada com os padrões (geométrica, pesos iguais) = o IDHM oficial; guardamos só a coluna 'indice'
ufs$idhm <- construir_indice(ufs)$indice
# -- Geométrica versus aritmética

# calcula o índice nas duas regras de agregação, mudando só o argumento 'agregacao'
geo <- construir_indice(ufs, agregacao = "geometrica")
ari <- construir_indice(ufs, agregacao = "aritmetica")

comparar <- geo |>
  rename(rank_geo = rank, idx_geo = indice) |>          # renomeia as colunas para distinguir as versões
  # left_join() cola as duas tabelas lado a lado, casando as linhas pela coluna "uf" (by = "uf")
  left_join(ari |> rename(rank_ari = rank, idx_ari = indice), by = "uf") |>
  mutate(mudanca = rank_geo - rank_ari)  # mutate() acrescenta uma coluna; positivo = sobe ao virar aritmética

# ordena pela mudança absoluta (maior primeiro) e, com [, c(...)], seleciona as colunas a exibir
arrange(comparar, desc(abs(mudanca)))[, c("uf", "rank_geo", "rank_ari", "mudanca")]

# --- Pesos derivados dos dados (ponderação estatística) ---

# PCA sobre os 3 subíndices das dimensões. Eles JÁ estão normalizados em [0,1] (Etapa 5), por
# isso NÃO os repadronizamos: scale. = FALSE preserva a dispersão que o min-max deixou — a PCA
# roda sobre a covariância dos subíndices como o índice de fato os usa. (scale. = TRUE imporia
# variância 1 a todos, desfazendo a normalização já feita.)
pca_dim <- prcomp(ufs[, c("i.longev", "i.educ", "i.renda")], scale. = FALSE)

# usa as cargas (em módulo) do 1º componente como pesos, normalizados para somar 1
pesos_pca <- abs(pca_dim$rotation[, 1])
pesos_pca <- pesos_pca / sum(pesos_pca)
round(pesos_pca, 3)

# compara o ranking com pesos data-driven ao do IDHM (pesos iguais), via construir_indice()
iguais <- construir_indice(ufs)
datad  <- construir_indice(ufs, pesos = pesos_pca)
cor(iguais$indice, datad$indice, method = "spearman")   # ≈ 1 = ordenamento quase idêntico

######################################################################
## Etapa 7 · Análise de incerteza e sensibilidade
######################################################################

# --- Sensibilidade sistemática (Spearman) ---

# índice de referência (o IDHM padrão), contra o qual compararemos cada cenário
idx_padrao <- construir_indice(ufs)$indice

# list() guarda cenários de natureza mista; cada item é, ele próprio, uma lista com
# [regra de agregação, vetor de pesos]. Os nomes à esquerda do '=' rotulam cada cenário
cenarios <- list(
  "renda dobrada"            = list("geometrica", c(1, 1, 2)),
  "educação dobrada"         = list("geometrica", c(1, 2, 1)),
  "índice social (s/ renda)" = list("geometrica", c(1, 1, 0)),
  "aritmética (iguais)"      = list("aritmetica",  c(1, 1, 1))
)

# sapply() aplica a mesma função a cada cenário da lista e junta os resultados num vetor.
# Dentro da função, cen[[1]] é a regra e cen[[2]] o vetor de pesos daquele cenário
# (colchetes duplos [[ ]] extraem UM elemento de uma lista)
spearman <- sapply(cenarios, function(cen) {
  idx <- construir_indice(ufs, agregacao = cen[[1]], pesos = cen[[2]])$indice
  # correlação de Spearman entre o ranking padrão e o do cenário (1 = ordenamento idêntico)
  round(cor(idx_padrao, idx, method = "spearman"), 3)
})
sort(spearman, decreasing = TRUE)   # ordena do mais estável (≈1) ao menos estável

# --- Incerteza por simulação de Monte Carlo ---

# fixa a semente do gerador aleatório: garante que os "sorteios" saiam iguais a cada execução
# (reprodutibilidade). Qualquer número serve; usamos 2026
set.seed(2026)

# replicate(1000, {...}) repete o bloco entre chaves 1.000 vezes e empilha os resultados
# em colunas — cada coluna é o ranking das 27 UFs em uma simulação
sim <- replicate(1000, {
  w   <- runif(3, 0.5, 2)                            # 3 pesos sorteados ao acaso entre 0,5 e 2
  agr <- sample(c("geometrica", "aritmetica"), 1)    # sorteia 1 das 2 regras de agregação
  construir_indice(ufs, agregacao = agr, pesos = w)$rank
})
rownames(sim) <- ufs$NOME   # nomeia as linhas da matriz com as UFs

# prepara os dados para o gráfico: transforma a matriz (UFs × 1.000 simulações) em formato
# "longo" (uma linha por par UF–simulação), que é o que o ggplot espera
mc <- as.data.frame(sim) |>
  mutate(uf = ufs$NOME) |>
  # pivot_longer empilha todas as colunas de simulação numa só coluna "rank"; o -uf significa
  # "todas as colunas, exceto uf"
  pivot_longer(-uf, values_to = "rank") |>
  # reordena as UFs pela posição MEDIANA, para o gráfico sair ordenado do melhor ao pior
  mutate(uf = reorder(uf, rank, median))

# ggplot constrói o gráfico em camadas, somadas com '+'. aes() mapeia variáveis a eixos;
# geom_boxplot() desenha, para cada UF, a caixa que resume a distribuição das 1.000 posições
ggplot(mc, aes(rank, uf)) +
  geom_boxplot(outlier.size = 0.4, fill = "#d6e4f0", color = "#2980b9") +
  labs(x = "Posição no ranking (1 = melhor)", y = NULL) +   # rótulos dos eixos
  theme_minimal()                                           # tema visual limpo

# --- O efeito da transformação logarítmica ---

# refaz a normalização da renda SEM o logaritmo (min-max linear), para isolar o efeito do log
renda_sem_log <- (ufs$RDPC - 8) / (4033 - 8)
# recompõe o índice com essa renda alternativa, mantendo longevidade e educação
i_sem_log <- (ufs$i.longev * ufs$i.educ * renda_sem_log)^(1/3)

# compara as posições com e sem log lado a lado
data.frame(uf = ufs$NOME,
           rank_com_log = rank(-ufs$idhm, ties.method = "min"),
           rank_sem_log = rank(-i_sem_log, ties.method = "min")) |>
  mutate(mudanca = rank_com_log - rank_sem_log) |>   # quantas posições cada UF se desloca
  arrange(desc(mudanca)) |> head(10)                 # as 10 que mais sobem ao retirar o log

######################################################################
## Etapa 8 · Volta às dimensões
######################################################################

ufs |>
  select(idhm, i.longev, i.educ, i.renda) |>          # mantém só o índice e os três subíndices
  # empilha as três colunas de dimensão em duas: "dimensao" (o nome) e "valor" (o número)
  pivot_longer(c(i.longev, i.educ, i.renda), names_to = "dimensao", values_to = "valor") |>
  # recode() troca os códigos internos por rótulos legíveis para a legenda do gráfico
  mutate(dimensao = recode(dimensao,
                           i.longev = "Longevidade", i.educ = "Educação", i.renda = "Renda")) |>
  ggplot(aes(valor, idhm)) +                           # eixo x = valor da dimensão, y = IDHM
  geom_point(color = "#2980b9") +                      # um ponto por UF
  geom_smooth(method = "lm", se = FALSE, color = "#c0392b") +  # reta de regressão (lm), sem faixa de erro
  facet_wrap(~dimensao) +                              # um painel separado para cada dimensão
  labs(x = "Valor da dimensão", y = "IDHM") +
  theme_minimal()

# --- Mesmo índice, perfis distintos ---

# seleciona as duas UFs e empilha seus três subíndices para o gráfico
perfil <- ufs |>
  filter(NOME %in% c("Rio Grande do Norte", "Roraima")) |>
  select(NOME, i.longev, i.educ, i.renda) |>
  pivot_longer(-NOME, names_to = "dimensao", values_to = "valor") |>
  mutate(dimensao = recode(dimensao,
                           i.longev = "Longevidade", i.educ = "Educação", i.renda = "Renda"))

# barras agrupadas (position = "dodge") comparam, dimensão a dimensão, as duas UFs
ggplot(perfil, aes(dimensao, valor, fill = NOME)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Subíndice (0–1)", fill = NULL) +
  theme_minimal()

# volta à base completa 'radar' para comparar dois anos
delta <- radar |>
  # %in% testa se ANO é um dos valores do vetor c(2012, 2024) — guarda só esses dois anos
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  select(NOME, ANO, IDHM_L, IDHM_E, IDHM_R) |>
  # pivot_wider faz o oposto de pivot_longer: "alarga" a tabela, criando colunas separadas por
  # ano (IDHM_L_2012, IDHM_L_2024, ...), o que permite subtrair um ano do outro na mesma linha
  pivot_wider(names_from = ANO, values_from = c(IDHM_L, IDHM_E, IDHM_R)) |>
  transmute(uf      = NOME,
            d_long  = IDHM_L_2024 - IDHM_L_2012,   # variação 2012→2024 de cada dimensão
            d_educ  = IDHM_E_2024 - IDHM_E_2012,
            d_renda = IDHM_R_2024 - IDHM_R_2012)

# colMeans() tira a média de cada coluna — aqui, a variação média entre as 27 UFs por dimensão
round(colMeans(delta[, c("d_long", "d_educ", "d_renda")]), 4)

######################################################################
## Etapa 9 · Validação externa
######################################################################

# --- Reprodução do índice oficial ---

# arredondamento comercial (metade para cima), em vez do padrão do R (metade para o par)
arred <- function(x) floor(x * 1000 + 0.5) / 1000

# replica a convenção do Atlas: cada subíndice arredondado a 3 casas antes de combinar;
# na educação, os subcomponentes (estoque e fluxo) também são arredondados
L <- arred(ufs$i.longev)
R <- arred(ufs$i.renda)
E <- arred((arred(ufs$si.escol)^1 * arred(ufs$si.fluxo)^2)^(1/3))
idhm_repro <- arred((L * E * R)^(1/3))

# dispersão do oficial (eixo x) contra o reproduzido (eixo y)
ggplot(ufs, aes(IDHM, idhm_repro)) +
  geom_point(color = "#2980b9", size = 2) +
  # reta de 45° (y = x): se a reprodução bate com o oficial, os pontos caem sobre ela
  geom_abline(slope = 1, intercept = 0, color = "#c0392b", linetype = "dashed") +
  labs(x = "IDHM oficial (Atlas/Radar)", y = "IDHM reproduzido") +
  theme_minimal()

# concordância quantificada: correlação, maior diferença e quantas UFs batem exato (de 27)
c(correlacao        = cor(ufs$IDHM, idhm_repro),
  maior_dif         = max(abs(ufs$IDHM - idhm_repro)),
  iguais_ao_oficial = sum(idhm_repro == ufs$IDHM)) |> round(4)

# cut() converte um número contínuo em categorias, fatiando-o nos pontos de corte informados.
# -Inf e Inf são "menos/mais infinito"; right = FALSE faz o intervalo fechar à esquerda
faixa <- function(x) cut(x, c(-Inf, .5, .6, .7, .8, Inf), right = FALSE,
                         labels = c("Muito Baixo", "Baixo", "Médio", "Alto", "Muito Alto"))

# table() cruza as faixas do oficial com as do reproduzido: a diagonal mostra as concordâncias
table(oficial = faixa(ufs$IDHM), reproduzido = faixa(idhm_repro))

# --- Validação externa (convergente e discriminante) ---

# indicadores EXTERNOS, relacionados mas NÃO usados no índice, vindos da própria base:
# proporção de pobres (PMPOB), mortalidade infantil (MORT1) e índice de Gini (GINI).
# O vetor abaixo guarda, para cada apelido, o nome real da coluna na base
externos <- c(pobreza = "PMPOB", mort_infantil = "MORT1", gini = "GINI")

# sapply() percorre esse vetor e, para cada coluna, correlaciona o nosso índice com ela.
# ufs[[col]] usa o nome guardado em 'col' para pegar a coluna correspondente
sapply(externos, function(col) round(cor(ufs$idhm, ufs[[col]], method = "spearman"), 3))

# --- Comparação com o índice irmão (IDHMAD) ---

# monta uma tabela com o IDHM, o IDHMAD (ajustado à desigualdade), a perda (%) e a posição
# de cada UF nos dois rankings; 'desloca' = quantas posições a UF muda ao passar de um ao outro
ad <- ufs |>
  transmute(uf = NOME, idhm = IDHM, idhmad = IDHMAD, perda = IDHMAD_PERDA,
            rank_idhm   = rank(-IDHM,   ties.method = "min"),
            rank_idhmad = rank(-IDHMAD, ties.method = "min"),
            desloca     = rank_idhm - rank_idhmad)   # positivo = sobe ao ajustar pela desigualdade

# dois resumos: a correlação entre os dois rankings e a perda média (mean = média) entre as UFs
c(spearman   = round(cor(ad$idhm, ad$idhmad, method = "spearman"), 3),
  perda_media = round(mean(ad$perda), 1))            # perda média (%) por desigualdade

# as 8 UFs que mais se deslocam (em valor absoluto) ao ajustar o índice pela desigualdade
arrange(ad, desc(abs(desloca))) |> head(8)

######################################################################
## Etapa 10 · Visualização e comunicação
######################################################################

# st_read() (pacote sf) lê o arquivo geográfico com os contornos das UFs; quiet = TRUE silencia mensagens
uf_geo <- st_read("data/uf_brasil.geojson", quiet = TRUE)

# projeta para SIRGAS 2000 / Polyconic (EPSG:5880), o sistema oficial para mapas do Brasil em metros:
# torna a barra de escala correta (em lat/long a escala variaria com a latitude)
uf_geo <- st_transform(uf_geo, 5880)

# as.integer() converte o código do IBGE em número inteiro, para casar com a base por valor numérico
# (mais seguro que casar por nome, que varia em acento e grafia)
uf_geo$CODIGO <- as.integer(uf_geo$codigo_ibg)
# left_join() cola os dados do IDHM ao mapa, casando pela coluna CODIGO
mapa <- left_join(uf_geo, mutate(ufs, CODIGO = as.integer(CODIGO)), by = "CODIGO")

ggplot(mapa) +
  # geom_sf() desenha os polígonos das UFs; fill = idhm pinta cada uma conforme seu índice
  geom_sf(aes(fill = idhm), color = "white", linewidth = 0.2) +
  # escala de cor contínua viridis (legível p/ daltônicos); direction = -1 inverte (escuro = maior)
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "IDHM") +
  # elementos cartográficos obrigatórios, além do título e da legenda (acima):
  annotation_scale(location = "bl", style = "ticks") +                         # escala
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),       # orientação (norte)
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  labs(title = "IDHM — Unidades da Federação, 2024",
       caption = "Fonte: Atlas do Desenvolvimento Humano / Radar IDHM (PNUD, FJP, IBGE).") +  # fonte
  theme_void()   # tema sem eixos nem grades, adequado a mapas

# paleta oficial do Atlas/PNUD: vermelho → laranja → amarelo → verde → azul
cores_idhm <- c("Muito Baixo" = "#C0392B", "Baixo" = "#E67E22", "Médio" = "#F1C40F",
                "Alto"        = "#1E8449", "Muito Alto" = "#5DADE2")

# IDHM oficial das UFs em 2012 e 2024, discretizado nas 5 faixas com cut()
faixas_anos <- radar |>
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  transmute(CODIGO = as.integer(CODIGO), ANO,
            faixa = cut(IDHM, c(-Inf, .5, .6, .7, .8, Inf), right = FALSE,
                        labels = names(cores_idhm)))

# junta ao mapa: cada UF aparece duas vezes (uma por ano), para os dois mapas lado a lado
mapa_anos <- left_join(uf_geo, faixas_anos, by = "CODIGO")

ggplot(mapa_anos) +
  geom_sf(aes(fill = faixa), color = "white", linewidth = 0.2) +
  facet_wrap(~ANO) +                                          # um mapa por ano, lado a lado
  # scale_fill_manual fixa a cor de cada faixa; drop = FALSE mantém as 5 na legenda,
  # mesmo as ausentes (Muito Baixo, Baixo), para a escala ficar comparável entre anos
  scale_fill_manual(values = cores_idhm, drop = FALSE, name = "Faixa IDHM") +
  labs(caption = "Fonte: Atlas do Desenvolvimento Humano / Radar IDHM (PNUD, FJP, IBGE).") +
  theme_void()

# --- Evolução no tempo (slope chart) ---

# painel com os dois anos; ANO como fator vira eixo categórico (dois pontos no x)
evol <- radar |>
  filter(AGREGACAO == "Unidade da Federação", ANO %in% c(2012, 2024)) |>
  mutate(ANO = factor(ANO))

# group = NOME liga os dois anos de cada UF por uma linha — a "inclinação" de cada trajetória
ggplot(evol, aes(ANO, IDHM, group = NOME)) +
  geom_line(color = "#2980b9", alpha = 0.6) +
  geom_point(color = "#2980b9", size = 1.6) +
  labs(x = NULL, y = "IDHM") +
  theme_minimal()

######################################################################
## Exercício
######################################################################

# troque a agregação e/ou os pesos abaixo para definir o SEU cenário
meu <- construir_indice(ufs, agregacao = "geometrica", pesos = c(1, 1, 1))

# compara o ranking oficial com o do seu cenário e mostra quem mais se desloca
ufs |>
  transmute(uf           = NOME,
            rank_oficial = rank(-IDHM, ties.method = "min"),
            rank_meu     = meu$rank,
            mudanca      = rank_oficial - rank_meu) |>
  arrange(desc(abs(mudanca)))

# mapa do índice modificado: cola o seu índice (meu$indice) ao mapa e pinta as UFs por ele
# (uf_geo já vem projetado da Etapa 10, então a escala fica correta)
mapa_meu <- left_join(uf_geo, mutate(ufs, CODIGO = as.integer(CODIGO), idhm_meu = meu$indice), by = "CODIGO")
ggplot(mapa_meu) +
  geom_sf(aes(fill = idhm_meu), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Meu índice") +
  annotation_scale(location = "bl", style = "ticks") +                         # escala
  annotation_north_arrow(location = "tr", style = north_arrow_minimal(),       # norte
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  labs(caption = "Fonte: Atlas do Desenvolvimento Humano / Radar IDHM (PNUD, FJP, IBGE).") +  # fonte
  theme_void()
