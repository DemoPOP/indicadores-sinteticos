# Produção de indicadores sintéticos

[![Licença: CC BY 4.0](https://img.shields.io/badge/Licen%C3%A7a-CC%20BY%204.0-blue.svg)](https://creativecommons.org/licenses/by/4.0/deed.pt-br)
[![Site](https://img.shields.io/badge/site-GitHub%20Pages-1d6e56.svg)](https://demopop.github.io/indicadores-sinteticos/)

Material do **Workshop de Métodos Demográficos — DEMOPOP 2026**, ministrado por **Caio César Soares Gonçalves** (Departamento de Demografia / CEDEPLAR-UFMG). O tutorial reconstrói o **Índice de Desenvolvimento Humano Municipal (IDHM)** para as 27 Unidades da Federação, a partir da base pública do **Radar IDHM (2012–2024)**, percorrendo as **10 etapas** do *Handbook on Constructing Composite Indicators* (OCDE, 2008). Em cada etapa, o IDHM é o caso âncora e catalogam-se as técnicas disponíveis — inclusive as que ele não adota —, de modo que o método possa ser transposto a qualquer conceito multidimensional.

🔗 **Site:** <https://demopop.github.io/indicadores-sinteticos/>

## O que você vai aprender

1. A passagem **conceito → dimensões → indicadores** (a "escada da abstração").
2. A **normalização** de indicadores medidos em escalas distintas.
3. A **ponderação e a agregação** — e como a média aritmética e a geométrica alteram o ordenamento.
4. A **análise de robustez** e a sensibilidade do índice às escolhas metodológicas.
5. A **leitura crítica** de índices sintéticos e de seus limites.

## Conteúdo

- **[Tutorial](https://demopop.github.io/indicadores-sinteticos/tutorial.html)** — reconstrução do IDHM nas 10 etapas da OCDE:
  1. Marco teórico e conceitual
  2. Seleção de dados
  3. Tratamento e preparação dos dados
  4. Análise multivariada
  5. Normalização
  6. Ponderação e agregação
  7. Análise de incerteza e sensibilidade
  8. Volta às dimensões
  9. Validação externa
  10. Visualização e comunicação

## Estrutura dos arquivos

```
.
├── index.qmd       página inicial do site
├── tutorial.qmd    o tutorial (código em R + saídas)
├── _quarto.yml     configuração do site
├── custom.css      tema visual
├── assets/         logos institucionais
└── data/           arquivos básicos para download
    ├── adh_radar_base_2012_2024.xlsx   base pública do Atlas / Radar IDHM
    └── uf_brasil.geojson               malha das Unidades da Federação
```

## Reprodução local

Requer R com os pacotes `readxl`, `dplyr`, `tidyr`, `ggplot2`, `sf` e `ggspatial`. Clone o repositório e, no RStudio, renderize com *Render* (ou `quarto render`), mantendo a pasta `data/` no diretório do projeto.

## Dados

Base pública do **Atlas do Desenvolvimento Humano no Brasil** (PNUD, Ipea e Fundação João Pinheiro) — *Radar IDHM*, estimada a partir da PNAD Contínua/IBGE. Disponível em <https://www.atlasbrasil.org.br/acervo/biblioteca>.

## Como citar

> Gonçalves, C. C. S. (2026). *Produção de indicadores sintéticos* [material de workshop]. Workshop de Métodos Demográficos — DEMOPOP, Departamento de Demografia / CEDEPLAR-UFMG. <https://demopop.github.io/indicadores-sinteticos/>

## Licença

Material distribuído sob a licença [Creative Commons Atribuição 4.0 Internacional (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/deed.pt-br).

---

<p align="center">
  <img src="assets/cedeplar-logo.png" height="46">
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="assets/log_face.png" height="46">
  &nbsp;&nbsp;&nbsp;&nbsp;
  <img src="assets/Logo_UFMG.png" height="46">
</p>
