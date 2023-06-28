
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NCAGEDdataR

<!-- badges: start -->
<!-- badges: end -->

**NCAGEDdataR** é um pacote R que permite aos usuários acessar facilmente conjuntos de dados do **NOVO CAGED**. 

<img align="right" src="man/figures/NCAGEDdataR_logo1.png" alt="logo" width="180"><img align="right" src="man/figures/NCAGEDdataR_logo2.png" alt="logo" width="180">

- `NCdown`: realiza o download do aquivo .xlsx no site do Ministério do Trabalho - **[PDET](http://pdet.mte.gov.br/novo-caged)** com os dados do NOVO CAGED para o ano e mês especificados.
.xlsx dísponibilizado 
- `NCdata`: realiza o extração dos dados, para o ano e mês especificados, retornando como lista de `data.frame` cada uma das abas do aquivo .xlsx dísponibilizado no site do Ministério do Trabalho - **[PDET](http://pdet.mte.gov.br/novo-caged)**.



<br />

<!-- badges: start -->
<!-- [![CRAN/METACRAN Version](https://www.r-pkg.org/badges/version/geouy)](https://CRAN.R-project.org/package=geouy) -->
<!-- [![CRAN/METACRAN Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/geouy?color=blue)](https://CRAN.R-project.org/package=geouy)  -->
<!-- [![CRAN/METACRAN downloads per month](https://cranlogs.r-pkg.org/badges/geouy?color=orange)](https://CRAN.R-project.org/package=geouy) -->
<!-- <br /> -->
<!-- [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) -->
<!-- [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/RichDeto/geouy?branch=master&svg=true)](https://ci.appveyor.com/project/RichDeto/geouy) -->
<!-- [![R](https://github.com/Natanaelsl/NCAGEDdataR/actions/workflows/r.yml/badge.svg)](https://github.com/Natanaelsl/NCAGEDdataR/actions/workflows/r.yml) -->

![GitHub R package version (subdirectory of
monorepo)](https://img.shields.io/github/r-package/v/Natanaelsl/NCAGEDdataR)
![GitHub Repo
stars](https://img.shields.io/github/stars/Natanaelsl/pagedreport?color=orange)
![GitHub](https://img.shields.io/github/license/Natanaelsl/NCAGEDdataR)

<!-- badges: end -->

<br />

<!-- --- -->

## Instalação

Você pode instalar a versão de desenvolvimento do NCAGEDdataR no
[GitHub](https://github.com/) com:

``` r
# install.packages("devtools")
devtools::install_github("Natanaelsl/NCAGEDdataR")
```

<!-- --- -->

## Exemplo

Este é um exemplo básico que mostra como resolver um problema comum:

``` r
## Carregando o pacote
library(NCAGEDdataR)

## Download do arquivo .xlsx do NOVO CAGED de Abril/2023 na pasta documentos.
# NCdata(2023, "Abril", ".../Documents/")

## Gerando lista de 'data.frame' do NOVO CAGED de Abril/2023.
# NCdata(2023, "Abril")

```

<!-- --- -->

## Informações

### O Novo Caged

Desde janeiro de 2020, o uso do Sistema do Cadastro Geral de Empregados
e Desempregados (Caged) foi substituído pelo Sistema de Escrituração
Digital das Obrigações Fiscais, Previdenciárias e Trabalhistas (eSocial)
para parte das empresas, conforme estabelecido pela Portaria SEPRT nº
1.127, de 14/10/2019. Permanece a obrigatoriedade de envio das
informações por meio do Caged apenas para órgãos públicos e organizações
internacionais que contratam celetistas.

O **Novo Caged** é a geração das estatísticas do emprego formal por meio
de informações captadas dos sistemas eSocial, Caged e Empregador Web.

### Vantagem

A sintaxe da função `NCAGEDdataR` opera com a mesma lógica
independentemente da base de interesse, o que torna intuitivo o download/extração
de qualquer conjunto de dados usando uma única linha de código.
