
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NCAGEDdataR

<!-- badges: start -->
<!-- badges: end -->

<img align="right" src="man/figures/NCAGEDdataR_logo1.png" alt="logo" width="180"><img align="right" src="man/figures/NCAGEDdataR_logo2.png" alt="logo" width="180">

**NCAGEDdataR** é um pacote R que permite aos usuários acessar
facilmente conjuntos de dados do **NOVO CAGED**. O pacote realiza a
extração dos dados retornando as tabelas (`as.data.frame`) como
*objetos* para cada uma das abas do aquivo .xlsx dísponibilizado no site
do Ministério do Trabalho -
**[PDET](http://pdet.mte.gov.br/novo-caged)**.

<br />

<!-- badges: start -->

[![CRAN/METACRAN
Version](https://www.r-pkg.org/badges/version/geouy)](https://CRAN.R-project.org/package=geouy)
[![CRAN/METACRAN Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/geouy?color=blue)](https://CRAN.R-project.org/package=geouy)
[![CRAN/METACRAN downloads per
month](https://cranlogs.r-pkg.org/badges/geouy?color=orange)](https://CRAN.R-project.org/package=geouy)
<br /> [![Project Status: Active – The project has reached a stable,
usable state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/RichDeto/geouy?branch=master&svg=true)](https://ci.appveyor.com/project/RichDeto/geouy)
[![R build
status](https://github.com/RichDeto/geouy/workflows/R-CMD-check/badge.svg)](https://github.com/RichDeto/geouy/actions)
![GitHub Repo
stars](https://img.shields.io/github/stars/Natanaelsl/pagedreport?color=orange)

<!-- badges: end -->

<br />

<!-- --- -->

## Installation

You can install the development version of NCAGEDdataR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Natanaelsl/NCAGEDdataR")
```

<!-- --- -->

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(NCAGEDdataR)

## basic example code
# NCAGEDdataR(2023, "Abril")
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

### Carregando Dados

A sintaxe da função `NCAGEDdataR` opera com a mesma lógica
independentemente da base de interesse, o que torna intuitivo o download
de qualquer conjunto de dados usando uma única linha de código. Assim:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.