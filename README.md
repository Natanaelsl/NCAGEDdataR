
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
<!-- [![CRAN/METACRAN Version](https://www.r-pkg.org/badges/version/geouy)](https://CRAN.R-project.org/package=geouy) -->
<!-- [![CRAN/METACRAN Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/geouy?color=blue)](https://CRAN.R-project.org/package=geouy)  -->
<!-- [![CRAN/METACRAN downloads per month](https://cranlogs.r-pkg.org/badges/geouy?color=orange)](https://CRAN.R-project.org/package=geouy) -->
<!-- <br /> -->
<!-- [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) -->
<!-- [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/RichDeto/geouy?branch=master&svg=true)](https://ci.appveyor.com/project/RichDeto/geouy) -->

[![R](https://github.com/Natanaelsl/NCAGEDdataR/actions/workflows/r.yml/badge.svg)](https://github.com/Natanaelsl/NCAGEDdataR/actions/workflows/r.yml)
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
library(NCAGEDdataR)

## basic example code
NCdata(2023, "Abril")
#> ⠙ Preparando!
#> [1] 2023
#> [1] "abril"
#> [1] 2023
#> ✔ Preparando! [1.7s]
#> ⠙ Definindo caminho aos dados✔ Definindo caminho aos dados [35ms]
#> ⠙ Download realizado!
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |==                                                                    |   4%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |=======================                                               |  34%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |============================================                          |  64%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |==========================================================            |  84%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%
#> Os dados foram extraidos do link abaixo: 
#>  http://pdet.mte.gov.br/images/Novo_CAGED/2023/202304/3-tabelas.xlsx 
#> Os arquivos foram salvos temporariamente em: 
#>  C:\Users\natan\AppData\Local\Temp\RtmpoDxFCm\CAGED46b8754337da.xlsx
#> ✔ Download realizado! [11.5s]
#> ✔ Organizando dados e criando lista de 'Data.frames'! [12ms]
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
