Como criar um pacote no R
================
Guilherme Ferreira
Publicado em: 05/08/2026

## Configuração inicial

### Bibliotecas

Carregamos as seguintes bibliotecas do **R** para adicionar
funcionalidades extras:

| Pacote     | Função                                             |
|------------|----------------------------------------------------|
| *rvest*    | Web scraping (coleta de dados HTML)                |
| *xml2*     | Parsing e manipulação de documentos XML/HTML       |
| *dplyr*    | Manipulação e transformação de dados               |
| *tidyr*    | Arrumação e organização de dados                   |
| *stringr*  | Manipulação de strings e expressões regulares      |
| *pdftools* | Extração de texto e metadados de arquivos PDF      |
| *purrr*    | Programação funcional com vetores e listas         |
| *knitr*    | Geração de relatórios dinâmicos em RMarkdown       |
| *tibble*   | Estruturas de dados modernas (tibbles)             |
| *devtools* | Ferramentas para desenvolvimento de pacotes        |
| *usethis*  | Automação de tarefas no desenvolvimento de pacotes |

### Diretório de trabalho

Definimos o diretório de trabalho:

``` r
# Definir diretório de trabalho 
setwd("/caminho/diretorio/MachadoR")  
```

## Introdução

A melhor maneira de compartilhar código em **R** é através do objeto
denominado *pacote*, que inclue funcões reutilizáveis, documentação de
uso e dados. Não deve ser confundido com *biblioteca*, que é o diretório
onde estão armazenados os *pacotes* instalados, do mesmo modo que uma
biblioteca física contém livros. A confusão semântica ocorre sobretudo
porque a função usada para carregar um *pacote* se escreve utilizando o
mesmo termo para chamada da *biblioteca*: *library(pacote)*.

Desenvolvi um *pacote* com as obras completas de Machado de Assis para
análise de texto no **R**, a exemplo dos existes para Jane Austen
(*janeaustenr*), H. C. Andersen (*hcandersenr*), Harry Potter
(*harrypotter*) e Projeto Gutemberg (*gutenbergr*), para suprir uma
lacuna já que, a exemplo desse último, o *pacote* **literaturaBR**
também não abrange a totalidade da produção escrita do autor.

O processo envolve organizar os textos, documentá-los e empacotá-los
seguindo as convenções do **R**. O *pacote* **janeaustenr**, por
exemplo, contém os textos completos dos romances de Jane Austen prontos
para análise. Para criar um similar para Machado de Assis, seguimos os
passos de desenvolvimento de *pacotes*, focando na inclusão e
documentação dos dados.

Nesta primeira versão, os textos não foram submetidos a tratamento
prévio, de modo que cabe ao usuário realizar a limpeza dos dados, antes
de proceder à análise.

## Base de dados

Reaproveitamos os dados que já haviam sido extraídos do projeto de
edição das obras de [Machado de Assis](https://machado.mec.gov.br/) em
formato digital, elaborado pelo **MEC**, quando publicamos um estudo
preliminar sobre análise computacional do *corpus* machadiano, com uso
de ferramentas para processamento de linguagem natural e análise
estatística baseadas em [*shell
scripts*](https://github.com/guiajf/machado).

## Passo a passo para criar o *pacote*

### Preparamos o ambiente

Primeiro, instalamos e carregamos as ferramentas necessárias para a
criação do pacote, no ambiente de desenvolvimento **RStudio**.

``` r
# Instalar pacotes necessários, se ainda não os tiver
install.packages(c("devtools", "usethis", "roxygen2", "testthat"))

# Carregar os pacotes
library(devtools)
library(usethis)
```

### Definimos a estrutura do *pacote*

Usamos a função *create_package()* para iniciar o novo *pacote*.

``` r
# Cria o pacote em um novo diretório
create_package("/caminho/diretorio/machador")
```

Isso criará uma estrutura de pastas com arquivos essenciais, como
**DESCRIPTION**, **NAMESPACE** e pastas como **R/** e **man/**.

### Organizamos os dados

Esta é a etapa mais trabalhosa, quando colocamos os dados de texto no
formato correto. Realizamos o pré-processamento dos dados que serão
disponibilizados para os usuários, no formato de arquivos *.rda*.

    • Criamos a pasta data-raw/: usamos a função use_data_raw() para criar uma pasta onde colocamos o script de preparação dos dados. Isso é importante para manter um registro de como os dados brutos foram processados.

``` r
use_data_raw()
```

    • Preparamos os dados: dentro da pasta data-raw/, criamos um script (ex: preparar_obras_machado.R). Neste script, carregamos os arquivos PDF, processamos e organizamos os textos em um objeto do R, por exemplo, um único tibble com uma coluna para o título da obra e outra para o texto.

``` r
# Caminho para os PDFs
pasta_pdfs <- "/caminho/diretorio/MachadoR"

# Função para carregar um PDF individual
carregar_pdf_individual <- function(arquivo_pdf) {
  tryCatch({
    # Extrair texto do PDF
    texto_completo <- pdf_text(arquivo_pdf) %>%
      paste(collapse = "\n") %>%
      str_split("\n") %>%
      unlist()

    # Converter para dataframe
    tibble(text = texto_completo)
  }, error = function(e) {
    warning(paste("Erro ao ler", arquivo_pdf, ":", e$message))
    return(NULL)
  })
}

# Função para extrair título da primeira linha
extrair_titulo <- function(df_texto) {
  if (is.null(df_texto) || nrow(df_texto) == 0) {
    return(NA_character_)
  }

  # Procurar primeira linha não vazia
  primeira_linha <- df_texto %>%
    filter(str_trim(text) != "") %>%
    slice(1) %>%
    pull(text) %>%
    str_trim()

  # Limpar título (remover números romanos, etc)
  titulo <- primeira_linha %>%
    str_remove("^CAP[ÍI]TULO\\s+[IVXLCDM]+\\s*[-–]?\\s*") %>%
    str_remove("^ATO\\s+(PRIMEIRO|SEGUNDO|TERCEIRO)\\s*[-–]?\\s*") %>%
    str_remove("^PERSONAGENS\\s*$") %>%
    str_trim()

  return(titulo)
}



# Função para limpar texto (versão simplificada)
limpar_texto_simples <- function(df_texto) {
  df_texto %>%
    filter(
      str_trim(text) != "",
      !str_detect(text, "^%PDF"),
      !str_detect(text, "^[0-9]+ 0 obj"),
      !str_detect(text, "^endobj$"),
      !str_detect(text, "^stream$"),
      !str_detect(text, "^endstream$")
    ) %>%
    mutate(
      text = str_trim(text),
      text = str_squish(text)
    ) %>%
    filter(text != "")
}

# Pipeline completo para processar todos os PDFs
processar_todas_obras <- function(pasta) {
  # Listar todos os PDFs
  arquivos_pdf <- list.files(pasta, pattern = "\\.pdf$", full.names = TRUE)

  cat("Encontrados", length(arquivos_pdf), "arquivos PDF\n\n")

  # Processar cada arquivo
  resultados <- map_dfr(seq_along(arquivos_pdf), ~{
    arquivo <- arquivos_pdf[.x]
    nome_arquivo <- basename(arquivo)

    cat("Processando", .x, "/", length(arquivos_pdf), ":", nome_arquivo, "\n")

    # Carregar PDF
    df_texto <- carregar_pdf_individual(arquivo)

    if (is.null(df_texto) || nrow(df_texto) == 0) {
      cat("Arquivo vazio ou erro\n")
      return(tibble())
    }

    # Extrair metadados
    titulo <- extrair_titulo(df_texto)

    # Limpar texto
    df_limpo <- limpar_texto_simples(df_texto)

    if (nrow(df_limpo) == 0) {
      cat("Texto vazio após limpeza\n")
      return(tibble())
    }

    # Juntar texto em uma única string
    texto_completo <- paste(df_limpo$text, collapse = "\n")

    # Retornar dataframe com metadados e texto
    tibble(
      titulo = titulo,
      texto = texto_completo
    )
  })

  cat("\n Processamento concluído!\n")
  cat("Total de obras processadas:", nrow(resultados), "\n")

  return(resultados)
}

# Executar processamento
cat("Iniciando processamento de todas as obras...\n\n")
obras_completas <- processar_todas_obras(pasta_pdfs)


obras_textos <- obras_completas %>%
  select(titulo, texto) %>%
  filter(!is.na(titulo))


cat("\nDataframes criados:\n")
cat("  - obras_textos:", nrow(obras_textos), "obras\n")

# Salvar em arquivos .rda
dir.create("machador/data", recursive = TRUE, showWarnings = FALSE)

save(obras_textos, file = "machador/data/obras_textos.rda")

cat("\nArquivos salvos:\n")
cat("  - machador/data/obras_textos.rda\n")
```

    • Criamos outro script (ex: extrair_metadados.R). Neste script, extraímos os metadados dos códigos fontes das páginas que fornecem o acesso às obras por gênero, devidamente enumeradas, depois de salvas manualmente. Os arquivos foram salvos como view-source do Chrome, que formata o código fonte com numeração de linhas e syntax highlighting. O HTML real está codificado dentro das tags <td class="line-content"> com entidades HTML (&lt; em vez de <). 

    Este código:
    1. Lê o arquivo view-source
    2. Extrai o conteúdo de todas as linhas (td.line-content)
    3. Decodifica as entidades HTML (&lt; → <, &gt; → >, etc.)
    4. Reconstrói o HTML original
    5. Parseia o HTML reconstruído
    6. Extrai os dados dos elementos div.item, div.titulo e div[class='detalhe ano']

``` r
diretorio <- "/caminho/diretorio/machador/"
arquivos <- list.files(diretorio, pattern = "\\.html$", full.names = TRUE)

lista_dfs <- list()

for (i in seq_along(arquivos)) {
  arquivo <- arquivos[i]
  
  # Extrair o gênero do nome do arquivo
  nome_arquivo <- basename(arquivo)
  genero <- str_extract(nome_arquivo, "(?<=-)[^-]+(?=\\.html$)")
  
  # Ler o arquivo view-source
  html_view <- read_html(arquivo)
  
  # Extrair todo o conteúdo das linhas
  linhas <- html_view %>% html_elements("td.line-content") %>% html_text()
  
  # Juntar todas as linhas
  html_codificado <- paste(linhas, collapse = "\n")
  
  # Decodificar entidades HTML (&lt; -> <, &gt; -> >, etc.)
  html_decodificado <- html_codificado %>%
    str_replace_all("&lt;", "<") %>%
    str_replace_all("&gt;", ">") %>%
    str_replace_all("&quot;", "\"") %>%
    str_replace_all("&amp;", "&") %>%
    str_replace_all("&apos;", "'")
  
  # Extrair apenas a parte do HTML útil (remover DOCTYPE se estiver repetido)
  html_limpo <- str_extract(html_decodificado, "(?s)<html.*?>.*</html>")
  
  if (is.na(html_limpo)) {
    # Tentar extrair de outra forma
    html_limpo <- html_decodificado
  }
  
  # Parsear o HTML decodificado
  tryCatch({
    html_final <- read_html(html_limpo)
    
    # Agora extrair os itens
    itens <- html_final %>% html_elements("div.item")
    
    titulos <- c()
    anos <- c()
    
    for (item in itens) {
      titulo <- item %>% html_element("div.titulo") %>% html_text(trim = TRUE)
      ano <- item %>% html_element("div[class='detalhe ano']") %>% html_text(trim = TRUE)
      
      if (!is.na(titulo) && titulo != "") {
        titulos <- c(titulos, titulo)
        anos <- c(anos, ifelse(is.na(ano) || ano == "", NA, ano))
      }
    }
    
    if (length(titulos) > 0) {
      df_temp <- data.frame(
        titulo = titulos,
        ano = anos,
        genero = genero,
        stringsAsFactors = FALSE
      )
      
      lista_dfs[[i]] <- df_temp
      cat(sprintf("Arquivo %d (%s): %d itens extraídos\n", i, genero, nrow(df_temp)))
    } else {
      cat(sprintf("Arquivo %d (%s): Nenhum item encontrado\n", i, genero))
    }
    
  }, error = function(e) {
    cat(sprintf("Erro no arquivo %d (%s): %s\n", i, genero, e$message))
  })
}

# Combinar todos os dataframes
metadados_completos <- bind_rows(lista_dfs)

cat(sprintf("\nTotal de registros: %d\n", nrow(metadados_completos)))

if (nrow(metadados_completos) > 0) {
  print(head(metadados_completos, 10))
  save(metadados_completos, file = "machador/data/obras_metadata.rda")
  cat("Arquivo 'obras_metadata.rda' salvo com sucesso!\n")
} else {
  cat("Nenhum dado extraído.\n")
}
```

    • Criamos o script "stopwords_pt.R" para converter o arquivo texto contendo as palavras mais comuns do idioma para o formato adequado.

``` r
# Ler o arquivo de texto
stopwords_pt <- tibble(palavra = readLines("data-raw/stpw.txt", encoding = "UTF-8"))

# Salvar como .rda na pasta data/
usethis::use_data(stopwords_pt, overwrite = TRUE)
```

    • Alternativamente, usamos a função usethis::use_data no script acima para salvar o tibble como um arquivo .rda na pasta data/, tornando-o acessível quando o pacote for carregado. O mesmo procedimento poderia ser adotado para salvar os arquivos anteriores.

### Atualizamos o *.Rbuildignore*

Para que o arquivo *stpw.txt* e o script de criação não sejam
empacotados e enviados para o computador do usuário final (economizando
espaço), adicionamos esta linha ao final do arquivo *.Rbuildignore*

``` r
^data-raw$
```

### Preparamos a documentação dos *datasets*

A documentação é crucial para que os usuários saibam como utilizar os
dados. Deve ser criado um arquivo de documentação para o *dataset*.

Usmos *usethis::use_r(“machador”)* para criar um *script* **R** na pasta
*R/* onde será colocada a documentação no formato *roxygen2*.

Depois de escrever a documentação, executamos *devtools::document()*
para gerar os arquivos de ajuda na pasta *man/*.

``` r
#' Metadados das obras de Machado de Assis
#'
#' Dataset contendo informações sobre as obras completas de Machado de Assis
#' disponíveis no site do MEC.
#'
#' @format Um data frame com colunas:
#' \describe{
#'   \item{titulo}{Título da obra}
#'   \item{ano}{Ano de publicação (quando disponível)}
#'   \item{genero}{Gênero literário (romance, conto, poesia, crônica, teatro)}
#'   \item{arquivo}{Nome do arquivo PDF original}
#' }
#' @source \url{https://machado.mec.gov.br/}
"obras_metadata"

#' Textos completos das obras de Machado de Assis
#'
#' Dataset contendo os textos completos das obras de Machado de Assis,
#' limpos e prontos para análise de texto.
#'
#' @format Um data frame com colunas:
#' \describe{
#'   \item{titulo}{Título da obra}
#'   \item{texto}{Texto completo da obra como string única}
#' }
#' @source \url{https://machado.mec.gov.br/}
"obras_textos"

#' Stopwords em Português
#'
#' Lista de palavras vazias (stopwords) em português para filtragem
#' em análises de texto.
#'
#' @format Um tibble com uma coluna:
#' \describe{
#'   \item{palavra}{A palavra vazia (character)}
#' }
"stopwords_pt"
```

### Definimos as funções auxiliares

Criamos o arquivo *machador/R/funcoes_auxiliares.R*:

``` r
#' Filtrar obras por gênero
#'
#' @param genero Vetor de gêneros literários
#' @return Data frame com metadados das obras filtradas
#' @export
#' @examples
#' filtrar_por_genero("romance")
#' filtrar_por_genero(c("romance", "conto"))
filtrar_por_genero <- function(genero) {
  machador::obras_metadata %>%
    dplyr::filter(genero %in% !!genero)
}

#' Obter texto de uma obra específica
#'
#' @param titulo Título da obra
#' @return Texto completo da obra
#' @export
#' @examples
#' obter_texto("Dom Casmurro")
obter_texto <- function(titulo) {
  machador::obras_textos %>%
    dplyr::filter(titulo == !!titulo) %>%
    dplyr::pull(texto)
}

#' Preparar texto para análise com tidytext
#'
#' @param titulo Título da obra
#' @return Data frame tokenizado pronto para tidytext
#' @export
#' @examples
#' preparar_tidytext("Memórias Póstumas de Brás Cubas")
preparar_tidytext <- function(titulo) {
  texto <- obter_texto(titulo)

  if (length(texto) == 0) {
    stop("Obra não encontrada")
  }

  tibble::tibble(texto = texto) %>%
    tidytext::unnest_tokens(palavra, texto)
}

#' Listar todas as obras disponíveis
#'
#' @return Data frame com todas as obras
#' @export
listar_obras <- function() {
  machador::obras_metadata
}

#' Buscar obras por palavra-chave no título
#'
#' @param palavra Palavra-chave para busca
#' @return Data frame com obras que contêm a palavra no título
#' @export
#' @examples
#' buscar_por_titulo("Casmurro")
buscar_por_titulo <- function(palavra) {
  machador::obras_metadata %>%
    dplyr::filter(stringr::str_detect(titulo, regex(palavra, ignore_case = TRUE)))
}
```

### Identidade visual: a logomarca 

Para dar identidade visual ao pacote, criei uma logomarca hexagonal (padrão na comunidade **R**) com referência direta a Machado de Assis. Utilizei os óculos *pince-nez*, um dos ícones mais reconhecidos do autor, como elemento central do *design*.

A logomarca foi incorporada ao pacote seguindo a convenção da comunidade:

· salvo em man/figures/logo.png;

· referenciado no README.md com alinhamento à esquerda;

· incluída automaticamente no site de documentação.

A criação da logomarca foi uma etapa importante para tornar o pacote mais profissional e facilmente reconhecível pela comunidade. Para esse propósito, contamos com a assistência do Agente autônomo **Manus**, ao qual submetemos o seguinte *prompt*: "Crie um logo hexagonal padrão **R** com fundo violeta exibindo a label "machador" sob o *pincenez* usado por Machado de Assis."

### Preenchemos o arquivo *DESCRIPTION*

Editamos o arquivo *DESCRIPTION* na raiz do *pacote*. Preenchemos os
campos *Title*, *Description*, *Author*, *Maintainer* e *License*.
Escolhemos a licença com *usethis::use_mit_license()*.

``` r
Package: machador
Title: Obras Completas de Machado de Assis para Análise de Texto
Version: 0.1.0
Authors@R: person("Guilherme Ferreira", "G. F.", email = "guilherme@exemplo.com", role = c("aut", "cre"))
Description: Dataset completo das obras de Machado de Assis (1839-1908),
    incluindo romances, contos, poesias, crônicas, teatro, crítica, traduções
    e miscelânea. Os textos foram extraídos da edição do MEC
    (https://machado.mec.gov.br/) e estão formatados para análise de texto
    com tidytext.
License: MIT + file LICENSE
URL: https://github.com/guiajf/machador, https://guiajf.github.io/machador/
BugReports: https://github.com/guiajf/machador/issues
Imports:
    dplyr,
    stringr,
    tibble,
    tidytext
```

### Testamos e instalamos o *pacote*

Por fim, instalmos seu *pacote* localmente para testar se tudo
funcionou.

``` r
# Instala o pacote localmente
devtools::install()

# Carrega o pacote e testa
library(machador)
# Verifica se o dataset está disponível
head(obras_metadata)
```

### Estrutura do *pacote* com funções auxiliares

A estrutura do pacote **R** para dados com funções auxiliares ficou
assim:

<pre style="font-family: monospace; background-color: #f5f5f5; padding: 15px; border-radius: 5px; border-left: 4px solid #2c3e50; line-height: 1.6;">machador/
├── DESCRIPTION          # Metadados do pacote
├── NAMESPACE           # Funções exportadas
├── data/
│   ├── stopwords_pt.rda   <- (O dado final, vai para o usuário)
│   ├── obras_metadata.rda
│   └── obras_textos.rda
├── data-raw/
│   ├── stpw.txt           <- (A matéria-prima, vai para o GitHub)
│   └── stopwords_pt.R     <- (O script de criação, vai para o GitHub)
├── R/
│   ├── data.R             <- (Apenas a documentação roxygen)
│   └── funcoes_auxiliares.R
├── man/                  <- Documentação gerada
├── man/figures/
│   ├── logo.png           <- (A logomarca personalizada do pacote)
└── 
</pre>

### Resumo da organização

``` r
dados <- data.frame(
  Pasta_Arquivo = c(
    "R/funções_auxiliares.R",
    "data-raw/stopwords_pt.R",
    "data-raw/preparar_obras__machado.R",
    "data-raw/extrair_metadados.R",
    "data/obras_textos.rda",
    "data/obras_metadata.rda"
  ),
  Conteudo = c(
    "Funções auxiliares exportadas",
    "Script para criar o arquivo de stopwords",
    "Script para preparar os dados",
    "Script para extrair os metadados",
    "Dataset final contendo as obras de Machado",
    "Dataset contendo os metadados das obras de Machado"
  )
)

kable(dados, format = "html", table.attr = "class='table table-striped'")
```

### Publicamos no **GitHub**

Para controle de versão, utilizamos os seguintes comandos:

``` r
# Configurar Git
usethis::use_git()

# Conectar ao GitHub
usethis::use_github()
```

### Configuramos o **token**

Durante o processo, foi necessário configurar um *Personal Access Token*
com permissões específicas:

*repo*: Para acessar e modificar o repositório;

*workflow*: Para permitir a criação de workflows do GitHub Actions.

``` r
# Configurar Git
usethis::use_git()

# Conectar ao GitHub
gitcreds::gitcreds_set()
```

### Criamos um site com *pkgdown*

Definimos a configuração inicial do site com a documentação em
<https://guiajf.github.io/machador/>.

``` r
# Instalar e configurar pkgdown
install.packages("pkgdown")
usethis::use_pkgdown()

# Configurar GitHub Pages
usethis::use_pkgdown_github_pages()
```

O comando *use_pkgdown_github_pages()* criou automaticamente:

- o arquivo \*\_pkgdown.yml\* com as configurações do site;

- o *workflow* *.github/workflows/pkgdown.yaml* para automação;

- a *branch* *gh-pages* para hospedagem.

### Personalizamos o site

O arquivo \*\_pkgdown.yml\* foi personalizado para organizar as funções
em categorias:

``` r
reference:
  - title: "Funções Principais"
    desc: "Funções para explorar e analisar as obras"
    contents:
      - listar_obras
      - filtrar_por_genero
      - obter_texto
      - preparar_tidytext
      - buscar_por_titulo

  - title: "Datasets"
    desc: "Conjuntos de dados incluídos no pacote"
    contents:
      - obras_metadata
      - obras_textos
      - stopwords_pt
```

### Configuramos o arquivo de *workflow*

Além do *pkgdown.yaml*, configurei o *R-CMD-check.yaml* para verificação
contínua de qualidade, para garantir que o pacote funcione em diferentes
sistemas operacionais e versões do **R**.

``` r
name: R-CMD-check
on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]
```

### Configuramos a automação com **GitHub Actions**

O workflow pkgdown.yaml configurado no GitHub Actions garante que toda
vez que um *push* seja feito para a branch *main* ou *master*, o
**GitHub Actions** baixe o pacote, instala todas as dependências, gere o
site com *pkgdown::build_site()* e publique o site na branch *gh-pages*.
Desse modo, o site sempre estará atualizado com a versão mais recente do
**README** e documentação. Finalmente, para que o site ficasse
acessível, configurei manualmente o *GitHub Pages*.

O resultado está disponível em: <https://guiajf.github.io/machador/>

### Resumimos o fluxo de trabalho

- **Documentação**: *devtools::document()* gera os arquivos *.Rd*;

- **verificação**: *devtools::check()* garante que não haja erros;

- **Commit** e **Push**: *git add .*, *git commit -m “mensagem”*, *git
  push*;

- **build automático**: *GitHub Actions* constrói o site
  automaticamente;

### Conclusão

A criação do pacote **machador** foi uma jornada que combinou a paixão
pela literatura brasileira com as técnicas de desenvolvimento de
software para **R**. O resultado é uma ferramenta que democratiza o
acesso à análise textual das obras de **Machado de Assis**, permitindo
que pesquisadores, estudantes e entusiastas explorem esse rico
patrimônio literário com ferramentas computacionais.

O *pacote* contém 116 obras completas de **Machado de Assis**,
distribuídas em oito gêneros literários (romance, conto, poesia,
crônica, teatro, crítica, tradução, miscelânea); conta com ferramentas
intuitivas para filtragem e extração, integração com *tidytext* para
análises avançadas, site de documentação interativo e atualizado
automaticamente, tudo isso em código aberto e licenciado sob **MIT**.

**Informações úteis**

- **Repositório GitHub**: <https://github.com/guiajf/machador>

- **Site de documentação**: <https://guiajf.github.io/machador/>

- **Instalação**: remotes::install_github(“guiajf/machador”)

### Referências

Boehmke, B. (2016). *harrypotter: Data sets for the Harry Potter
series*. GitHub. <https://github.com/bradleyboehmke/harrypotter>

Bryan, J. (2021). *Happy Git and GitHub for the useR*.
<https://happygitwithr.com/>

Emil Hvitfeldt, E. & Silge, J. (2022). *Supervised Machine Learning for
Text Analysis in R*. <https://smltar.com/>

Ferreira, G. (2026). *machador: obras completas de Machado de Assis*.
GitHub. <https://github.com/guiajf/machado/>

Heiss, A. (2025). *Example: 14 - Example. Data Visualization Spring 2025
course site*.
<https://datavizsp25.classes.andrewheiss.com/example/14-example.html>

Program Historian. (2025). *Processamento básico de textos em R*.
<https://programminghistorian.org/pt/licoes/processamento-basico-texto-r>

ROpenSci. (2016). *gutenbergr: Download and process public domain works
from Project Gutenberg*. GitHub.
<https://github.com/ropensci/gutenbergr>

Silge, J. (2016). *janeaustenr: Jane Austen’s complete novels, ready for
text analysis*. GitHub. <https://github.com/juliasilge/janeaustenr>

Silge, J., & Robinson, D. (2017). *Text Mining with R: A Tidy Approach.
O’Reilly Media*. <https://www.tidytextmining.com/>

Silge, J., & Robinson, D. (2025). *Introduction to tidytext.
R-Universe*.
<https://juliasilge.r-universe.dev/articles/tidytext/tidytext.html>

Sillas Gonzaga. (2017). *literaturaBR: Textos da literatura clássica
brasileira para praticar Text Mining*. GitHub.
<https://github.com/sillasgonzaga/literaturaBR>

West, C. E. (2020). *Packaging Your R Code*.
<https://clarewest.github.io/blog/post/2020-04-23-packaging-your-r-code/>

Wickham, H., & Bryan, J. (2023). *R Packages*. (2nd ed.). O’Reilly
Media. <https://r-pkgs.org/>
