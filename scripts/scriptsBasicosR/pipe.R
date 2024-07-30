library(dplyr)
library(tibble)

# Limpar dados
mtcars_clean <- mtcars %>%
  rownames_to_column(var = "model") %>%
  as_tibble() %>%
  filter(cyl < 8)

# Selecionar carros com 4 cyl e tirar média de mpg e wt
mtcars_clean %>%
  filter(cyl == 4) %>%
  group_by(cyl) %>%
  summarise(
    mpg = mean(mpg),
    wt = mean(wt)
  )
#> # A tibble: 1 x 3
#>     cyl   mpg    wt
#>   <dbl> <dbl> <dbl>
#> 1     4  26.7  2.29

# Selecionar carros com 6 cyl e tirar média de drat e disp
mtcars_clean %>%
  filter(cyl == 6) %>%
  group_by(cyl) %>%
  summarise(
    drat = mean(drat),
    disp = mean(disp)
  )
#> # A tibble: 1 x 3
#>     cyl  drat  disp
#>   <dbl> <dbl> <dbl>
#> 1     6  3.59  183.

library(dplyr)
library(tibble)

# Limpa tabela, filtrando cyl < cyl_max
clean <- function(data, cyl_max = 8) {
  data %>%
    rownames_to_column(var = "model") %>%
    as_tibble() %>%
    filter(cyl < cyl_max)
}

# Resume tabela onde cyl == cyl_max, tirando média das colunas em ...
summarise_cyl <- function(data, cyl_num, ...) {
  data %>%
    filter(cyl == cyl_num) %>%
    group_by(cyl) %>%
    summarise_at(vars(...), mean)
}

# 4 cyl, média de mpg e wt
mtcars %>%
  clean(cyl_max = 8) %>%
  summarise_cyl(cyl_num = 4, mpg, wt)
#> # A tibble: 1 x 3
#>     cyl   mpg    wt
#>   <dbl> <dbl> <dbl>
#> 1     4  26.7  2.29

# 6 cyl, média de drat e disp
mtcars %>%
  clean(cyl_max = 8) %>%
  summarise_cyl(cyl_num = 6, drat, disp)
#> # A tibble: 1 x 3
#>     cyl  drat  disp
#>   <dbl> <dbl> <dbl>
#> 1     6  3.59  183.

# Referência ao pipe
`%>%` <- magrittr::`%>%`

# Limpa tabela, filtrando cyl < cyl_max
clean <- function(data, cyl_max = 8) {
  data %>%
    tibble::rownames_to_column(var = "model") %>%
    dplyr::as_tibble() %>%
    dplyr::filter(cyl < cyl_max)
}

# Resume tabela onde cyl == cyl_max, tirando média das colunas em ...
summarise_cyl <- function(data, cyl_num, ...) {
  data %>%
    dplyr::filter(cyl == cyl_num) %>%
    dplyr::group_by(cyl) %>%
    dplyr::summarise_at(dplyr::vars(...), mean)
}

# 4 cyl, média de mpg e wt
mtcars %>%
  clean(cyl_max = 8) %>%
  summarise_cyl(cyl_num = 4, mpg, wt)
#> # A tibble: 1 x 3
#>     cyl   mpg    wt
#>   <dbl> <dbl> <dbl>
#> 1     4  26.7  2.29

# 6 cyl, média de drat e disp
mtcars %>%
  clean(cyl_max = 8) %>%
  summarise_cyl(cyl_num = 6, drat, disp)
#> # A tibble: 1 x 3
#>     cyl  drat  disp
#>   <dbl> <dbl> <dbl>
#> 1     6  3.59  183.
