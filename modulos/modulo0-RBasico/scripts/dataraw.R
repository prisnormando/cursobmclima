library(magrittr)

# Limpar a base dados.xlsx
dados <- "data-raw/dados.xlsx" %>%
  readxl::read_xlsx() %>%
  dplyr::filter(cyl > 4) %>%
  dplyr::mutate(
    brand = stringr::str_extract(model, "^[A-z]+")
  ) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(
    mean_mpg = mean(mpg),
    prop_6_cyl = sum(cyl == 6)/dplyr::n()
  ) %>%
  dplyr::arrange(brand)

# Salvar a base para uso no pacote
usethis::use_data(dados)
#> ✔ Creating 'data/'
#> ✔ Saving 'dados' to 'data/dados.rda'