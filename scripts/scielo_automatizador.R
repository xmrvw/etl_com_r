# Instalar pacman se ainda não estiver instalado
if (!require("pacman")) install.packages("pacman")
library(pacman)
library(ggplot2)
library(ggtext)
install.packages("easyScieloPack")

# Carregar pacotes necessários
pacman::p_load(
  stringr,        
  dplyr,          
  writexl,        
  ggplot2,        
  easyScieloPack, 
  ggtext,         
  ggrepel         
)


# Define pasta de resultados fora do OneDrive
pasta_resultados <- "C:/Projetos R/etl_com_r"

# Cria a pasta caso não exista
if (!dir.exists(pasta_resultados)) {
  dir.create(pasta_resultados, recursive = TRUE)
}


# Buscar artigos com os principais termos
sc_smart <- search_scielo("smart city")
sc_sust  <- search_scielo("sustainable city")
sc_cid   <- search_scielo("cidades inteligentes")

# Unir todas as buscas em uma base única
base_cidades <- bind_rows(
  sc_smart  %>% mutate(termo = "smart city"),
  sc_sust   %>% mutate(termo = "sustainable city"),
  sc_cid    %>% mutate(termo = "cidades inteligentes")
)

# Organizar variáveis
base_cidades <- base_cidades %>%
  mutate(
    ano = as.numeric(year)
  ) %>%
  filter(!is.na(ano))   # excluir registros sem ano


df_agg_ano <- base_cidades %>%
  group_by(ano) %>%
  summarise(n_publicacoes = n()) %>%
  arrange(ano)

# Salvar base consolidada
write_xlsx(base_cidades, file.path(pasta_resultados, "base_cidades_scielo.xlsx"))
write_xlsx(df_agg_ano,  file.path(pasta_resultados, "publicacoes_sciello_ano.xlsx"))

#vizualização

p <- df_agg_ano %>%
  ggplot(aes(x = ano, y = n_publicacoes)) +
  geom_line(color = "darkblue", alpha = .6, linewidth = 1) +
  geom_point(size = 3, color = "darkblue") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "red") +
  geom_text(aes(x = 2015, y = max(n_publicacoes), 
                label = "ODS 2015"), color = "red", vjust = -1) +
  labs(
    title = "Evolução de publicações sobre <b><span style='color:darkblue;'>Cidades Inteligentes e Sustentáveis</span></b>",
    subtitle = "SciELO (2000–2024) | Linha vermelha = adoção dos ODS (2015)",
    x = "Ano",
    y = "Número de artigos",
    caption = "Fonte: SciELO | Elaboração própria"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_markdown(),
    plot.caption = element_text(size = 10)
  )

print(p)

# ANÁLISE COM FOCO PÓS-ODS


# Subconjunto: pós-2015
df_agg_ods <- df_agg_ano %>% filter(ano >= 2015)

# Exportar resultados
write_xlsx(df_agg_ods, file.path(pasta_resultados, "publicacoes_pos_ODS.xlsx"))

# Calcular crescimento relativo (pós-2015 vs pré-2015)
crescimento <- (sum(df_agg_ods$n_publicacoes) / 
                  sum(df_agg_ano$n_publicacoes[df_agg_ano$ano < 2015])) - 1
crescimento

