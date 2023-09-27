# 
library(tidyverse)
pib <- readxl::read_excel(here::here("data","pib.xls")) %>% 
  janitor::clean_names()
pib %>% names()
pib <- 
  pib %>% 
  filter(ano == 2020) %>% 
  transmute(
    cod_ibge = codigo_do_municipio,
    uf = tolower(sigla_da_unidade_da_federacao),
    municipio = abjutils::rm_accent(tolower(nome_do_municipio)),
    microrregiao = nome_da_microrregiao,
    mesorregiao = nome_da_mesorregiao,
    nome_da_regiao_rural,
    regiao_metropolitana,
    tipo_concentracao_urbana,
    pib_prc_corr = produto_interno_bruto_a_precos_correntes_r_1_000,
    pib_pc = produto_interno_bruto_per_capita_a_precos_correntes_r_1_00,
    vab_agro = valor_adicionado_bruto_da_agropecuaria_a_precos_correntes_r_1_000,
    vab_ind = valor_adicionado_bruto_da_industria_a_precos_correntes_r_1_000,
    vab_serv = valor_adicionado_bruto_dos_servicos_a_precos_correntes_exceto_administracao_defesa_educacao_e_saude_publicas_e_seguridade_social_r_1_000,
    vab_total = valor_adicionado_bruto_total_a_precos_correntes_r_1_000,
    impostos = impostos_liquidos_de_subsidios_sobre_produtos_a_precos_correntes_r_1_000,
    prop_vab_ag = vab_agro/vab_total,
    prop_vab_ind = vab_ind/vab_total,
    prop_vab_serv = vab_serv/vab_total,
    
  )
pib

frota <- readxl::read_excel(here::here("data","frota.xls"),skip = 2) %>% 
  janitor::clean_names()
frota <- 
  frota %>% 
  transmute(
    uf = tolower(uf),
    municipio = abjutils::rm_accent(tolower(municipio)),
    total_frota = total,
    automovel,
    moto = motocicleta+ motoneta,
    trator = trator_estei+trator_rodas,
    onibus = onibus + micro_onibus
    
  )

# area plantada lavoura temporaria
area_plantada_temp <- 
  readxl::read_excel(here::here("data","tabela1612.xlsx"),skip = 4) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = x1,
            area_plantada_temp = as.numeric(total))

# valor  lavoura temporaria
valor_area_temp <- 
  readxl::read_excel(here::here("data","tabela1612.xlsx"),skip = 4,sheet =2) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
            valor_area_temp = as.numeric(total))

# lavoura permanente
area_plantada_perm <- 
  readxl::read_excel(here::here("data","tabela1613.xlsx"),skip = 4) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
            area_plantada_perm = as.numeric(total))

# valor lavoura permanente
valor_area_perm <- 
  readxl::read_excel(here::here("data","tabela1613.xlsx"),skip = 4,sheet =2) %>% 
  janitor::clean_names()%>% 
  transmute(cod_ibge = x1,
            valor_area_perm = as.numeric(total))


# cabeça de gado
cabeca <- 
  readxl::read_excel(here::here("data","tabela3939.xlsx"),skip = 4,sheet = 1) %>% 
  janitor::clean_names() %>% 
  transmute(
    cod_ibge = as.character(x1),
    bovino,suino_total,galinaceos_total)
# porcos


# população estimada
## avaliar
pop_est <- 
  read.csv2(here::here("data","censo.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(cod_ibge = as.character(cod_ibge))

# area urbanizada
area_urb <- 
  readxl::read_excel(here::here("data","tabela8418.xlsx"),skip = 3,sheet =1) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = as.character(x1),
            area_urbanizada = as.numeric(x2019))

## area maepada
area_mapeada <- 
  readxl::read_excel(here::here("data","tabela8418.xlsx"),skip = 3,sheet =2) %>% 
  janitor::clean_names() %>% 
  transmute(cod_ibge = as.character(x1),
            area_mapeada = as.numeric(x2019))


### empresas



empresas <- read.csv2(here::here("data","tabela993.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(cod_ibge = as.character(cod_ibge))
# estban 
estban <- read.csv2(here::here("data","202306_ESTBAN.csv")) %>% 
  janitor::clean_names() %>% 
  filter(nome_instituicao != "BCO COOPERATIVO SICREDI S.A.") %>% 
  transmute(
    cod_ibge=as.character(codmun_ibge),
    credito = 
      verbete_161_empres_e_tit_descontados         +
      verbete_162_financiamentos                   +
      verbete_163_fin_rurais_agricul_cust_invest   +
      verbete_167_financiamentos_agroindustriais,
    deposito =  verbete_420_depositos_de_poupanca  +
                verbete_432_depositos_a_prazo              
  ) %>% 
    group_by(cod_ibge) %>% 
    summarise(credito  = sum(credito),
              deposito = sum(deposito))
estban

## agencias 
formatar_numero <- function(numero) {
  if (numero < 10) {
    return(sprintf("%02d", numero))
  } else {
    return(as.character(numero))
  }
}

agencias <- 
  read.csv2(here::here("data","202306agencias.csv")) %>% 
  janitor::clean_names() %>% tibble()
agencias <- 
agencias %>% 
  filter(segmento %in%
           c("Banco do Brasil - Banco Múltiplo                            ",
             "Banco Múltiplo                                              ",
             "Caixa Econômica Federal                                     ",
             "Banco Comercial                                             ",
             "Banco Múltiplo Cooperativo                                  ",
             "Banco Comercial Estrangeiro - Filial no país                ")) %>% 
  transmute(
    cod_ibge = as.character(municipio_ibge),
    idade_ag = as.Date(data_inicio,"%d/%m/%Y")
  ) %>% 
  group_by(cod_ibge) %>% 
  summarise(
    min_idade_ag = min(idade_ag),
      qtd_ag = n()
  ) %>% 
  mutate(
    index = as.numeric(min_idade_ag),
    ano = year(min_idade_ag),
    mes = formatar_numero(month(min_idade_ag)),
    anomes = as.numeric(paste0(ano,mes))
  )

lat_long <- 
  read.csv(here::here("data","lat_long.csv")) %>% 
  janitor::clean_names()
lat_long <- 
lat_long %>% 
  transmute(
    cod_ibge = as.character(codigo_ibge),
    latitude,
    longitude
  )

## joins 
dados_ibge <- 
  pib %>% 
  mutate(cod_ibge = as.character(cod_ibge)) %>% 
  left_join(valor_area_temp) %>% 
  left_join(area_plantada_temp) %>% 
  left_join(area_plantada_perm) %>% 
  left_join(valor_area_perm) %>% 
  left_join(cabeca) %>% 
  left_join(pop_est) %>% 
  left_join(area_urb) %>% 
  left_join(area_mapeada) %>%
  left_join(lat_long) %>% 
  left_join(empresas)


my_df <- 
  left_join(dados_ibge,frota) %>% 
  left_join(estban) %>% 
  left_join(agencias)

my_df <- my_df %>% 
  mutate_if(is.numeric,tidyr::replace_na,replace = 0) %>% 
  mutate(
    automovel_pc = automovel / populacao,
    total_auto_pc = total_frota / populacao) %>% 
  mutate_if(is.character,tidyr::replace_na,replace = "NA")

my_df %>% glimpse()
codigos_ibge_capitais <- c(
  5300108, 3304557, 3550308, 1501402, 3106200,
  1400100, 5103403, 4106902, 4205407, 2304400,
  5208707, 2507507, 1600303, 2704302, 1302603,
  2408102, 1721000, 4314902, 1100205, 2611606,
  1200401, 2927408, 2111300, 2211001, 3205309,
  2800308,5002704
)
my_df %>% 
  # remover SP e RJ reduziu muito outliers e o peso da cauda.
  filter(!cod_ibge %in% codigos_ibge_capitais) %>% 
  saveRDS(here::here("data","final.RDS"))
# Vetor com os códigos do IBGE das capitais brasileiras

