library(tidyverse)

flower_breeding_sheet <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTdwUI4iZE1wdfZv1xdi2qJtldnWS2iiQdjRjKP-4oKoH0R8a07vaVFxZHSwFiDlwzb6gZAE8U5C_vG/pubhtml#"

source_html <- xml2::read_html(flower_breeding_sheet)

source_tbl <- source_html %>% 
  rvest::xml_node("#588946015 > div > table") %>%
  rvest::html_table()

flower_tbl <- source_tbl %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  select(-x1) %>%
  rename(colour = color) %>%
  mutate(
    across(where(is.character), tolower),
    across(starts_with("gene"), 
           ~case_when(
             . == 0 ~ "00",
             . == 1 ~ "01",
             . == 2 ~ "11")),
    seed_bag = if_else(seed_bag == 1, TRUE, FALSE),
    gene_4 = if_else(species != "rose", NA_character_, gene_4),
    geneotype = if_else(
      species == "rose",
      paste0(gene_1, gene_2, gene_3, gene_4),
      paste0(gene_1, gene_2, gene_3)
      ),
    flower_id = paste0(species, "_", strtoi(geneotype, 2))
    )
