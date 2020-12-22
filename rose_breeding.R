
roses <- flower_tbl %>%
  filter(species == "rose") %>%
  select(flower_id, colour, geneotype, starts_with("gene_"), seed_bag)

rose_breeding <- roses %>%
  select(flower_id, colour, starts_with("gene_")) %>%
  pivot_longer(cols = c(-flower_id, -colour), names_to = "gene_num") %>%
  separate(value, into = c("allele_1", "allele_2"), sep = character())
