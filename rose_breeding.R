
roses <- flower_tbl %>%
  filter(species == "rose") %>%
  select(flower_id, colour, geneotype, starts_with("gene_"), seed_bag)

rose_genetics <- roses %>%
  select(flower_id, colour, starts_with("gene_")) %>%
  pivot_longer(cols = c(-flower_id, -colour), 
               names_to = "gene", 
               values_to = "gene_val") %>%
  mutate(
    allele_1 = str_sub(gene_val, 1, 1),
    allele_2 = str_sub(gene_val, 2, 2)
  ) %>%
  pivot_longer(cols = c(allele_1, allele_2), 
               names_to = "allele",
               values_to = "allele_val") %>%
  select(flower_id, gene, allele, allele_val)

rose_pairs <- purrr::cross(list(roses$flower_id, roses$flower_id, c("allele_1", "allele_2"), c("allele_1", "allele_2"))) %>%
  enframe(name = "combo_id", value = "value") %>%
  unnest(value) %>%
  mutate(
    value = flatten_chr(value),
    position = rep(rep(c("parent_a", "parent_b"), 2), nrow(.)/4),
    type = rep(c("flower_id", "flower_id", "allele", "allele"), nrow(.)/4)
    ) %>%
  pivot_wider(names_from = type, values_from = value)

rose_breeding <- rose_pairs %>%
  left_join(rose_genetics, by = c("flower_id", "allele")) %>%
  group_by(combo_id, gene) %>%
  summarise(
    combo_parents = paste0(flower_id, collapse = " x "),
    combo_gene = paste0(allele_val, collapse = "")
  ) %>%
  group_by(combo_id, combo_parents) %>%
  arrange(gene) %>%
  summarise(
    combo_geneotype = paste0(combo_gene, collapse = "")
  )