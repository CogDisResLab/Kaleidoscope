go_gene_table %>% 
  ggplot(aes(DataSet, Log2FC, fill = Ref )) + geom_violin()  +
  geom_boxplot(width = 0.3) + geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6, size = 5),
        title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        legend.title = element_blank()
  )

fct_reorder(go_gene_table$DataSet, go_gene_table$Ref)

factor(go_gene_table$DataSet, levels = unique(go_gene_table[order(go_gene_table$Ref), "DataSet"]))

go_gene_table$DataSet <- factor(go_gene_table$DataSet, levels = go_gene_table$DataSet[order(go_gene_table$Ref)] %>% unique())

df$derma <- factor(df$derma, levels = df$derma[order(df$prevalence)])