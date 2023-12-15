library(BuenColors)
library(ggplot2)
library(ggrepel)
library(tidyverse)
data_summary = readxl::read_excel('F:\\scLT-seq db\\scLTdb summary.xlsx')
###tissue
tissue_summary = as.data.frame(table(unlist(strsplit(data_summary$tissue,','))))
tissue_summary  <- tissue_summary  %>%
  mutate(csum = rev(cumsum(rev(Freq))),
         pos = Freq/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Freq/2, pos))
png(filename = 'F:\\scLT-seq db\\out_result\\statistic plot/pie_tissue.png',
    width = 2400,height = 2000,res = 400)
ggplot(tissue_summary, aes(x = "" , y = Freq, fill =  fct_inorder(Var1))) +
  geom_col(width = 1, color = 0) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = jdb_palette("brewer_spectra"))+
  guides(fill = guide_legend(title = "Tissue")) +
  theme_void()+theme(text = element_text(family = "Arial", size = 12))
dev.off()
###tech
tissue_summary = as.data.frame(table(unlist(strsplit(data_summary$Technology,','))))
tissue_summary  <- tissue_summary  %>%
  mutate(csum = rev(cumsum(rev(Freq))),
         pos = Freq/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Freq/2, pos))
png(filename = 'F:\\scLT-seq db\\out_result\\statistic plot/pie_tech.png',
    width = 2400,height = 2000,res = 400)
ggplot(tissue_summary, aes(x = "" , y = Freq, fill =  fct_inorder(Var1))) +
  geom_col(width = 1, color = 0) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = jdb_color_map(names(jdb_color_maps)))+
  guides(fill = guide_legend(title = "Technology")) +
  theme_void()+theme(text = element_text(family = "Arial", size = 12))
dev.off()
###species
tissue_summary = as.data.frame(table(unlist(strsplit(data_summary$species,','))))
tissue_summary  <- tissue_summary  %>%
  mutate(csum = rev(cumsum(rev(Freq))),
         pos = Freq/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Freq/2, pos))
png(filename = 'F:\\scLT-seq db\\out_result\\statistic plot/pie_species.png',
    width = 2400,height = 2000,res = 400)
ggplot(tissue_summary, aes(x = "" , y = Freq, fill =  fct_inorder(Var1))) +
  geom_col(width = 1, color = 0) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = jdb_palette("brewer_spectra"))+
  guides(fill = guide_legend(title = "Species")) +
  theme_void()+theme(text = element_text(family = "Arial", size = 12))
dev.off()
