#devtools::install_github("jiang-junyao/FateMapper")
#devtools::install_github("caleblareau/BuenColors")
library(FateMapper)
library(BuenColors)
library(ggplot2)
### load data
obj_metadata_list = readRDS('obj_metadata_list.rds')

obj_metadata_list2 <- purrr::map(names(obj_metadata_list),function(i){
    obj_metadata_list[[i]] %>% mutate(dataset = i)
}) %>% setNames(names(obj_metadata_list))

saveRDS(obj_metadata_list2,file = 'obj_metadata_list.rds',compress = F)

metadata = obj_metadata_list[[1]]
colnames(metadata)[1:2] = c('UMAP_1','UMAP_2')
### UMAP
ggplot(metadata,aes(x=UMAP_1,y=UMAP_2,color=celltype))+geom_point()+
  theme_void()+scale_color_manual(values = jdb_palette("corona"))

### Barcode visualization on UMAP
plot_clone_embedding(c('CCGTTGGCTTCAGTAAACTTTAGAAAGAG'),metadata,colors = c(rgb(200/255,200/255,200/255),
                                             rgb(239/255,153/255,81/255)),)

### clone analysis
cell_fate = metadata[,c('barcodes','celltype')]
cell_fate = cell_fate[!is.na(cell_fate$barcodes),]
barcode_freq = fate_mapping(cell_fate,show_row = F)

### cell type similarity
ct_similarity = cell_type_fate_similartiy(cell_fate)

### clone fate bias
bias_list = list()
for (i in unique(cell_fate[,2])) {
  bias_list[[i]] =clone_fate_bias(cell_fate,i)
}



bias_list2 <- purrr::map(unique(cell_fate[,2]),function(i){
    clone_fate_bias(cell_fate,i)
}) |> setNames(unique(cell_fate[,2]))

test <- readRDS('clone_fate_bias/Pei_2020_CSC_TPE13.rds')

test1 <- as_tibble(purrr::reduce(test,bind_rows))
values = colors)
  return(p1)
}
#split into 2 functions
#for shiny plot, run get_clone_embedding2 first to get all data used for plot, 
#then shiny using plot_clone_embedding2 to plot, which will be more fast

system.time({
  p1 <- plot_clone_embedding(c('394'),obj,colors = c(rgb(200/255,200/255,200/255),rgb(239/255,153/255,81/255)))
  plot(p1)
})
system.time({
  p1 <- plot_clone_embedding2(get_clone_embedding2(c('394'),obj))
  plot(p1)
})
#more faster




##




### clone analysis
cell_fate = obj@meta.data[,c('barcodes','celltype')]
barcode_freq = fate_mapping(cell_fate)





### cell type similarity
ct_similarity = cell_type_fate_similartiy(cell_fate)

### clone fate bias
system.time({
  bias_list = list()
  for (i in unique(cell_fate[,2])) {
    bias_list[[i]] = clone_fate_bias(cell_fate,i)
  }
}
)

system.time(
  bias_list2 <- purrr::map(unique(cell_fate[,2]),function(i){
    clone_fate_bias(cell_fate,i)
  }) |> setNames(unique(cell_fate[,2]))
)





