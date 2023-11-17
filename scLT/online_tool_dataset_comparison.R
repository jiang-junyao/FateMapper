library(FateMapper)
library(ggplot2)
metdata_list = readRDS('obj_metadata_list.rds') ### modify this dir
dataset1_name = "Pei_2020_CSC_TPE13"
dataset2_name = "Pei_2020_CSC_TPE9"



dataset_cell_number_compare <- function(dataset1_name,dataset2_name,metdata_list){

  dataset1 = metdata_list[[dataset1_name]]
  dataset2 = metdata_list[[dataset2_name]]
  dataset1_ct_freq = as.data.frame(table(dataset1$celltype))
  dataset1_ct_freq$dataset = dataset1_name
  dataset2_ct_freq = as.data.frame(table(dataset2$celltype))
  dataset2_ct_freq$dataset = dataset2_name
  df_final = rbind(dataset1_ct_freq,dataset2_ct_freq)

  ggplot(df_final,aes(x=Var1,y=Freq,fill=dataset))+
    geom_col(stat='identity', position='dodge',width = 0.8)+theme_classic()+
    xlab('cell state')+ylab('cell number') +scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = c("#5E4FA2","#F88D51"))
}

dataset_fate_bias_number_compare <- function(dataset1_name,dataset2_name,metdata_list){

  data_dir = 'F:\\scLT-seq db\\out_result\\clone_fate_bias/' ### modify this dir
  dataset1 = readRDS(paste0(data_dir,dataset1_name,'.rds'))
  dataset2 = readRDS(paste0(data_dir,dataset2_name,'.rds'))

  dataset1_sig_clone = c()
  dataset1_insig_clone = c()
  dataset2_sig_clone = c()
  dataset2_insig_clone = c()

  for (i in dataset1) {
    dataset1_sig_clone = c(dataset1_sig_clone,nrow(i[i[,6]<=0.05,]))
    dataset1_insig_clone = c(dataset1_insig_clone,nrow(i[i[,6]>0.05,]))
  }
  for (i in dataset2) {
    dataset2_sig_clone = c(dataset2_sig_clone,nrow(i[i[,6]<=0.05,]))
    dataset2_insig_clone = c(dataset2_insig_clone,nrow(i[i[,6]>0.05,]))
  }

  dataset1_fate_bias_freq = data.frame('celltype'=names(dataset1),
                                       'dataset1_sig_clone' = dataset1_sig_clone,
                                       'dataset1_insig_clone' = dataset1_insig_clone,
                                       'dataset'=dataset1_name)
  dataset2_fate_bias_freq = data.frame('celltype'=names(dataset2),
                                       'dataset1_sig_clone' = dataset2_sig_clone,
                                       'dataset1_insig_clone' = dataset2_insig_clone,
                                       'dataset'=dataset2_name)
  df_final = rbind(dataset1_fate_bias_freq,dataset2_fate_bias_freq)
  ggplot(df_final,aes(x=celltype,y=dataset1_sig_clone,fill=dataset))+
    geom_col(stat='identity', position='dodge',width = 0.8)+theme_classic()+
    xlab('cell state')+ylab('number of clone with biased fate') +scale_y_continuous(expand = c(0,0))+
    scale_fill_manual(values = c("#5E4FA2","#F88D51"))
}

dataset_fate_bias_number_compare(dataset1_name,dataset2_name,metdata_list)
