
#' fate mapping heatmap
#' @description plot heatmap for lineage tracing barcodes in each cell type
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param idx column name of cell type
#' @param order_use order cell type
#' @param show_rownames vector,indicating the order of cell types in the heatmap.
#' @param col heatmap colors
#' @param cluster_row boolean values determining if rows should be clustered
#' @param cluster_cols boolean values determining if cols should be clustered
#' @param ... other pheatmap parameters
#' @importFrom pheatmap pheatmap
#' @return
#' heatmap
#' @export
#'
#' @examples fate_mapping(fate_test)
fate_mapping <- function(data,idx='celltype',order_use=NULL,show_row=T,
                         cluster_rows=F,cluster_cols=T,...){
  data = data[!is.na(data[,1]),]
  lineage_use = unique(data[,idx])
  freq_list <- purrr::map(unique(data$barcodes),function(i){
    data_use = data[data$barcodes==i,]
    freq = as.data.frame(table(data_use$celltype))
    freq$Var1 = as.character(freq$Var1)
    freq = freq[freq$Freq!=0,]
    lineage_absent = lineage_use[!lineage_use %in% as.character(freq$Var1)]
    lineage_absent_df =data.frame(lineage_absent,rep(0,length(lineage_absent)))
    colnames(lineage_absent_df) = colnames(freq)
    freq = rbind(freq,lineage_absent_df)
    rownames(freq) = freq$Var1
    freq = freq[lineage_use,]
    return(as.data.frame(t(data.frame(freq$Freq))))
  })

  freq_df = do.call(dplyr::bind_rows,freq_list)
  rownames(freq_df) = unique(data$barcodes)
  colnames(freq_df) = lineage_use

  col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))
  if (!is.null(order_use)) {
    freq_df = freq_df[,order_use]
  }
  freq_df_ratio = apply(freq_df, 1, function(x){
    row_sum = sum(x)
    print(length(x))
    return(x/row_sum)
  })
  pheatmap::pheatmap(t(freq_df_ratio),show_rownames =show_row,color = col
                     ,cluster_rows = cluster_rows,cluster_cols = cluster_cols,
                     border_color = NA,...)

  return(freq_df)
}

#' Column plot for Composition of cell types of each barcode
#' @description make column plot to show Composition of cell types of each barcode
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param idx column name of cell type
#' @param barcode_use Vector of barcodes to plot
#'
#' @return
#' ggplot column plot
#' @export
#'
#' @examples fate_ct_col(fate_test)
fate_ct_col <- function(data,idx='cell_type',
                        barcode_use = NULL){

  lineage_use = unique(data[,idx])
  freq_list = list()

  for (i in unique(data[,1])) {
    data_use = data[data[,1]==i,]
    freq = as.data.frame(table(data_use[,idx]))
    freq$Var1 = as.character(freq$Var1)
    freq = freq[freq$Freq!=0,]
    lineage_absent = lineage_use[!lineage_use %in% as.character(freq$Var1)]
    lineage_absent_df =data.frame(lineage_absent,rep(0,length(lineage_absent)))
    colnames(lineage_absent_df) = colnames(freq)
    freq = rbind(freq,lineage_absent_df)
    rownames(freq) = freq$Var1
    freq = freq[lineage_use,]
    freq_list[[i]] = as.data.frame(t(data.frame(freq$Freq)))
  }

  freq_df = do.call(dplyr::bind_rows,freq_list)
  rownames(freq_df) = names(freq_list)
  colnames(freq_df) = lineage_use

  col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))
  freq_df$freq_sum = rowSums(freq_df)
  freq_df = freq_df[order(freq_df$freq_sum,decreasing = T),]
  if (!is.null(barcode_use)) {
    freq_df_use = freq_df[barcode_use,]
  }else{
    freq_df_use = freq_df
  }
  bar_plot_input = reshape2::melt(as.matrix(freq_df_use)[,1:4])
  colnames(bar_plot_input) = c('barcode','sample','count')
  ggplot(bar_plot_input,aes(x=barcode,y=count,fill=sample))+geom_col()+
    theme_classic()+theme(text = element_text(size=15),axis.text.x =
                            element_text(angle = 45,size=12,hjust=1))
}

#' Calculate cell fate similarity among all cell types
#'
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param idx column name of cell type
#' @param method character, indicating which correlation coefficient
#' (or covariance) is to be computed. One of "pearson", "kendall",
#' or "spearman (default)": can be abbreviated.
#' @param plot whether plot the heatmap
#' @param ... other pheatmap parameters
#'
#' @return
#' barcodes correlation between cell type
#' @export
#'
#' @examples cell_type_fate_similartiy(fate_test)
cell_type_fate_similartiy <- function(data,idx='celltype',method='spearman',
                                   plot = TRUE,out_similar_mt = FALSE,...){
  col<- rev(colorRampPalette(c("#cc0000", "#FFff00",'#66ccff','#000066'))(50))
  lineage_use = unique(data[,idx])
  sample_similarity_list = list()
  for (i in lineage_use) {
    for (j in lineage_use) {
      sample1 = data[data[,idx]==i,]
      sample2 = data[data[,idx]==j,]

      fre_all = as.data.frame(table(sample1[,1]))
      fre_all_all = as.data.frame(table(fre_all[,2]))
      fre_all1 = as.data.frame(table(sample2[,1]))
      fre_all_all1 = as.data.frame(table(fre_all1[,2]))

      overlapped_idx = intersect(sample1[,1],sample2[,1])
      all_idx = union(sample1[,1],sample2[,1])
      df_plot = data.frame(all_idx,rep(0,length(all_idx)),rep(0,
                                                              length(all_idx)))
      df_plot[,2] = fre_all[match(df_plot$all_idx,fre_all[,1]),2]
      df_plot[,3] = fre_all1[match(df_plot$all_idx,fre_all1[,1]),2]
      colnames(df_plot)[2:3] = c(i,j)
      df_plot[is.na(df_plot)] = 0

      sample_similarity = cor(df_plot[,2],df_plot[,3],method = method)
      sample_similarity_list[[paste0(i,'-',j)]] = data.frame(i
                                                             ,j,sample_similarity)
    }
  }
  sample_similarity_df = do.call(dplyr::bind_rows,sample_similarity_list)
  sample_similarity_df = reshape2::dcast(sample_similarity_df,i~j)
  rownames(sample_similarity_df) = sample_similarity_df[,1]
  sample_similarity_df = sample_similarity_df[,-1]
  sample_similarity_df[is.na(sample_similarity_df)]=0
  if (plot) {
    pheatmap::pheatmap(sample_similarity_df,color = col,...)
  }
  if (out_similar_mt) {
    return(sample_similarity_df)
  }
}



#' Calculate clone fate bias for specific cell type
#'
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param fate_use character, targeted fate cell type. if fate_use == '', this
#' function will automatically select clone dominant cell type.
#'
#' @return
#' @export
#'
#' @examples clone_fate_bias(fate_test)
clone_fate_bias <- function(data,fate_use = ''){
  data = data[!is.na(data[,1]),]
  all_clone_size = nrow(data)
  list_result = list()

  for (i in unique(data[,1])) {

    ### select dominant fate
    if (fate_use == '') {
      freq = as.data.frame(table(data[data[,1]==i,]))
      freq = freq[order(freq[,2],decreasing = T),]
      fate_use = as.character(freq[1,2])
      print(fate_use)
    }
    ct_all_clone_size = nrow(data[data[,2]==fate_use,])
    data_clone = data[data[,1]==i,]
    clone_size = nrow(data_clone)
    clone_ct_size = nrow(data_clone[data_clone[,2]==fate_use,])
    fate_ratio = clone_ct_size/clone_size
    if (length(clone_ct_size)>0) {
      p_val = fisher.test(data.frame(c(clone_ct_size,
                                       clone_size-clone_ct_size),
                             c(ct_all_clone_size-clone_ct_size,
                               all_clone_size-(ct_all_clone_size-clone_ct_size))
                             )
                            )$p.value
      FDR = p.adjust(p_val, method = "fdr")
      list_result[[as.character(i)]] = c(i,fate_use,clone_size,
                                         fate_ratio,p_val,FDR)
    }
  }

  result_df = as.data.frame(t(as.data.frame(list_result)))
  colnames(result_df) = c('clone_name','fate_use','clone_size','fate_ratio',
                          'pvalue','fdr')
  result_df = result_df[order(result_df[,5]),]
  result_df = result_df[result_df[,4]>0,]
  return(result_df)
}

#' Build lineage tree based on Neighbor Joining
#'
#' @param data data.frame, indicating lineage tracing data, first column should
#' be lineage tracing barcodes, second column should be related cell type
#' @param idx column name of cell type
#' @param method character, indicating which correlation coefficient
#' (or covariance) is to be computed. One of "pearson", "kendall",
#' or "spearman (default)": can be abbreviated.
#' @param ... parameter of plot function
#' @importFrom ape nj
#' @return
#' @export
#'
#' @examples
lineage_tree <- function(data,idx='celltype',method='spearman',...){
  ct_similarity = cell_type_fate_similartiy(data,out_similar_mt = T,plot = F)
  dist_mt = as.dist(ct_similarity)
  nj.tree <- ape::nj(dist_mt)
  plot(nj.tree, ...)
}
