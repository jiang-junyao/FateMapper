plot_barcode_distance <- function(){

}

plot_barcode_logo <- function(){

}

plot_barcode_venn <- function(){

}

plot_barcode_frequency <- function(){

}

#' Calculate the spearman correlation of select feature between each sample to
#' estimate consistency of technical replicates
#'
#' @param df_sample data.frame, indicating lineage tracing data, first column should
#' be sample, second column should be interested feature
#'
#' @return
#' @export
#'
#' @examples
sample_repeats <- function(df_sample){
  library(ggplot2)
  library(patchwork)
  sample = levels(as.factor(df_sample[,1]))
  colnames(df_sample) = c('sample','reads_number')
  p1=ggplot(df_sample,aes(x=sample,y=reads_number))+geom_col()+xlab('')+
    ylab('')+theme(axis.ticks.y = element_blank(),
                   axis.text.y = element_blank())
  for (i in sample) {
    for (j in sample) {
      if (i!=j) {
        sample1 = df_sample[df_sample[,1]==i,]
        sample2 = df_sample[df_sample[,1]==j,]
        fre_all = as.data.frame(table(sample1[,2]))
        fre_all_all = as.data.frame(table(fre_all[,2]))
        fre_all1 = as.data.frame(table(sample2[,2]))
        fre_all_all1 = as.data.frame(table(fre_all1[,2]))
        overlapped_idx = intersect(sample1[,2],sample2[,2])
        all_idx = union(sample1[,2],sample2[,2])
        df_plot = data.frame(all_idx,rep(0,length(all_idx)),rep(0,length(all_idx)))
        match(fre_all[,1],df_plot$all_idx)
        df_plot[,2] = fre_all[match(df_plot$all_idx,fre_all[,1]),2]
        df_plot[,3] = fre_all1[match(df_plot$all_idx,fre_all1[,1]),2]
        colnames(df_plot)[2:3] = c('sample1','sample2')
        df_plot[is.na(df_plot)]=0

        cor1=cor(df_plot[,2],df_plot[,3],method='spearman')
        cor1 = round(cor1,3)
        df_plot = df_plot[,2:3]
        df_plot[,1] = df_plot[,1]/sum(df_plot[,1])
        df_plot[,2] = df_plot[,2]/sum(df_plot[,2])
        p2=ggplot(df_plot,aes(x=sample1,y=sample2))+geom_point()+theme_classic()+
          geom_smooth(method = "lm", se = FALSE)+xlab('barcode ratio') +
          ylab('')+ggtitle(paste0(i,' ',j,' correlation:',cor1))+
          theme(plot.title = element_text(size = 8))
        p1=p1+p2

      }
    }
  }
  p1
}

#' Title
#'
#' @param mt clone matrix
#'
#' @return
#' @export
#'
#' @examples
sort_clone_mt <- function(mt){
  mt = as.matrix(mt)
  final_idx = c()
  left_idx = 1:nrow(mt)
  for (i in 1:ncol(mt)) {
    col_use = mt[as.numeric(left_idx),i]
    names(col_use) = left_idx
    sort_idx = sort(col_use,decreasing = T)
    final_idx = c(final_idx,names(sort_idx)[sort_idx!=0])
    left_idx = names(sort_idx)[sort_idx==0]
  }
  final_idx = c(final_idx,left_idx)
  return(as.numeric(final_idx))
}

