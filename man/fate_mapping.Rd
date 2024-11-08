% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineage_hierarchy.R
\name{fate_mapping}
\alias{fate_mapping}
\title{fate mapping heatmap}
\usage{
fate_mapping(
  data,
  idx = "celltype",
  order_use = NULL,
  show_row = T,
  cluster_rows = F,
  cluster_cols = T,
  ...
)
}
\arguments{
\item{data}{data.frame, indicating lineage tracing data, first column should
be lineage tracing barcodes, second column should be related cell type}

\item{idx}{column name of cell type}

\item{order_use}{order cell type}

\item{cluster_cols}{boolean values determining if cols should be clustered}

\item{...}{other pheatmap parameters}

\item{show_rownames}{vector,indicating the order of cell types in the heatmap.}

\item{col}{heatmap colors}

\item{cluster_row}{boolean values determining if rows should be clustered}
}
\value{
heatmap
}
\description{
plot heatmap for lineage tracing barcodes in each cell type
}
\examples{
fate_mapping(fate_test)
}
