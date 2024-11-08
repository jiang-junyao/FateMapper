% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineage_hierarchy.R
\name{lineage_tree}
\alias{lineage_tree}
\title{Build lineage tree based on Neighbor Joining}
\usage{
lineage_tree(data, idx = "celltype", method = "spearman", ...)
}
\arguments{
\item{data}{data.frame, indicating lineage tracing data, first column should
be lineage tracing barcodes, second column should be related cell type}

\item{idx}{column name of cell type}

\item{method}{character, indicating which correlation coefficient
(or covariance) is to be computed. One of "pearson", "kendall",
or "spearman (default)": can be abbreviated.}

\item{...}{parameter of plot function}
}
\description{
Build lineage tree based on Neighbor Joining
}
