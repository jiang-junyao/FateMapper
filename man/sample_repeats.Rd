% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc.R
\name{sample_repeats}
\alias{sample_repeats}
\title{Calculate the spearman correlation of select feature between each sample to
estimate consistency of technical replicates}
\usage{
sample_repeats(df_sample)
}
\arguments{
\item{df_sample}{data.frame, indicating lineage tracing data, first column should
be sample, second column should be interested feature}
}
\description{
Calculate the spearman correlation of select feature between each sample to
estimate consistency of technical replicates
}
