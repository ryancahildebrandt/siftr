% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ef_list}
\alias{ef_list}
\title{Embedding & Frequency List}
\format{
List of 1.5 million:
\describe{
\item{word}{target word}
\item{freq}{number of word occurrences in frequency corpus}
\item{emb}{pretrained word embedding vector, 100d}
}
\usage{
ef_list
}
\description{
Wordlist containing frequency and embedding information per word
}
\details{
Word frequencies: https://github.com/IlyaSemenov/wikipedia-word-frequency/tree/master/results, Word embeddings: https://wikipedia2vec.github.io/wikipedia2vec/pretrained/
}
\keyword{datasets}
