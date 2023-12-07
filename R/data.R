#' Embedding & Frequency List
#'
#' Wordlist containing frequency and embedding information per word
#'
#' @name ef_list
#' @docType data
#' @details Word frequencies: https://github.com/IlyaSemenov/wikipedia-word-frequency/tree/master/results, Word embeddings: https://wikipedia2vec.github.io/wikipedia2vec/pretrained/
#' @format List of 1.5 million:
#' \describe{
#'   \item{word}{target word}
#'   \item{freq}{number of word occurrences in frequency corpus}
#'   \item{emb}{pretrained word embedding vector, 100d}
#'
"ef_list"