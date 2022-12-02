#' Calculates SIF weighted embedding for a single word. Not particularly useful on its own, as SIF assumes multiple word embeddings to average together
#'
#' @param word character: target word
#' @param efl list: embedding and frequency object, assumes each word-named list entry has freq and embedding
#' @param weight_param numeric: parameter used to tweak SIF calculation, default = .001
#'
#' @return numeric vector: weighted embedding vector for target word
#' @export
#'
#' @usage
#' word_sif(word = "chicken")
#' word_sif(word = "tender", efl = ef_list, weight_param = .004)
word_sif <- function(word, efl,  weight_param = 1e-3) {
	if (!(word %in% names(efl))) {
		word <- "_UNK_"
	}
	word_emb <- unlist(efl[[word]]$emb[[1]])
	word_freq <- efl[[word]]$freq
	word_weight <- weight_param / (weight_param + word_freq)
	out <- word_weight * word_emb
	return(out)
}