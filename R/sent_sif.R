#' Calculates SIF weighted embedding for given sentence. Uses word embeddings calculated via word_sif function
#'
#' @param sentence character: target sentence
#'
#' @return numeric vector: weighted and averaged embedding vector for target sentence
#' @export
#'
#' @usage
#' sent_sif(sentence = "I need a chicken tender")
sent_sif <- function(sentence) {
	sent <- tolower(sentence) 
	sent <- gsub("[[:punct:]]", "", sent) 
	sent <- strsplit(sent, " ")
	sent <- unlist(sent)
	
	sent_mat <- sapply(sent, word_sif, efl = ef_list)
	sent_sum <- apply(sent_mat, 1, sum)
	out <- sent_sum / length(sent)
	return(out)
}