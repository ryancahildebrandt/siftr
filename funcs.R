#!/usr/bin/env Rscript
# -*- coding: utf-8 -*- 
# Created on Sun Oct 30 02:51:50 PM EDT 2022 
# author: Ryan Hildebrandt, github.com/ryancahildebrandt

# imports ----
{
	
	load("./app_data/ef_list.RData")
}

# emb funcs ----
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

# model funcs ----
train_rf <- function(in_df) {
	in_df <- in_df[!is.na(in_df$user), !colnames(in_df) %in% c("text", "model", "emb", "keep", "sift")]
	mdl <- train(
		form = user ~ .,
		data = in_df,
		method = "rf"
	)
	out <- list(
		"df" = in_df,
		"mdl" = mdl,
		"acc" = mdl$results,
		"conf" = mdl$finalModel$confusion,
		"pred" = mdl$finalModel$predicted,
		"miss" = in_df[!is.na(in_df$user) & in_df$user != in_df$model, c(1:5)]
	)
	return(out)
}

pred_rf <- function(in_df, rf) {
	in_df$keep <- predict(rf$mdl, newdata = in_df[, 5:ncol(in_df)], type = "prob")[, 1]
	in_df$sift <- predict(rf$mdl, newdata = in_df[, 5:ncol(in_df)], type = "prob")[, 2]
	in_df$model <- predict(rf$mdl, newdata = in_df[, 5:ncol(in_df)])
	out <- in_df[order(in_df$keep),]
	return(out)
}

# in/out funcs ----
update_x <- function(in_df, inds) {
	in_df$user[inds][in_df$model[inds] == "sift"] <- "keep"
	in_df$user[inds][in_df$model[inds] == "keep"] <- "sift"
	return(in_df)
}

initial_x <- function(in_df, inds, label) {
	in_df$user[inds] <- label
	return(in_df)
}

render_rt <- function(in_df) {
	out <- renderReactable(
		reactable(
			in_df[, c(1:5)],
			filterable = TRUE,
			searchable = TRUE,
			selection = "multiple",
			showPageSizeOptions = TRUE,
			defaultPageSize = 25,
			onClick = "select",
			theme = slate(),
			columns = list(
				.selection = colDef(
					name = "feedback",
					width = 80,
					sticky = "left",
					style = list(cursor = "pointer"),
					headerStyle = list(cursor = "pointer")
				))))
	return(out)
}

prep_df <- function(in_df) {
	in_df$user <- NA
	in_df$model <- NA
	in_df$keep <- NA
	in_df$sift <- NA
	in_df$emb <- apply(in_df["text"], 1, function(x) list(sent_sif(x)))
	out <- cbind(in_df, matrix(unlist(in_df$emb), ncol = 100))
	return(out)
}

