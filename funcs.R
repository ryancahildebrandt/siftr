#!/usr/bin/env Rscript
# -*- coding: utf-8 -*- 
# Created on Sun Oct 30 02:51:50 PM EDT 2022 
# author: Ryan Hildebrandt, github.com/ryancahildebrandt

# imports ----
{
	
	
	library(tidyverse)
	load("./data/ef_list.RData")
}

# emb funcs ----
word_sif <- function(word,  weight_param = 1e-3) {
	if (!(word %in% names(ef_list))) {
		word <- "_UNK_"
	}
	word_emb <- unlist(ef_list[[word]]$emb[[1]])
	word_freq <- ef_list[[word]]$freq
	word_weight <- weight_param / (weight_param + word_freq)
	out <- word_weight * word_emb
	return(out)
}

sent_sif <- function(sentence) {
	sent <- sentence %>% 
		tolower(.) %>% 
		str_replace_all(., "[[:punct:]]", "") %>% 
		str_split(., " ") %>% 
		unlist(.)
	sent_mat <- sapply(sent, word_sif)
	sent_sum <- apply(sent_mat, 1, sum)
	out <- sent_sum / length(sent)
	return(out)
}

# model funcs ----
train_rf <- function(in_df) {
	mdl <- train(
		form = user ~ .,
		data = in_df %>% dplyr::select(., -text, -model, -emb, -keep, -sift) %>% filter(., !is.na(user)),
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
	out <- in_df %>%
		mutate(., 
			   keep = predict(rf$mdl, newdata = in_df[, 5:ncol(in_df)], type = "prob")[, 1],
			   sift = predict(rf$mdl, newdata = in_df[, 5:ncol(in_df)], type = "prob")[, 2], 
			   model = predict(rf$mdl, newdata = in_df[, 5:ncol(in_df)])
		) %>%
		arrange(., keep)
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
	out <- in_df %>% 
		mutate(.,
			   user = NA,
			   model = NA,
			   keep = NA, 
			   sift = NA, 
			   emb = apply(.["text"], 1, function(x) list(sent_sif(x)))
			   ) %>%
		cbind(., matrix(unlist(.$emb), ncol = 100))
	return(out)
}

