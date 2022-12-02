#!/usr/bin/env Rscript
# -*- coding: utf-8 -*- 
# Created on Thu Oct 27 06:40:26 PM EDT 2022 
# author: Ryan Hildebrandt, github.com/ryancahildebrandt

# imports ----
{
	
	library(readr)
}

# get files ----
options(timeout = 3000)
download.file(
	"http://wikipedia2vec.s3.amazonaws.com/models/en/2018-04-20/enwiki_20180420_win10_100d.txt.bz2",
	"./app_data/enwiki_20180420_win10_100d.txt.bz2"
	)
unzip(
	zipfile = "./app_data/enwiki_20180420_win10_100d.txt.bz2",
	exdir = "./app_data/"
	)

download.file(
	"https://raw.githubusercontent.com/IlyaSemenov/wikipedia-word-frequency/master/results/enwiki-2022-08-29.txt",
	"./app_data/enwiki-2022-08-29.txt"
	)

# embs/freqs merge ----
ef_df <- merge(
	x = read_delim("./app_data/enwiki_20180420_win10_100d.txt.bz2", col_names = FALSE, skip = 1),
	y = read_delim("./app_data/enwiki-2022-08-29.txt", col_names = c("X1","freq")),
	by.x = "X1",
	by.y = "X1") 
colnames(ef_df)[1] <- "word"
ef_df <- ef_df[order(-ef_df$freq),]
ef_df$emb <- apply(ef_df[, 2:101], 1, function(i) list(as.vector(i, "numeric")))
ef_df <- ef_df[, c(1, 102, 103)]
ef_df <- rbind(ef_df, list(word = "_UNK_", emb = list(rep(0, 100)), freq = 0))

ef_list <- split(ef_df, ef_df$word)
# uncomment following line to limit vocab
#ef_list <- ef_list[1:300000]

# save ----
#save(ef_df, file = "./app_data/ef_df.RData")
save(ef_list, file = "./app_data/ef_list.RData", compress = "bzip2")
