#!/usr/bin/env Rscript
# -*- coding: utf-8 -*- 
# Created on Thu Oct 27 06:40:26 PM EDT 2022 
# author: Ryan Hildebrandt, github.com/ryancahildebrandt

# imports ----
{
	
	
	library(tidyverse)
}

# get files ----
options(timeout = 3000)
download.file(
	"http://wikipedia2vec.s3.amazonaws.com/models/en/2018-04-20/enwiki_20180420_win10_100d.txt.bz2",
	"./data/enwiki_20180420_win10_100d.txt.bz2"
	)
unzip(
	zipfile = "./data/enwiki_20180420_win10_100d.txt.bz2",
	exdir = "./data/"
	)

download.file(
	"https://raw.githubusercontent.com/IlyaSemenov/wikipedia-word-frequency/master/results/enwiki-2022-08-29.txt",
	"./data/enwiki-2022-08-29.txt"
	)

# embs/freqs merge ----
ef_df <- merge(
	x = read_delim("./data/enwiki_20180420_win10_100d.txt.bz2", col_names = FALSE, skip = 1),
	y = read_delim("./data/enwiki-2022-08-29.txt", col_names = c("X1","freq")),
	by.x = "X1",
	by.y = "X1") %>%
	rename(., word = X1) %>% 
	mutate(., emb = apply(.[, 2:101], 1, function(i) list(as.vector(i, "numeric")))) %>% 
	dplyr::select(., -c(X2:X101)) %>% 
	add_row(word = "_UNK_", emb = list(rep(0, 100)), freq = 0)

ef_list <- split(ef_df, ef_df$word)
ef_list <- ef_list[1:300000]

# save ----
#save(ef_df, file = "./data/ef_df.RData")
save(ef_list, file = "./data/ef_list.RData")
