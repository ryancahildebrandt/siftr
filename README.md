# SIFtR

------------------------------------------------------------------------

[*Open*](https://gitpod.io/#https://github.com/ryancahildebrandt/siftr) *in gitpod*

## *Purpose*

The present project has 2 goals. First is an R implementation of the Smooth Inverse Frequency (SIF) algorithm for sentence embeddings, to fill a gap in sentence embedding techniques in R (that *aren't* running off python in the background). SIF is a relatively lightweight and remarkably accurate embedding approach, which in some tasks provides comparable performance to neural network based embedding models. Second is an application of this algorithm (via Shiny) to identify and *sift* out undesirable text data, based on user input fed into a random forest classifier. The app assumes desirability based on some semantic aspect of the data, and uses user provided examples of good and bad data to try and label the full dataset. The user can then provide feedback on the model predictions in order to refine it, with the intent that the user only needs to label a handful of datapoints to get a decent split between useful and non-useful data in an unlabeled dataset.

*As an aside, the overlap between the SIF algorithm, the idea of "sifting" data, and the convention of throwing an "r" on the end of R packages was a very happy accident.*

------------------------------------------------------------------------

## Dataset

The dataset used for the current project was pulled from the following:

-   [Word frequencies](https://github.com/IlyaSemenov/wikipedia-word-frequency/tree/master/results) for weighting word embeddings 

-   [Pretrained word embeddings](https://wikipedia2vec.github.io/wikipedia2vec/pretrained/) trained on Wikipedia articles, 100 dimensions to keep the implementation a bit smaller than the standard 300

-   [Stringr](https://stringr.tidyverse.org/articles/stringr.html) for the default text data loaded with the Shiny app, specifically the *fruits* and *sentences* datasets

## Implementation

The present SIF implementation is based on the [original](https://github.com/PrincetonML/SIF) algorithm as well as [this](https://www.kaggle.com/code/procode/sif-embeddings-got-69-accuracy/notebook) notebook, which provides a slightly simplified approach. The implementation from the original authors included principal component removal after sentence embedding calculation, which I forgo in this project for simplicity.

The following functions constitute the core of the SIF weighted sentence embedding calculation, which can be briefly summarized as **the average of a sentence's constituent word embeddings, with each word embedding multiplied by the inverse frequency for that word.**

``` r
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
```

The above code also assigns a vector of 0s for words not contained in the model vocabulary. Input text has all punctuation removed, word tokenization based on spaces, and lowercasing applied.

------------------------------------------------------------------------

## Outputs

- [SIFtR](https://rhildebrandt.shinyapps.io/siftr/) Shiny app. The vocabulary has been kept to a 300,000 out of the full 1.5m words, due to memory constraints on unpaid Shiny projects. If you need a larger vocabulary, the full 1.5m word embeddings load with 8GB of memory no problem.
- siftr R package, which includes the SIF implementation and associated embedding and frequency data. devtools::install_github("ryancahildebrandt/siftr")

