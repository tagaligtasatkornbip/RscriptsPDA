library(quanteda)
library(tidyr)
library(dplyr)
library(tidytext)
library(tidyverse)

data <- read.csv("Shared Cleaned Corpus.csv", header = TRUE) |> 
  select(case, text_clean)

terms <- read.csv("TERMS.csv", header = TRUE)

comp_terms <- terms$terms |> 
  str_replace_all("_", " ") |> 
  str_squish()

toks_comm <- tokens(data$text_clean, remove_punct = FALSE) |> 
  tokens_compound(pattern = as.phrase(tokens(comp_terms, remove_punct = TRUE)),
                  concatenator = "_")
comp_terms <- comp_terms |> 
  str_replace_all(" ", "_")

kwic(toks_comm, pattern = comp_terms,
     window = 7, valuetype = "fixed") %>%
  as.data.frame() %>%
  mutate(case = data$case[match(docname, names(toks_comm))]) %>%
  select(case, pre, keyword, post) %>%
  write_csv("KWIC.csv")
