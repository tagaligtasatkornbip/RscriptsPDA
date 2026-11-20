library(quanteda)
library(tidytext)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

if (!exists("toks_ng") || !exists("doc_signature")) source("Frequency Analysis.r")

TOP_SENT <- 10
sent_exclude <- c("mar", "grace", "supporter", "supporters", "revolutionary")

font <- "Times New Roman"
if (.Platform$OS.type == "windows") {
  windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
}
dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else
  function(filename, ...) grDevices::png(filename, ..., type = if (.Platform$OS.type == "windows") "windows" else "cairo")

case_labels <- c(TOBOSO19 = "Toboso 19", SAF44 = "SAF 44")

cap_words <- c(npa = "NPA", afp = "AFP", up_univ = "UP", saf = "SAF", pnp = "PNP", milf = "MILF",
               abs = "ABS", cbn = "CBN", dzmm = "DZMM", abscbn = "ABS-CBN", dds = "DDS", lgu = "LGU", bbm = "BBM", ndf = "NDF", cpp = "CPP",
               noynoy = "Noynoy", aquino = "Aquino", duterte = "Duterte", marcos = "Marcos", leni = "Leni",
               roxas = "Roxas", ninoy = "Ninoy", marwan = "Marwan", quirino = "Quirino", inopacan = "Inopacan", leyte = "Leyte", cory = "Cory", mar = "Mar",
               god = "God", jesus = "Jesus", christ = "Christ", lord = "Lord",
               mary = "Mary", piattos = "Piattos", philippine = "Philippine", philippines = "Philippines",
               pilipinas = "Pilipinas", filipino = "Filipino", filipinos = "Filipinos", negros = "Negros",
               toboso = "Toboso", occidental = "Occidental", makabayan = "Makabayan", manila = "Manila",
               marawi = "Marawi", mendiola = "Mendiola", hacienda = "Hacienda", luisita = "Luisita",
               mamasapano = "Mamasapano", mindanao = "Mindanao", china = "China", chinese = "Chinese",
               liberal = "Liberal", diliman = "Diliman", covid = "COVID")

cap_label <- function(x) {
  vapply(str_split(x, " "), function(w) {
    hit <- w %in% names(cap_words)
    w[hit] <- cap_words[w[hit]]
    paste(w, collapse = " ")
  }, character(1))
}

toks_sent <- toks %>%
  tokens_remove(stop_all, valuetype = "fixed", padding = TRUE) %>%
  tokens_select("*", min_nchar = 3, padding = TRUE)

words <- dfm(toks_sent, remove_padding = TRUE) %>%
  convert(to = "tripletlist") %>%
  as_tibble() %>%
  transmute(doc_id = as.character(document),
            word   = as.character(feature),
            n      = frequency,
            case   = str_remove(doc_id, "_\\d+$"))

bing <- get_sentiments("bing") %>%
  filter(!word %in% sent_exclude)

sentiment_words <- words %>%
  inner_join(bing, by = "word")

sentiment_counts <- sentiment_words %>%
  count(case, word, sentiment, wt = n, name = "n", sort = TRUE)
write_csv(sentiment_counts, "output/Sentiment Word Counts.csv")

sentiment_summary <- sentiment_words %>%
  group_by(case, sentiment) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  complete(case = names(case_labels), sentiment = c("positive", "negative"),
           fill = list(total = 0)) %>%
  group_by(case) %>%
  mutate(percentage = round(100 * total / sum(total), 2)) %>%
  ungroup() %>%
  mutate(Case = case_labels[case])
write_csv(sentiment_summary, "output/Sentiment Summary.csv")

sent_table <- xtabs(total ~ case + sentiment, data = sentiment_summary)
sent_test  <- chisq.test(sent_table)

comment_sentiment <- sentiment_words %>%
  group_by(case, doc_id, sentiment) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = sentiment, values_from = total, values_fill = 0)

comment_sentiment <- clean %>%
  select(doc_id, case, Comment) %>%
  left_join(comment_sentiment, by = c("doc_id", "case")) %>%
  mutate(positive = coalesce(positive, 0),
         negative = coalesce(negative, 0),
         sentiment_score = positive - negative,
         sentiment_category = case_when(
           positive == 0 & negative == 0 ~ "no match",
           sentiment_score > 0 ~ "positive",
           sentiment_score < 0 ~ "negative",
           TRUE ~ "mixed"))
write_csv(comment_sentiment, "output/Sentiment per Comment.csv")

coverage <- comment_sentiment %>%
  group_by(case) %>%
  summarise(comments = n(),
            matched  = sum(sentiment_category != "no match"),
            coverage = round(100 * matched / comments, 2),
            .groups = "drop")
write_csv(coverage, "output/Sentiment Coverage.csv")

category_share <- comment_sentiment %>%
  count(case, sentiment_category) %>%
  group_by(case) %>%
  mutate(percentage = round(100 * n / sum(n), 2)) %>%
  ungroup()
write_csv(category_share, "output/Sentiment Categories.csv")

sent_cols <- list(
  "Toboso 19" = c(Negative = "#7a2a20", Positive = "#dc9286"),
  "SAF 44"    = c(Negative = "#1d3d5c", Positive = "#86abd0")
)

sent_theme <- theme_minimal(base_size = 12, base_family = font) +
  theme(text = element_text(family = font, colour = "black"),
        axis.text = element_text(colour = "black"),
        strip.text = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey88", linewidth = 0.3),
        panel.spacing.x = unit(2, "lines"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background  = element_rect(fill = "white", colour = NA),
        plot.margin = margin(15, 15, 15, 15))

bing_val <- setNames(ifelse(bing$sentiment == "positive", 1, -1), bing$word)
negators <- c("not", "no", "never", "without", "hindi", "wala", "walang")

score_ngram <- function(x) {
  w <- str_split(x, " ")[[1]]
  v <- bing_val[w]
  v[is.na(v)] <- 0
  s <- sum(v)
  if (w[1] %in% negators && s != 0) s <- -s
  s
}

ngram_sentiment <- function(n) {
  d <- dfm(tokens_ngrams(toks_ng, n = n, concatenator = " "))
  parts <- str_split(featnames(d), " ")
  keep <- vapply(parts, function(w) !any(w %in% stop_all) && all(nchar(w) >= 3) &&
                   !anyDuplicated(w) && !tail(w, 1) %in% cut_end && !w[1] %in% cut_start,
                 logical(1)) & !featnames(d) %in% drop_ngrams
  d <- d[, keep]
  d <- d[, !duplicated(doc_signature(d))]
  trip <- convert(d, to = "tripletlist")
  counts <- tibble(case  = str_remove(as.character(trip$document), "_\\d+$"),
                   ngram = as.character(trip$feature),
                   n     = trip$frequency) %>%
    count(case, ngram, wt = n, name = "n")
  feats <- unique(counts$ngram)
  scores <- tibble(ngram = feats, score = vapply(feats, score_ngram, numeric(1)))
  counts %>%
    inner_join(filter(scores, score != 0), by = "ngram") %>%
    mutate(key = vapply(str_split(ngram, " "), function(w) paste(sort(sub("s$", "", w)), collapse = " "), character(1))) %>%
    group_by(case, key, score) %>%
    summarise(ngram = ngram[which.max(n)], n = sum(n), .groups = "drop") %>%
    select(-key) %>%
    mutate(sentiment = ifelse(score > 0, "positive", "negative"))
}

ngram_sentiment_all <- bind_rows(
  ngram_sentiment(2) %>% mutate(type = "Bigrams"),
  ngram_sentiment(3) %>% mutate(type = "Trigrams")
)
write_csv(ngram_sentiment_all, "output/Sentiment Ngram Counts.csv")

ngram_summary <- ngram_sentiment_all %>%
  group_by(case, type, sentiment) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  group_by(case, type) %>%
  mutate(percentage = round(100 * total / sum(total), 2)) %>%
  ungroup()
write_csv(ngram_summary, "output/Sentiment Ngram Summary.csv")

sent_all <- bind_rows(
  sentiment_counts %>% transmute(case, term = word, sentiment, n, type = "Words"),
  ngram_sentiment_all %>% transmute(case, term = ngram, sentiment, n, type)
)

top_terms <- function(cs, types) {
  d <- sent_all %>%
    filter(case == cs, type %in% types) %>%
    group_by(type, sentiment) %>%
    slice_max(n, n = TOP_SENT, with_ties = FALSE) %>%
    ungroup()
  d <- bind_rows(
    d %>% filter(sentiment == "negative") %>% arrange(type, desc(n), term),
    d %>% filter(sentiment == "positive") %>% arrange(type, desc(n), desc(term))
  ) %>%
    group_by(type, sentiment) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    mutate(Sentiment = factor(str_to_title(sentiment), levels = c("Negative", "Positive")),
           type = factor(type, levels = c("Words", "Bigrams", "Trigrams")),
           id = paste(term, type, sentiment, sep = "___"))
  d %>% mutate(id = factor(id, levels = rev(d$id)))
}

sent_plot <- function(d, lab) {
  ggplot(d, aes(x = n, y = id, fill = Sentiment)) +
    geom_col(width = 0.75) +
    scale_y_discrete(labels = function(x) cap_label(sub("___.*$", "", x))) +
    scale_fill_manual(values = sent_cols[[lab]], name = "Legend:") +
    labs(x = NULL, y = NULL) +
    sent_theme
}

for (cs in names(case_labels)) {
  lab <- case_labels[[cs]]

  p_words <- sent_plot(top_terms(cs, "Words"), lab) +
    facet_wrap(~ Sentiment, nrow = 1, scales = "free_y")
  ggsave(paste0("output/Sentiment Words - ", lab, ".png"), p_words, width = 10, height = 6,
         dpi = 300, bg = "white", device = dev)

  p_ngrams <- sent_plot(top_terms(cs, c("Bigrams", "Trigrams")), lab) +
    facet_wrap(~ type + Sentiment, nrow = 2, scales = "free",
               labeller = labeller(.multi_line = FALSE))
  ggsave(paste0("output/Sentiment Ngrams - ", lab, ".png"), p_ngrams, width = 12, height = 9,
         dpi = 300, bg = "white", device = dev)
}

print(coverage)
print(sentiment_summary, n = Inf)
print(sent_test)
print(category_share, n = Inf)
print(sentiment_counts %>% group_by(case, sentiment) %>% slice_max(n, n = TOP_SENT, with_ties = FALSE), n = Inf)
print(ngram_summary, n = Inf)
