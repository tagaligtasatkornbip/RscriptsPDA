library(quanteda)
library(quanteda.textstats)
library(stopwords)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

dir.create("output", showWarnings = FALSE)
top_n <- 20

raw <- bind_rows(
  read_csv("Toboso 19 Corpus.csv", show_col_types = FALSE) %>% mutate(case = "TOBOSO19"),
  read_csv("SAF 44 Corpus.csv",    show_col_types = FALSE) %>% mutate(case = "SAF44")
) %>%
  mutate(commenter_id = paste0("Commenter_", match(Username, unique(Username)))) %>%
  select(-Username)

preprocess <- function(x) {
  x %>%
    str_replace_all("https?://\\S+|www\\.\\S+", " ") %>%
    str_replace_all("@\\S+", " ") %>%
    str_replace_all("[\u2019\u2018`]", "'") %>%
    str_replace_all("\\bU\\.?P\\b\\.?", " up_univ ") %>%
    str_to_lower() %>%
    str_replace_all("\\bn\\.p\\.a\\.?", " npa ") %>%
    str_replace_all("\\b(a?ha(ha)+h?|he(he)+|hi(hi)+|lo+l+|lmao+|wa?h(aha)+)\\b", " haha ") %>%
    str_remove_all("'")
}

clean <- raw %>%
  filter(!is.na(Comment), str_trim(Comment) != "") %>%
  group_by(case) %>%
  mutate(text_clean = preprocess(Comment),
         doc_id     = paste0(case, "_", row_number())) %>%
  ungroup()

print(count(clean, case))

clean %>%
  select(doc_id, case, commenter_id, VideoID, Date, Comment, text_clean) %>%
  write_csv("output/Shared Cleaned Corpus.csv")

norm <- read_csv("NormalizedDictionary.csv", show_col_types = FALSE) %>%
  mutate(variant = str_squish(str_to_lower(variant)), standard = str_squish(standard),
         n_words = str_count(variant, " ") + 1)
norm <- norm[!duplicated(norm$variant, fromLast = TRUE), ]

toks <- corpus(clean, docid_field = "doc_id", text_field = "text_clean") %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, split_hyphens = TRUE)

for (k in sort(unique(norm$n_words), decreasing = TRUE)) {
  nk <- norm[norm$n_words == k, ]
  toks <- tokens_replace(toks, phrase(nk$variant), phrase(nk$standard), valuetype = "fixed")
}

en_stop     <- stopwords("en", source = "snowball")
custom_stop <- str_trim(read_lines("Stopwords.txt"))
custom_stop <- custom_stop[custom_stop != "" & !str_starts(custom_stop, "#")]

keep_words <- c("walang")

stop_all <- setdiff(c(en_stop, str_remove_all(en_stop, "'"),
                      stopwords("tl", source = "stopwords-iso"),
                      custom_stop),
                    keep_words)

unigram_stop <- c("never", "forget", "good", "job", "human", "rights", "walang", "kwenta")

toks_ns <- toks %>%
  tokens_remove(c(stop_all, unigram_stop), valuetype = "fixed", padding = TRUE) %>%
  tokens_select("*", min_nchar = 3, padding = TRUE)

dfm(toks) %>%
  dfm_keep(stop_all, valuetype = "fixed") %>%
  textstat_frequency() %>%
  select(feature, frequency, docfreq) %>%
  write_csv("output/Removed Noise Words.csv")

toks_kwic <- toks

dfmat <- dfm(toks_ns, remove_padding = TRUE)
cat("DFM:", ndoc(dfmat), "comments x", nfeat(dfmat), "terms\n")

freq <- textstat_frequency(dfmat, n = top_n, groups = case)
write_csv(freq, "output/Top Words in Frequency.csv")

tfidf_dfm <- dfmat %>%
  dfm_tfidf(scheme_tf = "prop", base = exp(1))

trip <- convert(tfidf_dfm, to = "tripletlist")
tfidf_comments <- tibble(doc_id = as.character(trip$document),
                         term   = as.character(trip$feature),
                         tf_idf = trip$frequency) %>%
  mutate(case = str_remove(doc_id, "_\\d+$"))
write_csv(tfidf_comments, "output/TFIDF per Comment.csv")

tfidf_by_case <- tfidf_comments %>%
  group_by(case, term) %>%
  summarise(tfidf_sum = sum(tf_idf), n_comments = n(), .groups = "drop") %>%
  group_by(case) %>%
  slice_max(tfidf_sum, n = top_n, with_ties = FALSE) %>%
  mutate(rank = row_number(), .before = term) %>%
  ungroup()
write_csv(tfidf_by_case, "output/Top Words TFIDF.csv")

key <- textstat_keyness(dfm_group(dfmat, groups = case), target = "TOBOSO19", measure = "chi2")
write_csv(key, "output/Keyness.csv")

plot_top <- function(case_name, label, colour) {
  freq %>%
    filter(group == case_name) %>%
    ggplot(aes(x = frequency, y = reorder(feature, frequency))) +
    geom_col(fill = colour) +
    labs(title = paste("Top", top_n, "terms:", label), x = "Frequency", y = NULL) +
    theme_minimal(base_size = 12)
}

ggsave("output/Toboso 19.png", plot_top("TOBOSO19", "Toboso 19", "#b5473a"),
       width = 7, height = 7, dpi = 300)
ggsave("output/SAF 44.png", plot_top("SAF44", "SAF 44", "#2f5d8a"),
       width = 7, height = 7, dpi = 300)

kwic(toks_kwic, pattern = c(head(key$feature, top_n), tail(key$feature, top_n)),
     window = 7, valuetype = "fixed") %>%
  as.data.frame() %>%
  mutate(case = str_remove(docname, "_\\d+$")) %>%
  select(case, docname, keyword, pre, post) %>%
  write_csv("output/KWIC.csv")

print(as_tibble(select(freq, group, rank, feature, frequency)), n = Inf)
print(tfidf_by_case, n = Inf)
print(head(key, top_n))
print(tail(key, top_n))

TOP <- 20
case_labels <- c(TOBOSO19 = "Toboso 19", SAF44 = "SAF 44")
ngram_names <- c("Unigrams", "Bigrams", "Trigrams")
ngram_cols <- list(
  "Toboso 19" = c(Unigrams = "#7a2a20", Bigrams = "#b5473a", Trigrams = "#dc9286"),
  "SAF 44"    = c(Unigrams = "#1d3d5c", Bigrams = "#2f5d8a", Trigrams = "#86abd0")
)

drop_ngrams <- c("janth claire flores", "johanna roxas", "job afp", "job afp ubusin",
                 "job philippine army", "good job philippine", "job phil army", "good job phil", "salamat mary grace", "ren up_univ grads",
                 "mary grace", "grace piattos", "namatay walang human",
                 "walang human", "kwenta presidente")

cut_end   <- c("walang", "human", "galing", "mary")
cut_start <- c("rights", "grace", "job")

dup_ids  <- clean$doc_id[duplicated(clean[c("case", "commenter_id", "Comment")])]
toks_ng  <- tokens_subset(toks_kwic, !docnames(toks_kwic) %in% dup_ids)

doc_signature <- function(d) {
  m <- d
  vapply(seq_len(ncol(m)), function(j) {
    if (m@p[j + 1] == m@p[j]) return("")
    paste(m@i[(m@p[j] + 1):m@p[j + 1]], collapse = ",")
  }, character(1))
}

ngram_top <- function(n) {
  d <- dfm(tokens_ngrams(toks_ng, n = n, concatenator = " ")) %>%
    dfm_weight(scheme = "boolean")
  parts <- str_split(featnames(d), " ")
  stops <- if (n == 1) c(stop_all, unigram_stop) else stop_all
  keep <- vapply(parts, function(w) !any(w %in% stops) && all(nchar(w) >= 3) &&
                   !anyDuplicated(w) &&
                   (n == 1 || (!tail(w, 1) %in% cut_end && !w[1] %in% cut_start)), logical(1)) &
    !featnames(d) %in% drop_ngrams
  d <- d[, keep]
  if (n > 1) d <- d[, !duplicated(doc_signature(d))]
  textstat_frequency(d, groups = case) %>%
    as_tibble() %>%
    group_by(group) %>%
    slice_max(frequency, n = TOP, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(ngram = ngram_names[n])
}

ng <- bind_rows(lapply(1:3, ngram_top)) %>%
  mutate(ngram = factor(ngram, levels = ngram_names), Case = case_labels[group])
write_csv(ng, "output/Top Ngrams.csv")

font <- "Times New Roman"
if (.Platform$OS.type == "windows") {
  windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
}
dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else
  function(filename, ...) grDevices::png(filename, ..., type = if (.Platform$OS.type == "windows") "windows" else "cairo")

for (cs in unique(ng$Case)) {
  d <- ng %>%
    filter(Case == cs) %>%
    mutate(id = reorder(paste(feature, ngram, sep = "___"), frequency))

  p <- ggplot(d, aes(x = frequency, y = id, fill = ngram)) +
    geom_col(width = 0.85) +
    facet_wrap(~ ngram, nrow = 1, scales = "free") +
    scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = ngram_cols[[cs]], name = "Legend:") +
    labs(x = "Frequency (number of comments)", y = "Terms") +
    theme_minimal(base_size = 12, base_family = font) +
    theme(text = element_text(family = font, colour = "black"),
          axis.text = element_text(colour = "black"),
          axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 6)),
          axis.title.y = element_text(face = "bold", size = 13, margin = margin(r = 6)),
          strip.text = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey88", linewidth = 0.3),
          panel.spacing.x = unit(1.5, "lines"),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background  = element_rect(fill = "transparent", colour = NA),
          legend.position = c(0.94, 0.14),
          legend.title = element_text(face = "bold"),
          legend.key.size = unit(0.45, "cm"),
          legend.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = margin(15, 15, 15, 15))

  ggsave(paste0("output/Ngrams - ", cs, ".png"), p, width = 14, height = 7.5, dpi = 300,
         bg = "transparent", device = dev)
}

pretty <- c(npa = "NPA", up_univ = "UP", lgu = "LGU", saf = "SAF", afp = "AFP", milf = "MILF",
            noynoy = "Noynoy", aquino = "Aquino", duterte = "Duterte", marcos = "Marcos",
            leni = "Leni", roxas = "Roxas", ninoy = "Ninoy", marwan = "Marwan", negros = "Negros",
            bbm = "BBM", pnp = "PNP", pilipinas = "Pilipinas")

kd <- bind_rows(head(key, top_n), tail(key, top_n)) %>%
  distinct(feature, .keep_all = TRUE) %>%
  mutate(Case  = factor(ifelse(chi2 > 0, "Toboso 19", "SAF 44"), levels = c("SAF 44", "Toboso 19")),
         value = abs(chi2),
         label = ifelse(feature %in% names(pretty), pretty[feature], feature)) %>%
  arrange(Case, desc(value))
kd <- kd %>% mutate(id = factor(paste(feature, Case, sep = "___"), levels = rev(paste(feature, Case, sep = "___"))))

p <- ggplot(kd, aes(x = value, y = id, fill = Case)) +
  geom_col(width = 0.75) +
  facet_wrap(~ Case, nrow = 1, scales = "free_y") +
  scale_y_discrete(labels = setNames(kd$label, as.character(kd$id))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("SAF 44" = "#2f5d8a", "Toboso 19" = "#b5473a"),
                    labels = c(expression(italic("SAF 44") ~ "Terms"),
                               expression(italic("Toboso 19") ~ "Terms")),
                    name = "Legend:") +
  labs(x = expression(bold(chi^2)), y = NULL) +
  theme_minimal(base_size = 12, base_family = font) +
  theme(text = element_text(family = font, colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(t = 10)),
        strip.text = element_text(face = "bold.italic", size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        panel.spacing.x = unit(2, "lines"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        plot.margin = margin(15, 15, 15, 15))

ggsave("output/Keyness Figure.png", p, width = 10, height = 7, dpi = 300,
       bg = "transparent", device = dev)
