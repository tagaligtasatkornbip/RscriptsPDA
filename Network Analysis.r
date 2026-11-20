library(quanteda)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(igraph)
library(ggraph)
library(ggrepel)

dir.create("output/network", recursive = TRUE, showWarnings = FALSE)

colors <- c(
  SAF44 = "#2f5d8a",
  TOBOSO19 = "#b5473a"
)

labels <- c(
  SAF44 = "SAF 44",
  TOBOSO19 = "Toboso 19"
)

top_features <- c(
  unigram = 50,
  bigram = 100,
  trigram = 100
)

minimum_edge_comments <- 1

edges_by_count_per_type <- 2
edges_by_rate_per_type <- 4

drop_terms <- c("johanna", "janth")

drop_ngrams <- c(
  "janth claire flores", "johanna roxas", "job afp",
  "job afp ubusin", "job philippine army", "good job philippine",
  "salamat mary grace", "ren up_univ grads", "mary grace",
  "grace piattos", "namatay walang human", "walang human",
  "kwenta presidente", "sto nino viva", "boy sili", "juan saamerica", 
  "bala laglag baril", "manggagawa magsasaka mangingisda", "city negros occidental",
  "magsasaka mangingisda tsuper", "good job mary", "syempre pre galit", 
  "malayong barangay pasasamahan", "mangingisda tsuper etc", "totoo walang ideolohiyang",
  "elmer sportrock", "human rights npa"
)

cut_end <- c("walang", "human", "galing")
cut_start <- c("rights")

duplicate_rows <- duplicated(
  clean[c("case", "commenter_id", "Comment")]
)
dup_ids <- clean$doc_id[duplicate_rows]

select_top_features <- function(dfm_input, ngram_type, n_tokens) {

  if (quanteda::nfeat(dfm_input) == 0) {
    return(tibble(
      term = character(),
      ngram = character(),
      n_tokens = integer(),
      n_comments = integer()
    ))
  }

  limit <- min(
    unname(top_features[[ngram_type]]),
    quanteda::nfeat(dfm_input)
  )

  ranked <- quanteda::topfeatures(
    quanteda::dfm_weight(dfm_input, scheme = "boolean"),
    n = limit
  )

  selected <- names(ranked)
  feature_docfreq <- quanteda::docfreq(dfm_input)

  tibble(
    term = selected,
    ngram = ngram_type,
    n_tokens = as.integer(n_tokens),
    n_comments = as.integer(feature_docfreq[selected])
  )
}

make_ngram_dfm <- function(toks_case, n) {

  ngram_tokens <- quanteda::tokens_ngrams(
    toks_case,
    n = n,
    concatenator = " "
  )

  ngram_dfm <- quanteda::dfm(ngram_tokens)

  if (quanteda::nfeat(ngram_dfm) == 0) {
    return(ngram_dfm)
  }

  features <- quanteda::featnames(ngram_dfm)
  parts <- strsplit(features, " ", fixed = TRUE)

  keep <- vapply(parts, function(x) {
    length(x) == n &&
      all(nchar(x) >= 3 & !(x %in% stop_all)) &&
      !anyDuplicated(x) &&
      !(tail(x, 1) %in% cut_end) &&
      !(x[1] %in% cut_start)
  }, logical(1))

  keep <- keep & !(features %in% drop_ngrams)

  ngram_dfm[, keep, drop = FALSE]
}

make_feature_pool <- function(case_name) {

  toks_case <- quanteda::tokens_subset(
    toks,
    case == case_name & !(docnames(toks) %in% dup_ids)
  )

  toks_ns_case <- quanteda::tokens_subset(
    toks_ns,
    case == case_name & !(docnames(toks_ns) %in% dup_ids)
  )

  unigram_dfm <- quanteda::dfm(
    toks_ns_case,
    remove_padding = TRUE
  ) |>
    quanteda::dfm_remove(drop_terms, valuetype = "fixed")

  bigram_dfm <- make_ngram_dfm(toks_case, n = 2)
  trigram_dfm <- make_ngram_dfm(toks_case, n = 3)

  feature_pool <- bind_rows(
    select_top_features(unigram_dfm, "unigram", 1),
    select_top_features(bigram_dfm, "bigram", 2),
    select_top_features(trigram_dfm, "trigram", 3)
  ) |>
    distinct(term, .keep_all = TRUE) |>
    arrange(
      factor(ngram, levels = c("unigram", "bigram", "trigram")),
      desc(n_comments)
    )

  list(
    features = feature_pool,
    toks_case = toks_case
  )
}

extract_selected_sequence <- function(token_vector, feature_table,
                                      doc_id, case_name) {

  empty_result <- tibble(
    case = character(),
    doc_id = character(),
    sequence_order = integer(),
    term = character(),
    ngram = character(),
    n_tokens = integer(),
    start_token = integer(),
    end_token = integer()
  )

  if (length(token_vector) == 0 || nrow(feature_table) == 0) {
    return(empty_result)
  }

  feature_sets <- list(
    `1` = feature_table$term[feature_table$n_tokens == 1],
    `2` = feature_table$term[feature_table$n_tokens == 2],
    `3` = feature_table$term[feature_table$n_tokens == 3]
  )

  output <- vector("list", length(token_vector))
  output_n <- 0L
  i <- 1L
  sequence_order <- 0L
  token_count <- length(token_vector)

  while (i <= token_count) {

    matched <- FALSE

    for (k in 3:1) {

      end_position <- i + k - 1L

      if (end_position <= token_count) {

        candidate <- paste(
          token_vector[i:end_position],
          collapse = " "
        )

        if (candidate %in% feature_sets[[as.character(k)]]) {

          output_n <- output_n + 1L
          sequence_order <- sequence_order + 1L

          output[[output_n]] <- tibble(
            case = case_name,
            doc_id = doc_id,
            sequence_order = sequence_order,
            term = candidate,
            ngram = c("unigram", "bigram", "trigram")[k],
            n_tokens = as.integer(k),
            start_token = as.integer(i),
            end_token = as.integer(end_position)
          )

          i <- end_position + 1L
          matched <- TRUE
          break
        }
      }
    }

    if (!matched) {
      i <- i + 1L
    }
  }

  if (output_n == 0L) {
    return(empty_result)
  }

  bind_rows(output[seq_len(output_n)])
}

make_transitions <- function(sequence_table) {

  if (nrow(sequence_table) == 0) {
    return(tibble(
      case = character(),
      doc_id = character(),
      from = character(),
      from_ngram = character(),
      to = character(),
      to_ngram = character(),
      gap_tokens = integer()
    ))
  }

  sequence_table |>
    arrange(case, doc_id, sequence_order) |>
    group_by(case, doc_id) |>
    mutate(
      to = lead(term),
      to_ngram = lead(ngram),
      to_start = lead(start_token),
      gap_tokens = to_start - end_token - 1L
    ) |>
    ungroup() |>
    filter(!is.na(to), term != to) |>
    transmute(
      case,
      doc_id,
      from = term,
      from_ngram = ngram,
      to,
      to_ngram,
      gap_tokens = as.integer(gap_tokens)
    )
}

select_plot_edges <- function(nodes, edges) {

  source_counts <- nodes |>
    transmute(
      from = term,
      from_comments = n_comments
    )

  ranked_edges <- edges |>
    left_join(source_counts, by = "from") |>
    mutate(
      transition_rate = n / pmax(from_comments, 1L),
      transition_type = paste(from_ngram, to_ngram, sep = " -> ")
    ) |>
    filter(n >= minimum_edge_comments)

  if (nrow(ranked_edges) == 0) {
    return(ranked_edges)
  }

  top_by_count <- ranked_edges |>
    filter(n >= 2) |>
    group_by(from_ngram, to_ngram) |>
    slice_max(
      order_by = n,
      n = edges_by_count_per_type,
      with_ties = FALSE
    ) |>
    ungroup()

  top_by_rate <- ranked_edges |>
    filter(from_comments >= 3) |>
    group_by(from_ngram, to_ngram) |>
    slice_max(
      order_by = transition_rate,
      n = edges_by_rate_per_type,
      with_ties = FALSE
    ) |>
    ungroup()

  bind_rows(top_by_count, top_by_rate) |>
    distinct(from, to, .keep_all = TRUE) |>
    arrange(
      factor(from_ngram, levels = c("unigram", "bigram", "trigram")),
      factor(to_ngram, levels = c("unigram", "bigram", "trigram")),
      desc(n),
      desc(transition_rate)
    )
}

plot_one_network <- function(case_name, nodes, edges_to_plot) {

  label <- labels[[case_name]]
  colour <- colors[[case_name]]

  if (nrow(edges_to_plot) == 0) {
    message(
      "No sequential transitions meet the display threshold for ",
      label, "."
    )
    return(invisible(NULL))
  }

  connected_terms <- unique(c(edges_to_plot$from, edges_to_plot$to))

  nodes_to_plot <- nodes |>
    filter(term %in% connected_terms) |>
    transmute(
      name = term,
      ngram = factor(
        ngram,
        levels = c("unigram", "bigram", "trigram")
      ),
      n_comments
    )

  graph <- igraph::graph_from_data_frame(
    d = edges_to_plot |>
      select(from, to, n),
    directed = TRUE,
    vertices = nodes_to_plot
  )

  set.seed(199)

  network_plot <- ggraph::ggraph(graph, layout = "fr") +
    ggraph::geom_edge_fan(
      ggplot2::aes(edge_width = n),
      colour = colour,
      alpha = 0.45,
      arrow = grid::arrow(
        length = grid::unit(1.8, "mm"),
        type = "closed"
      ),
      end_cap = ggraph::circle(2, "mm")
    ) +
    ggraph::scale_edge_width(
      range = c(0.25, 1.5),
      name = "Comments (n)"
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(shape = ngram),
      size = 3.5,
      colour = colour,
      fill = colour
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        unigram = 21,
        bigram = 22,
        trigram = 23
      ),
      name = "Node type",
      labels = c(
        unigram = "Unigram",
        bigram = "Bigram",
        trigram = "Trigram"
      )
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name),
      repel = TRUE,
      max.overlaps = Inf,
      box.padding = 0.65,
      point.padding = 0.4,
      force = 4,
      min.segment.length = 0,
      size = 3.1,
      family = "sans",
      colour = "grey15"
    ) +
    ggplot2::labs(
      title = paste("Sequential Network Analysis of", label),
      subtitle = paste0(
        "Directed transitions between non-overlapping expressions; ",
        "edge width = number of comments containing the transition"
      ),
      x = "X",
      y = "Y"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 14
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 8.5,
        colour = "grey35"
      ),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8, colour = "grey35"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 8),
      plot.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = ggplot2::margin(12, 16, 12, 16)
    )

  output_file <- paste0(
    "output/network/",
    case_name,
    "_mixed_sequential_network.png"
  )

  ggplot2::ggsave(
    filename = output_file,
    plot = network_plot,
    width = 17,
    height = 13,
    dpi = 320,
    bg = "white"
  )

  invisible(network_plot)
}

network_results <- list()

for (case_name in names(colors)) {

  prepared <- make_feature_pool(case_name)
  feature_table <- prepared$features
  toks_case <- prepared$toks_case

  token_lists <- as.list(toks_case)
  names(token_lists) <- quanteda::docnames(toks_case)

  sequence_table <- bind_rows(lapply(names(token_lists), function(id) {
    extract_selected_sequence(
      token_vector = token_lists[[id]],
      feature_table = feature_table,
      doc_id = id,
      case_name = case_name
    )
  }))

  transition_events <- make_transitions(sequence_table)

  edges <- transition_events |>
    group_by(from, from_ngram, to, to_ngram) |>
    summarise(
      n = n_distinct(doc_id),
      transition_occurrences = n(),
      min_gap_tokens = min(gap_tokens, na.rm = TRUE),
      mean_gap_tokens = mean(gap_tokens, na.rm = TRUE),
      max_gap_tokens = max(gap_tokens, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(n), desc(transition_occurrences))

  nodes <- feature_table |>
    transmute(
      term,
      ngram,
      n_comments
    ) |>
    arrange(
      factor(ngram, levels = c("unigram", "bigram", "trigram")),
      desc(n_comments)
    )

  file_prefix <- paste0(
    "output/network/",
    case_name,
    "_sequential_network("
  )

  plot_edges <- select_plot_edges(nodes, edges)

  readr::write_csv(feature_table, paste0(file_prefix, "features).csv"))
  readr::write_csv(sequence_table, paste0(file_prefix, "sequences).csv"))
  readr::write_csv(
    transition_events,
    paste0(file_prefix, "transition_events).csv")
  )
  readr::write_csv(edges, paste0(file_prefix, "edges).csv"))
  readr::write_csv(plot_edges, paste0(file_prefix, "plot_edges).csv"))
  readr::write_csv(nodes, paste0(file_prefix, "nodes).csv"))

  plot_one_network(case_name, nodes, plot_edges)

  phrase_terms <- c(
    "alyssa alano",
    "human rights violation",
    "rebelde npa tatag",
    "matatalinong estudyante napapatay",
    "estudyante galing china",
    "worst presidente ever",
    "aquino buti namatay",
    "full blooded chinese"
  )

  phrase_nodes_found <- nodes |>
    filter(term %in% phrase_terms)

  if (nrow(phrase_nodes_found) > 0) {
    print(phrase_nodes_found)
  }

  phrase_edges <- edges |>
    filter(from %in% phrase_terms | to %in% phrase_terms)

  if (nrow(phrase_edges) > 0) {
    print(phrase_edges)
  }

  network_results[[case_name]] <- list(
    features = feature_table,
    sequences = sequence_table,
    transition_events = transition_events,
    edges = edges,
    nodes = nodes
  )

}
