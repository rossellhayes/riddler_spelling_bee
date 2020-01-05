library(pacman)
p_load(here, dplyr, dtplyr, furrr, magrittr, purrr, readr, stringr, tibble, tidyr)
plan(multiprocess)

# vowels    <- c("a", "e", "i", "o", "u")
# uncommons <- c("q", "z", "j", "x", "k", "f", "v", "w", "y")

word_list <- read_lines(here("enable1.txt")) %>%
  discard(str_detect, "s") %>%
  discard(~ nchar(.) < 4) %>%
  enframe(name = NULL, value = "word") %>%
  mutate(
    chars    = future_map(word, ~ str_split(., "") %>% flatten_chr()),
    n_chars  = nchar(word),
    unique   = future_map_chr(chars, ~ paste(sort(unique(.)), collapse = "")),
    n_unique = nchar(unique)
  ) %>%
  filter(n_unique <= 7) %>%
  mutate(
    pangram = n_unique == 7,
    score   = if_else(n_chars == 4, 1L, n_chars) + if_else(pangram, 7, 0)
  )

words <- word_list %>%
  group_by(unique) %>%
  summarize(score = sum(score)) %>%
  mutate(
    chars = str_split(unique, ""),
    dummy = 1
  )

letter_scores <- letters %>%
  enframe(name = NULL, value = "letters") %>%
  mutate(dummy = 1) %>%
  split(seq(nrow(.))) %>%
  future_map_dfr(
    ~ full_join(words, ., by = "dummy") %>%
      lazy_dt() %>%
      filter(future_map2_lgl(letters, chars, ~ .x %in% .y)) %>%
      group_by(letters) %>%
      summarize(score = sum(score)) %>%
      as_tibble()
  ) %>%
  arrange(desc(score))

top_letters <- letter_scores %>%
  top_n(16) %>%
  pull(letters)

honeycombs <- combn(top_letters, 7) %>%
  as_tibble() %>%
  as.list() %>%
  enframe(name = NULL, value = "letters") %>%
  filter(future_map_lgl(letters, ~ any(vowels %in% .))) %>%
  mutate(
    string = future_map_chr(letters, paste, collapse = ""),
    dummy  = 1
  )

honeycomb_scores <- honeycombs %>%
  split(seq(nrow(.))) %>%
  future_map_dfr(
    ~ full_join(words, ., by = "dummy") %>%
      lazy_dt() %>%
      filter(future_map2_lgl(chars, letters, ~ all(.x %in% .y))) %>%
      group_by(string) %>%
      summarize(score = sum(score)) %>%
      as_tibble()
  ) %>%
  arrange(desc(score))

top_honeycombs <- honeycomb_scores %>%
  top_n(400) %>%
  mutate(
    score   = NULL,
    letters = str_split(string, ""),
    dummy   = 1
  )

possibilities <- future_map_dfr(
  1:7,
  function(i) {
    mutate(
      top_honeycombs,
      center = map_chr(
        letters, function(x) x[i]
      )
    )
  }
)

scores <- possibilities %>%
  split(seq(nrow(.))) %>%
  future_map_dfr(
    ~ full_join(words, ., by = "dummy") %>%
      lazy_dt() %>%
      filter(
        future_map2_lgl(chars, letters, ~ all(.x %in% .y)),
        future_map2_lgl(center, chars, ~ .x %in% .y)
      ) %>%
      group_by(string, center) %>%
      summarize(score = sum(score)) %>%
      as_tibble()
  ) %>%
  arrange(desc(score))

pangram_check <- word_list %>%
  filter(unique == "aeginrt")

write_csv(scores, "scores.csv")

