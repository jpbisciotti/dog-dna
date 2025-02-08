# Description ------------------------------------------------------------------

# Doggy DNA Breed Analysis: Analyzes family members' guesses of a mixed-breed
# puppy's DNA test results. We adopted a puppy of mixed breed. Curious about her
# breed, we got her a doggy DNA test. Before the results were in, members of the
# family guessed her breed. The code below determines how well each family
# member guessed our new puppy's breed. I shared the results with family to
# declare a winner.

# Raw Data ---------------------------------------------------------------------

# Enter data
doggo <- tibble::tribble(
  ~"breed",                             ~"truth",    ~"me", ~"spouse", ~"fil", ~"mil", ~"bro", ~"ma", 
  "Australian Cattle Dog",                   54.7,      35,        30,      0,      0,     25,    50,
  "Pit Bull & Staffordshire Terrier",   18.6+11.2,      15,        35,      0,      0,     25,     0,
  "American Bully",                           5.5,       0,         0,      0,      0,      0,     0,
  "Other Breeds",                               0,      50,        35,    100,    100,     50,    50
) 

# Show data with totals
doggo |> 
  janitor::adorn_totals() |> 
  gt::gt()

# Distances --------------------------------------------------------------------

# Calculate various distances
distances <- purrr::map(
  names(dplyr::select(doggo, -breed)),
  function(person) {
    n = 1000
    nudge = 1
    truth <- round((doggo[["truth"]] / sum(doggo[["truth"]])) * n) + nudge
    guess <- round((doggo[[person]] / sum(doggo[[person]])) * n) + nudge
    kl_divergence <- sum(truth * log(truth/guess))
    js_divergence <- 0.5 * sum(truth * log(2 * truth/(truth + guess)) + guess * log(2 * guess/(truth + guess)))
    euclidean_dist <- sqrt(sum((truth - guess)^2))
    out <- tibble::tibble(
      "person" = person, 
      "kl" = kl_divergence, 
      "js" = js_divergence, 
      "eu" = euclidean_dist
      )
    return(out)
  }
) |> purrr::list_rbind() |> 
  dplyr::mutate(rank = dplyr::dense_rank(kl/max(kl) + js/max(js) + eu/max(eu))) |> 
  dplyr::arrange(rank)

# Rank of each person's guess
distances |> 
  dplyr::select(rank, person, distance = kl) |> 
  dplyr::mutate(scaled_distance = scales::percent(distance / max(distance), accuracy = 0.1)) |> 
  dplyr::mutate(distance = scales::number(distance, accuracy = 1, big.mark = ",")) |> 
  gt::gt()

distances |> 
  dplyr::select(rank, person, distance = js) |> 
  dplyr::mutate(scaled_distance = scales::percent(distance / max(distance), accuracy = 0.1)) |> 
  dplyr::mutate(distance = scales::number(distance, accuracy = 1, big.mark = ",")) |> 
  gt::gt()

# Plot distribution of guesses vs truth for each breed
doggo |> 
  tidyr::pivot_longer(-c(breed)) |> 
  dplyr::mutate(truth = ifelse(name != "truth", NA, value)) |> 
  dplyr::mutate(person = ifelse(name == "truth", NA, value)) |> 
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(group = name, x = breed, y = person, color = name, shape = name), 
                       size = 5, width = 0.1, height = 0.1) +
  ggplot2::geom_point(ggplot2::aes(group = name, x = breed, y = truth), size = 7) +
  ggplot2::facet_wrap(ggplot2::vars(breed), scales = "free_x", ncol = 4) + 
  ggplot2::labs(y = "percent", x = "breed") + 
  ggplot2::theme_bw()

# ------------------------------------------------------------------------------

# Function to compare two probability distributions
compare_distributions <- function(
    truth, 
    guess, 
    suppress_warnings = FALSE
) {
  
  # # Example usage
  # truth <- c(a=0.10, b=0.20, c=0.30, d=0.40)
  # # Similar guess
  # guess1 <- c(a=0.09, b=0.21, c=0.29, d=0.41)
  # results1 <- compare_distributions(truth, guess1)
  # # Different guess
  # guess2 <- c(a=0.40, b=0.30, c=0.20, d=0.10)
  # results2 <- compare_distributions(truth, guess2)
  
  # Validate inputs
  if (length(truth) != length(guess)) {
    stop("Each vector must be the same length")
  }
  if (abs(sum(truth) - 1) > 1e-10 || abs(sum(guess) - 1) > 1e-10) {
    stop("Each vector must sum to 1")
  }
  if (any(truth < 0) || any(guess < 0)) {
    stop("All probabilities must be non-negative")
  }
  
  # Perform chi-square test
  # Convert probabilities to counts using sample size n
  n = 1000
  truth_counts <- round(truth * n)
  guess_counts <- round(guess * n)
  
  # Define the proc_chi as an expression for suppress_warnings control flow. 
  proc_chi <- expression(chisq.test(x = truth_counts, y = guess_counts))
  
  # Run chi-square test with or without warning suppression based on parameter
  chisq_test <- if (suppress_warnings) {
    suppressWarnings({eval(proc_chi)}) 
  } else {
    eval(proc_chi)
  }
  
  # Create out list
  out <- list(
    statistic = chisq_test$statistic,
    p_value = chisq_test$p.value,
    interpretation = ifelse(chisq_test$p.value < 0.05, 
                            "significantly different", 
                            "not significantly different")
  )
  
  invisible(out)
}

vars_person <- doggo |> dplyr::select(-breed, -truth) |> names()

# Test goodness-of-fit
gof <- purrr::map(
  seq_along(vars_person), 
  function(i) {
    
    person <- vars_person[i]
    truth <- doggo[["truth"]] / sum(doggo[["truth"]])
    guess <- doggo[[person]] / sum(doggo[[person]])
    
    comparison <- compare_distributions(
      truth = truth, 
      guess = guess, 
      suppress_warnings = TRUE
      )
    
    out <- tibble::tibble(
      "person" = person, 
      "p_value" = comparison$p_value, 
      "interpretation" = comparison$interpretation
      )
    
    return(out)
  }
) |> 
  purrr::list_rbind() |> 
  dplyr::arrange(p_value)

gof

# ------------------------------------------------------------------------------