# functions used by the Bayesian Dixon and Coles SHiny App

# Instead of static MLE estimation (Dixon & Coles) use sequential Bayesian updating + a random walk

# Recursively update theta_t for theta = (alpha, beta, gamma, rho)

clean_team_names <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  return(x)
}

# Extract parameters cleanly 
get_param <- function(theta, name) {
  theta[grep(name, names(theta))]
}

# Extract attack parameters
extract_attack <- function(theta) {
  a <- theta[grep("^attack_", names(theta))]
  names(a) <- gsub("^attack_", "", names(a))
  return(a)
}

# Extract defence paramters
extract_defence <- function(theta) {
  d <- theta[grep("^defence_", names(theta))]
  names(d) <- gsub("^defence_", "", names(d))
  return(d)
}

# random walk to update parameters - sigma determines how dynamic the model is (higher sigma = more reactive model)
evolve_params <- function(theta_prev, sigma = 0.02) {
  theta_new <- theta_prev + rnorm(length(theta_prev), 0, sigma)
  
  # Constrain parameters 
  theta_new[grep("^off___", names(theta_new))] <-
    pmin(pmax(theta_new[grep("^off___", names(theta_new))], -3), 3)
  
  theta_new[grep("^def___", names(theta_new))] <-
    pmin(pmax(theta_new[grep("^def___", names(theta_new))], -3), 3)
  
  theta_new["hfa"] <- pmin(pmax(theta_new["hfa"], -1), 1)
  theta_new["rho"] <- pmin(pmax(theta_new["rho"], -0.1), 0.1)
  
  return(theta_new)
}

# Single match Dixon & Coles likelihood - approximate the log-likelihood contribution for one match
dc_loglik_single <- function(theta, match_row) {
  
  home <- match_row$H.Team
  away <- match_row$A.Team
  x <- match_row$H.goals
  y <- match_row$A.goals
  
  attack <- theta[grep("attack", names(theta))]
  defence <- theta[grep("defence", names(theta))]
  gamma <- theta["home"]
  rho <- theta["rho"]
  
  names(attack) <- gsub("attack_", "", names(attack))
  names(defence) <- gsub("defence_", "", names(defence))
  
  # check names exist
  if (!(home %in% names(attack)) || !(away %in% names(attack))) {
    return(-1e10)}
  
  if (!(home %in% names(defence)) || !(away %in% names(defence))) {
    return(-1e10)}
  
  lambda <- exp(attack[home] + defence[away] + gamma)
  mu     <- exp(attack[away] + defence[home])
  
  if (is.na(lambda) || is.na(mu)) return(-1e10)
  if (lambda <= 0 || mu <= 0) return(-1e10)
  
  # Dixon–Coles adjustment
  tau <- 1
  if (x == 0 && y == 0) tau <- 1 - lambda * mu * rho
  if (x == 0 && y == 1) tau <- 1 + lambda * rho
  if (x == 1 && y == 0) tau <- 1 + mu * rho
  if (x == 1 && y == 1) tau <- 1 - rho
  
  if (is.na(tau) || tau <= 0) tau <- 1e-10
  
  # log Poisson likelihood
  loglik <- log(tau) +
    dpois(x, lambda, log = TRUE) +
    dpois(y, mu, log = TRUE)
  
  return(loglik)
}


# Importance sampling (Bayesian update) 
# sigma = sensitivity of the model (larger = more reactive), S = number of samples
bayes_update <- function(theta_prev, match_row, S = 1000, sigma = 0.02) {
  
  param_names <- names(theta_prev)
  samples <- matrix(0, nrow = S, ncol = length(theta_prev))
  colnames(samples) <- param_names
  log_weights <- numeric(S)
  
  for (s in 1:S) {
    
    # 1. propagate (random walk)
    theta_s <- evolve_params(theta_prev, sigma)
    
    # 2. compute likelihood
    loglik <- dc_loglik_single(theta_s, match_row)
    
    samples[s, ] <- theta_s
    log_weights[s] <- loglik
  }
  
  # stabilise weights
  log_weights <- log_weights - max(log_weights)
  weights <- exp(log_weights)
  weights <- weights / sum(weights)
  
  # posterior mean
  theta_post <- colSums(samples * weights)
  names(theta_post) <- param_names
  
  return(theta_post)
}

# Once you have theta_T the posterior prediction for the next match
predict_match <- function(theta, home, away, max_goals = 6) {
  
  clean_team_names <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- gsub("\\s+", " ", x)
    return(x)
  }
  
  attack <- theta[grep("^off___", names(theta))]
  defence <- theta[grep("^def___", names(theta))]
  
  gamma <- theta["hfa"]
  rho   <- theta["rho"]
  
  names(attack) <- clean_team_names(gsub("^off___", "", names(attack)))
  names(defence) <- clean_team_names(gsub("^def___", "", names(defence)))
  
  # lookup tables 
  attack_lookup <- setNames(names(attack), tolower(names(attack)))
  
  home_key <- tolower(clean_team_names(home))
  away_key <- tolower(clean_team_names(away))
  
  home_resolved <- attack_lookup[home_key]
  away_resolved <- attack_lookup[away_key]
  
  if (is.na(home_resolved)) stop(paste("Home team not found:", home))
  if (is.na(away_resolved)) stop(paste("Away team not found:", away))
  
  lambda <- exp(attack[home_resolved] + defence[away_resolved] + gamma)
  mu <- exp(attack[away_resolved] + defence[home_resolved])
  
  # safety checks
  if (is.na(lambda) || is.na(mu)) stop("lambda or mu is NA")
  if (!is.finite(lambda) || !is.finite(mu)) stop("lambda or mu not finite")
  
  score_mat <- matrix(0, nrow = max_goals + 1, ncol = max_goals + 1)
  rownames(score_mat) <- paste0("H_", 0:max_goals)
  colnames(score_mat) <- paste0("A_", 0:max_goals)
  
  
  for (x in 0:max_goals) {
    for (y in 0:max_goals) {
      
      tau <- 1
      if (x == 0 && y == 0) tau <- 1 - lambda * mu * rho
      if (x == 0 && y == 1) tau <- 1 + lambda * rho
      if (x == 1 && y == 0) tau <- 1 + mu * rho
      if (x == 1 && y == 1) tau <- 1 - rho
      
      if (is.na(tau) || tau <= 0 || !is.finite(tau)) tau <- 1e-10
      
      score_mat[x+1, y+1] <- round(tau *
                                     dpois(x, lambda) *
                                     dpois(y, mu), 4)
    }
  }
  
  # --- totals ---
  row_totals <- rowSums(score_mat)
  col_totals <- colSums(score_mat)
  row_cum <- cumsum(row_totals)
  col_cum <- cumsum(col_totals)
  
  cum_df <- data.frame(
    Goals = 0:max_goals,
    Home_prob = row_totals,
    Home_cum = row_cum,
    Away_prob = col_totals,
    Away_cum = col_cum,
    Home_cum_odds = round(1/row_cum, 2), 
    Away_cum_odds = round(1/col_cum, 2)
  )
  
  # --- full score matrix with totals ---
  score_mat_full <- cbind(score_mat, Row_Total = row_totals)
  score_mat_full <- rbind(score_mat_full,
                          Col_Total = c(col_totals, sum(row_totals))
  )
  
  # --- odds ---
  dec_odds <- round(1/score_mat_full, 2)
  dec_odds[!is.finite(dec_odds)] <- NA
  
  # --- BTTS calculations ---
  btts_yes <- sum(score_mat[-1, -1])
  btts_no  <- 1 - btts_yes
  
  btts_yes_odds <- round(ifelse(btts_yes > 0, 1 / btts_yes, NA), 2)
  btts_no_odds  <- round(ifelse(btts_no > 0, 1 / btts_no, NA), 2)
  
  btts_df <- data.frame(
    Outcome = c("Yes", "No"),
    Probability = c(btts_yes, btts_no),
    Odds = c(btts_yes_odds, btts_no_odds))
  
  Under_2.5 <- sum(score_mat[1,1:3]) + sum(score_mat[1:3, 1]) + score_mat[2,2] - score_mat[1,1]
  Over_2.5 <- 1 - Under_2.5
  
  Un2.5_odds <- round(ifelse(Under_2.5 > 0, 1 / Under_2.5, NA), 2)
  Ov2.5_odds <- round(ifelse(Over_2.5 > 0, 1 / Over_2.5, NA), 2)
  
  Ov_Un_2.5df <- data.frame(Outcome = c("Over 2.5", "Under 2.5"),
                            Probability = c(Over_2.5, Under_2.5),
                            Odds = c(Ov2.5_odds, Un2.5_odds))
  
  list(
    score_matrix = score_mat_full,
    odds_matrix = dec_odds,
    cum_df = cum_df,
    BTTS = btts_df,
    Ov_Un_2.5 = Ov_Un_2.5df,
    home_win = c(sum(score_mat[row(score_mat) > col(score_mat)]), 
                 1/sum(score_mat[row(score_mat) > col(score_mat)])),
    draw     = c(sum(diag(score_mat)),
                 1/sum(diag(score_mat))),
    away_win = c(sum(score_mat[row(score_mat) < col(score_mat)]),
                 1/sum(score_mat[row(score_mat) < col(score_mat)]))
  )
}