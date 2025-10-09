## ===============================================================
## Lab 5 – Functions: Roshambo Battle Royale (RPS Bot)
## ===============================================================
## File to submit: youmin_bot.R (this file)
## - Defines function youmin_bot() that returns one of: "rock","paper","scissors"
## - Compatible with typical skeletons that pass histories as vectors
## - Includes a local test harness (throwdown_local) for quick checks
## Strategy (hybrid, adaptive):
##  1) Meta-predictors guess opponent next move using:
##     - LastMove (repeat tendency)
##     - Frequency-in-window (dominant move in recent window)
##     - Markov-1 (transition counts from opponent's last move)
##     - Counter-MyLast (assumes they try to beat my last)
##  2) Track rolling scores of each meta over past rounds; pick best
##  3) Inject small randomness to avoid being exploited
## Expected to outperform naive bots; tune WINDOW and EPS if needed.
## ===============================================================


MOVES <- c("rock","paper","scissors")

beats <- function(m) switch(m,
  rock = "paper",       # paper beats rock
  paper = "scissors",   # scissors beats paper
  scissors = "rock")    # rock beats scissors

beaten_by <- function(m) switch(m,
  rock = "scissors",
  paper = "rock",
  scissors = "paper")

rand_move <- function() sample(MOVES, 1L)

## ---- Core bot --------------------------------------------------
## Signature aims to match common skeletons:
## - opp: opponent move history (character vector)
## - me:  my move history (character vector)
## - out: outcomes history: "W","L","T" (optional)
## Fallbacks allow running with no args for round 1.
youmin_bot <- function(opp = character(), me = character(), out = character(), ...) {
  n <- length(opp)
  if (length(me) != n) me <- me[seq_len(min(length(me), n))]
  WINDOW <- 12L        # window length for recent behavior
  EPS <- 0.10          # exploration probability

  # Round 1: start randomized
  if (n == 0L) return(rand_move())

  last_opp <- opp[n]
  last_me  <- if (n >= 1L) me[n] else sample(MOVES,1L)

  # --- Meta predictors (return predicted opponent next move) -----
  pred_lastmove <- function() last_opp

  pred_freq <- function() {
    w <- seq.int(max(1L, n - WINDOW + 1L), n)
    tab <- sort(table(opp[w]), decreasing = TRUE)
    names(tab)[1L]
  }

  pred_markov1 <- function() {
    if (n < 2L) return(last_opp)
    trans <- list(rock = c(rock=0,paper=0,scissors=0),
                  paper = c(rock=0,paper=0,scissors=0),
                  scissors = c(rock=0,paper=0,scissors=0))
    start <- max(1L, n - WINDOW + 1L)
    for (i in seq.int(start, n-1L)) {
      a <- opp[i]; b <- opp[i+1L]
      trans[[a]][b] <- trans[[a]][b] + 1L
    }
    nxt <- names(which.max(trans[[last_opp]]))
    if (length(nxt) == 0L || is.na(nxt)) last_opp else nxt
  }

  pred_counter_mylast <- function() {
    # predict they will try to beat my last move
    beats(last_me)
  }

  predictors <- list(lastmove = pred_lastmove,
                     freq     = pred_freq,
                     markov1  = pred_markov1,
                     anti_me  = pred_counter_mylast)

  # --- Score predictors over recent rounds -----------------------
  score_predictor <- function(pfun) {
    # retrospective scoring: for each past round t, if pfun would have
    # correctly predicted opp[t], then the counter move would have won against opp[t]
    if (n == 0L) return(0)
    start <- max(1L, n - WINDOW + 1L)
    s <- 0L
    for (t in seq.int(start, n)) {
      # build context up to t-1 (simulate "what would it have predicted then")
      opp_hist <- opp[seq_len(t-1L)]
      me_hist  <- me[seq_len(t-1L)]
      pred <- tryCatch(pfun(), error = function(e) NA_character_)
      # If no history yet, skip
      if (is.na(pred)) next
      # If predicted opponent at t equals actual opponent at t, the chosen counter would have won
      if (t <= length(opp) && !is.na(opp[t])) {
        counter <- beats(pred)          # what we would play to beat predicted opp
        actual  <- opp[t]
        # win if counter beats actual
        if (counter == beats(actual)) s <- s + 1L
      }
    }
    s
  }

  scores <- sapply(predictors, score_predictor)
  # Choose the predictor with the highest retrospective score (break ties randomly)
  best_names <- names(scores)[which(scores == max(scores))]
  chosen <- sample(best_names, 1L)

  predicted_opp <- predictors[[chosen]]()
  # Main counter: play the move that beats the predicted opponent move
  move <- beats(predicted_opp)

  # Small exploration to avoid being fully predictable
  if (runif(1) < EPS) move <- rand_move()
  move
}


# Outcome function: returns "W","L","T" for youmin_bot vs opponent
round_outcome <- function(my, opp) {
  if (my == opp) return("T")
  if (beats(opp) == my) return("W") else return("L")
}

throwdown_local <- function(N = 1000L, opponent) {
  opp_hist <- character(0); me_hist <- character(0); out_hist <- character(0)
  w <- l <- t <- 0L
  for (r in seq_len(N)) {
    my <- youmin_bot(opp_hist, me_hist, out_hist)
    op <- opponent(opp = opp_hist, me = me_hist, out = out_hist)
    res <- round_outcome(my, op)
    w <- w + as.integer(res == "W")
    l <- l + as.integer(res == "L")
    t <- t + as.integer(res == "T")
    opp_hist <- c(opp_hist, op); me_hist <- c(me_hist, my); out_hist <- c(out_hist, res)
  }
  ratio <- if (l == 0L) Inf else w / l
  list(W = w, L = l, T = t, W_L_ratio = ratio)
}

## Example quick tests (run these lines in R after sourcing this file):
 source("youmin_bot.R")
 throwdown_local(1000, player_randombot)
 throwdown_local(1000, player_copybot)
 throwdown_local(1000, player_freqbot)
 throwdown_local(1000, player_r226bot)
 throwdown_local(1000, player_rockbot)
