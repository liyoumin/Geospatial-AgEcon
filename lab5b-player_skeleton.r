##	This is where you should start programming
##		your very own rock-paper-scissors bot:

player_skeleton <- function(my_plays=NULL, their_plays=NULL, outcomes=NULL) {
	##	I gonna get you with scissors!
	play <- "scissors"
	return(play)
}

##	Let's load two of the sample players. We can	do this by using the source() function
##		to load and evaluate the contents of a source code file.  By default, source() looks
##		in the current working directory for thespecified file, so set your working directory to the /src folder from the lab:
 setwd("/Users/macpro/Desktop/Youmin-phd/geospatical/Lab 5b - Functions, Roshambo Battle Royale/src/")
 source("player_randombot.r")
 source("player_rockbot.r")
 source("player_rotatebot.r")
 source("player_r226bot.r")
 source("player_switchbot.r")
 source("player_copybot.r")
 source("player_freqbot.r")
 
 ##	What did these source files load?
 ls()

##	Examine the contents and structure of these 
##		players:
 class(player_rockbot)
 args(player_rockbot)
 formals(player_rockbot)
 body(player_rockbot)

##	You should have noticed by now that our players	will have no other goal than 
##  to take the information provided, which is the history of our own plays, 
## the history of our opponents	plays, and the outcomes of those plays as	arguments, 
## perform some magic and return our next play based on what that magic tells us to do. 
## We need another function to actually conduct the competition between our players.
##	To pit our player against one of the other players we use the roshambo() function	from the "roshambo.r" source code:
source("roshambo.r")
roshambo(player_rockbot, player_randombot)

##	Look at the supplied arguments, which player is which by default, and how can we change
##		the number of rounds over which the competition	runs?  How does it determine who wins each round
##		and what does it do with that information?

##	Finally, if we source the throwdown() function from its source code file, you can pit your
##		player against all of the sample players I've	provided you.  Your grade will be based on your
##		ability to score a combined win/loss ratio greater than 1.9 over 1000 rounds against each competitor:
 source("throwdown.r")
 throwdown(player_randombot, player_copybot)
 args(throwdown)
### creat my own bot
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
youmin_bot <- function(opp = character(), me = character(), out = character()) {
  n <- length(opp)
  if (n == 0) return(sample(MOVES, 1))  # Round 1 random
  # --- Helper: frequency analysis over last 10 rounds ---
  lookback <- 10
  recent <- tail(opp, lookback)
  freq <- sort(table(recent), decreasing = TRUE)
  most_common <- names(freq)[1]
  # --- Helper: win–stay lose–shift rule detection ---
  if (n >= 2) {
    last_out <- tail(out, 1)
    if (last_out == "W") pred <- tail(opp, 1)  # they might repeat
    else if (last_out == "L") pred <- beats(tail(me, 1)) # they countered last time
    else pred <- most_common
  } else {
    pred <- most_common
  }
  # --- Mix strategies: slight randomness ---
  if (runif(1) < 0.1) pred <- sample(MOVES, 1)
  move <- beats(pred)
  return(move)
}


# Outcome function: returns "W","L","T" for youmin_bot vs opponent
round_outcome <- function(my, opp) {
if (my == opp) return("T")
if (beats(opp) == my) "W" else "L"
}

# Call opponent robustly no matter what formal args it uses
call_opponent <- function(opponent, opp_hist, me_hist, out_hist) {
  fmls <- names(formals(opponent))         # NULL or character()
  # No formals or zero-arg bot (e.g., stateful closure) → just call it
  if (is.null(fmls) || length(fmls) == 0L) return(opponent())
  # If it has "...", we can safely pass all three by name
  if ("..." %in% fmls) return(opponent(opp = opp_hist, me = me_hist, out = out_hist))
  # Build a named list of only the arguments it actually accepts
  args <- list()
  if ("opp" %in% fmls) args$opp <- opp_hist
  if ("me"  %in% fmls) args$me  <- me_hist
  if ("out" %in% fmls) args$out <- out_hist
  do.call(opponent, args)
}

throwdown_1 <- function(N = 1000L, opponent) {
  opp_hist <- character(0); me_hist <- character(0); out_hist <- character(0)
  w <- l <- t <- 0L
  for (r in seq_len(N)) {
    my <- youmin_bot(opp_hist, me_hist, out_hist)
    op <- call_opponent(opponent, opp_hist, me_hist, out_hist)
    res <- round_outcome(my, op)
    w <- w + (res == "W"); l <- l + (res == "L"); t <- t + (res == "T")
    opp_hist <- c(opp_hist, op); me_hist <- c(me_hist, my); out_hist <- c(out_hist, res)
  }
  ratio <- if (l == 0L) Inf else w / l
  list(W = as.integer(w), L = as.integer(l), T = as.integer(t), W_L_ratio = ratio)
}

## quick tests (run these lines in R after sourcing this file):
source("youmin_bot.R")
roshambo(youmin_bot, player_randombot)
throwdown_1(N = 1000, opponent = player_copybot)
throwdown_1(N = 1000, opponent = player_rockbot)
throwdown_1(N = 1000, opponent = player_randombot)
throwdown_1(N = 1000, opponent = player_rotatebot)
throwdown_1(N = 1000, opponent = player_switchbot)
throwdown_1(N = 1000, opponent = player_freqbot)
### > throwdown_1(N = 1000, opponent = player_freqbot)
# $W [1] 940 $L [1] 30; $T [1] 30; $W_L_ratio [1] 31.33333>1.9

# --- Adapter that accepts by "throwndown" ---
youmin_bot <- function(
    opp = NULL, me = NULL, out = NULL,
    my_plays = NULL, their_plays = NULL, outcomes = NULL, ...
) {
  # If lab-style names are present, map them to our internal names
  if (!is.null(my_plays) || !is.null(their_plays) || !is.null(outcomes)) {
    me  <- if (!is.null(my_plays))    my_plays    else me
    opp <- if (!is.null(their_plays)) their_plays else opp
    out <- if (!is.null(outcomes))    outcomes    else out
  }
  # Normalize NULLs to empty vectors
  if (is.null(opp)) opp <- character(0)
  if (is.null(me))  me  <- character(0)
  if (is.null(out)) out <- character(0)
  youmin_bot_core(opp, me, out)
}

throwdown(youmin_bot, rounds = 1000)


### A random bot wins ≈1/3 of rounds → win/loss ≈ 1.0.To get >1.9, we need exploit opponent predictability.
### Repeat-detection: If the opponent repeats moves, counter it.
### Win-stay/lose-shift detection: Opponents often repeat when they win and switch after losing.
### Frequency analysis: Look back over the last 5–10 rounds and counter the most common move.
### Markov prediction: Predict the next move based on transitions from the opponent’s last move.
### Mixed/randomized counter: Add small randomness to avoid being predictable.