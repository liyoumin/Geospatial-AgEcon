## Lab 5 – Controlling Program Flow
## ===============================================================
## Implements all required features per assignment:
## 1) Interactive X/Y entry with 'quit' to end
## 2) Store points, plot all points after each entry
## 3) Label every point with distance to the latest point
## 4) Draw "starburst" segments from each point to the newest point
## 5) After quitting: print single-line summary stats
## 6) Build data.frame: ID, X, Y, Distance_to_Last, Within_Half_Max
## 7) Save CSV
## Extra credit:
##  - Filled points; latest point colored red
##  - Distance labels rounded for readability
##  - Segments within half-maximum drawn with different lty
setwd <- ("/Users/macpro/Desktop/Youmin-phd/geospatical/Lab 5a - Controlling Program Flow/output/coords.csv")
euclid <- function(x1, y1, x2, y2) sqrt((x1 - x2)^2 + (y1 - y2)^2)

plot_scene <- function(df) {
  if (nrow(df) == 0L) { plot.new(); return(invisible(NULL)) }
  pad <- if (nrow(df) > 1L) max(diff(range(df$X)), diff(range(df$Y))) * 0.15 else 1
  xlim <- range(df$X); ylim <- range(df$Y)
  xlim <- c(xlim[1] - pad, xlim[2] + pad); ylim <- c(ylim[1] - pad, ylim[2] + pad)
  plot(df$X, df$Y, xlim = xlim, ylim = ylim,
       xlab = "X", ylab = "Y",
       main = "Geo Logger: distances to the latest point",
       pch = 16, col = "gray30", cex = 1.1)    # filled points (extra credit)
  points(df$X[nrow(df)], df$Y[nrow(df)], pch = 16, col = "red", cex = 1.3)  # last point in red (extra)
}

add_labels_and_segments <- function(df) {
  lastX <- df$X[nrow(df)]; lastY <- df$Y[nrow(df)]
  d <- euclid(df$X, df$Y, lastX, lastY)
  half_max <- 0.5 * max(d)
  text(df$X, df$Y, labels = format(round(d, 3), nsmall = 3),
       pos = 4, cex = 0.85, col = "black")  # rounded labels (extra)
  if (nrow(df) >= 2L) {
    for (i in seq_len(nrow(df) - 1L)) {
      lty_i <- if (d[i] < half_max) 2 else 1  # dashed if within half-max (extra)
      segments(df$X[i], df$Y[i], lastX, lastY, lty = lty_i)
    }
  }
  invisible(d)
}

## ---------- Main function --------------------------------------
geo_logger <- function(coords_csv = "coords.csv", save_csv = "geo_log.csv", pause_demo = 0.25) {
  log_df <- data.frame(ID = integer(0), X = numeric(0), Y = numeric(0))
  demo <- file.exists(coords_csv)
  demo_in <- NULL
  if (demo) {
    demo_in <- tryCatch({
      rd <- read.csv(coords_csv, header = TRUE)
      if (!all(c("X","Y") %in% names(rd))) stop("coords.csv must have columns X,Y")
      rd
    }, error = function(e) { message("Demo file error: ", e$message); NULL })
    demo <- !is.null(demo_in)
    if (demo) message("Demo mode: reading points from ", coords_csv)
  }

  if (demo) {
    for (k in seq_len(nrow(demo_in))) {
      x_in <- as.numeric(demo_in$X[k]); y_in <- as.numeric(demo_in$Y[k])
      log_df <- rbind(log_df, data.frame(ID = nrow(log_df) + 1L, X = x_in, Y = y_in))
      .plot_scene(log_df)
      d <- .add_labels_and_segments(log_df)
      if (pause_demo > 0) Sys.sleep(pause_demo)
    }
  } else {
    repeat {
      x_raw <- readline("Enter X (or 'quit' to finish): ")
      if (tolower(trimws(x_raw)) == "quit") break
      x_val <- suppressWarnings(as.numeric(x_raw))
      if (!is.finite(x_val)) { cat("  X must be numeric (or 'quit'). Try again.\n"); next }
      y_raw <- readline("Enter Y: ")
      y_val <- suppressWarnings(as.numeric(y_raw))
      if (!is.finite(y_val)) { cat("  Y must be numeric. Try again.\n"); next }
      log_df <- rbind(log_df, data.frame(ID = nrow(log_df) + 1L, X = x_val, Y = y_val))
      .plot_scene(log_df)
      d <- .add_labels_and_segments(log_df)
    }
  }

  if (nrow(log_df) == 0L) { cat("\nNo points entered. Nothing to save.\n"); return(invisible(NULL)) }

  lastX <- log_df$X[nrow(log_df)]; lastY <- log_df$Y[nrow(log_df)]
  d <- euclid(log_df$X, log_df$Y, lastX, lastY)
  d_mean <- mean(d); d_var <- var(d); d_min <- min(d); d_max <- max(d); d_med <- median(d)
  cat(paste0("\nDistances to last -> mean:", round(d_mean,4),
             "  var:", round(d_var,4),
             "  min:", round(d_min,4),
             "  max:", round(d_max,4),
             "  median:", round(d_med,4), "\n"))
  half_max <- 0.5 * d_max
  within_half <- d < half_max
  out <- data.frame(ID = log_df$ID, X = log_df$X, Y = log_df$Y,
                    Distance_to_Last = d, Within_Half_Max = within_half)
  write.csv(out, save_csv, row.names = FALSE)
  cat("Saved CSV -> ", save_csv, "\n", sep = "")
  invisible(out)
}

### uses coords.csv if present; otherwise interactive
source("geo_logger.R")
geo_logger()             # uses coords.csv if found; else interactive
# or specify a different input/output:
geo_logger(coords_csv = "coords.csv", save_csv = "geo_log.csv")
