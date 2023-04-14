source("utils.R")
source("reference-implementation.R")

knitr::opts_chunk$set(echo = FALSE, fig.align = "center", fig.pos = "H", fig.height = 3)
options(knitr.kable.NA = '-', scipen = 999)
theme_set(theme_bw())

# Functions --------------------------------------------------------------------

kish <- function(w) sum(w)^2 / sum(w^2)
kish_latex <- function(w) {
  result <- kish(w)
  sign <- if (result != round(result, 6)) "\\approx" else "="
  paste0("n^*(\\{", paste(w, collapse = ", "), "\\}) ", sign, " ", round(result, 6))
}
list_latex <- function(x) paste(x, collapse = ", ")

ex_hd_intro <- function() {
  x <- c(1, 2, 4, 8, 16)
  n <- length(x)
  p <- 0.5
  a <- 3
  b <- 3
  t <- seq(0, 1, by = 0.2)
  cdf_values <- pbeta(t, a, b)
  W <- tail(cdf_values, -1) - head(cdf_values, -1)
  est <- hdquantile(x, p)
  list(x = x, n = n, p = p, a = a, b = b, t = t, W = W, est = est)
}

ex_whd1 <- function() {
  x <- 1:5
  w <- c(1, 1, 0, 0, 1)
  p <- 0.5
  nw <- sum(w)^2 / sum(w^2)
  a <- (nw + 1) * p
  b <- (nw + 1) * (1 - p)
  t <- cumsum(c(0, w / sum(w)))
  cdf_values <- pbeta(t, a, b)
  W <- tail(cdf_values, -1) - head(cdf_values, -1)
  est <- sum(W * x)
  list(x = x, w = w, p = p, nw = nw, a = a, b = b, t = t, W = W, est = est)
}

ex_whd2 <- function() {
  x <- 1:5
  w <- c(0.4, 0.4, 0.05, 0.05, 0.1)
  p <- 0.5
  nw <- sum(w)^2 / sum(w^2)
  a <- (nw + 1) * p
  b <- (nw + 1) * (1 - p)
  t <- cumsum(c(0, w / sum(w)))
  cdf_values <- pbeta(t, a, b)
  W <- tail(cdf_values, -1) - head(cdf_values, -1)
  est <- sum(W * x)
  list(x = x, w = w, p = p, nw = nw, a = a, b = b, t = t, W = W, est = est)
}

ex_wthd <- function() {
  x <- c(1, 2, 3, 10000)
  w <- c(0.1, 0.4, 0.4, 0.1)
  p <- 0.5
  nw <- sum(w)^2 / sum(w^2)
  a <- (nw + 1) * p
  b <- (nw + 1) * (1 - p)
  t <- cumsum(c(0, w / sum(w)))
  cdf_values <- pbeta(t, a, b)
  W <- tail(cdf_values, -1) - head(cdf_values, -1)
  est1 <- sum(W * x)
  est2 <- sum(W[2:3] * x[2:3]) / sum(W[2:3])
  D <- 1 / sqrt(nw)
  L <- 0.5 - D / 2
  R <- 0.5 + D / 2
  list(x = x, w = w, p = p, nw = nw, a = a, b = b, t = t, W = W, est1 = est1, est2 = est2, D = D, L = L, R = R)
}

draw_hd <- function(n, p, points = NULL, labels = NULL, x_breaks = NULL) {
  a <- p * (n + 1)
  b <- (1 - p) * (n + 1)
  f <- function(q) dbeta(q, a, b)
  if (is.null(points))
    points <- 0:n/n
  n0 <- length(points) - 1
  if (is.null(labels))
    labels <- paste0("W", 1:n0)

  area <- function(x1, x2, fill) {
    x <- c(seq(x1, x2, by = 0.001))
    y <- f(x)
    xp <- c(x1, x, x2)
    yp <- c(0, y, 0)
    geom_polygon(data = data.frame(x = xp, y = yp), aes(x, y), fill = fill, alpha = 0.4, col = NA)
  }
  
  df_pdf <- data.frame(x = seq(0, 1, by = 0.001))
  df_pdf$y <- f(df_pdf$x)

  df_seg <- data.frame(x = points[2:n0])
  df_seg$y <- rep(0, n0 - 1)
  df_seg$yend <- f(df_seg$x)

  df_text <- data.frame(x = (tail(points, -1) + head(points, -1)) / 2)
  df_text$y <- f(df_text$x) / 2
  df_text$text <- labels

  if (is.null(x_breaks))
    x_breaks <- seq(0, 1, by = 0.2)

  plot <- ggplot()
  for (i in 1:n0)
    plot <- plot + area(points[i], points[i + 1], cbp$values[i])
  plot <- plot +
    geom_line(data = df_pdf, aes(x, y)) +
    geom_segment(data = df_seg, aes(x, y, xend = x, yend = yend)) +
    geom_text(data = df_text, aes(x, y, label = text)) +
    scale_x_continuous(breaks = x_breaks) +
    labs(x = "t", y = "Density") +
    theme(axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
  plot
}

draw_hf7 <- function(n, p, points = c(), x_breaks = NULL, x_labels = NULL) {
  h <- p * (n - 1) + 1
  l <- (h - 1) / n
  r <- h / n
  x <- seq(0, 1, by = 0.001)
  f <- Vectorize(function(u) {
    if (u < l || u > r)
      return(0)
    return(n)
  })
  F <- Vectorize(function(u) {
    if (u < l)
      return(0)
    if (u > r)
      return(1)
    return(u * n - h + 1)
  })
  area <- function(x1, x2, func, fill) {
    x <- c(seq(x1, x2, by = 0.001))
    y <- func(x)
    xp <- c(x1, x, x2)
    yp <- c(0, y, 0)
    geom_polygon(data = data.frame(x = xp, y = yp), mapping = aes(x, y), fill = fill, alpha = 0.5, col = NA)
  }
  hat <- function(x1, x2, func, col) {
    x <- c(seq(x1, x2, by = 0.001))
    y <- func(x)
    geom_line(data = data.frame(x, y), mapping = aes(x, y), col = col)
  }
  
  if (is.null(x_breaks))
    x_breaks <- seq(0, 1, by = 0.2)
  if (is.null(x_labels))
    x_labels <- x_breaks
  
  pdf <- ggplot() +
    geom_line(data = data.frame(x = x, y = f(x)), mapping = aes(x, y)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    labs(title = "PDF", x = "t", y = "Density")
  cdf <- ggplot() +
    geom_line(data = data.frame(x = x, y = F(x)), mapping = aes(x, y)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    labs(title = "CDF", x = "t", y = "CDF value")
  
  if (length(points) > 1)
    for (i in 1:(length(points) - 1)) {
      pdf <- pdf + area(points[i], points[i + 1], f, cbp$values[i]) + hat(points[i], points[i + 1], f, cbp$values[i])
      cdf <- cdf + area(points[i], points[i + 1], F, cbp$values[i]) + hat(points[i], points[i + 1], F, cbp$values[i])
    }
  
  ggarrange(pdf, cdf, nrow = 1)
}

# Figures ----------------------------------------------------------------------

figure_hd_intro <- function() draw_hd(5, 0.5)
figure_whd1 <- function() draw_hd(3, 0.5, 0:3/3, c("W1", "W2", "W5"))
figure_whd2 <- function() {
  w <- c(0.4, 0.4, 0.05, 0.05, 0.1)
  draw_hd(kish(w), 0.5, c(0, cumsum(w)))
}
figure_wthd <- function() {
  w <- c(0.1, 0.4, 0.4, 0.1)
  draw_hd(kish(w), 0.5, c(0, cumsum(w)), x_breaks = seq(0, 1, by = 0.1))
}

figure_hf_a <- function() draw_hf7(5, 0.25, c(0.2, 0.4))
figure_hf_b <- function() draw_hf7(5, 0.35, c(0.2, 0.4, 0.6))
figure_hf_c <- function() draw_hf7(3, 0.5, c(0, 3/9, 4/9, 5/9, 1), x_breaks = 0:9/9, x_labels = paste0(0:9, "/9"))

figure_smoothing_problem <- function() {
  set.seed(1729)
  
  n <- 100
  index <- 1:n
  x1 <- c(rnorm(n * 9 / 10, 10), rnorm(n * 1 / 10, 20))
  x2 <- rnorm(n, seq(10, 20, length.out = n))
  
  p1 <- ggplot(data.frame(index, x = x1), aes(index, x)) +
    geom_point(size = 0.5) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, NA)) +
    labs(x = "Time", y = "x", title = "(a) Time series with a change")
  
  p2 <- ggplot(data.frame(index, x = x2), aes(index, x)) +
    geom_point(size = 0.5) +
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, NA)) +
    labs(x = "Time", y = "x", title = "(b) Time series with a trend")
  
  ggarrange(p1, p2, nrow = 1)
}

figure_simulation_median_exponential_smoothing <- function() {
  set.seed(1729)
  n <- 1000
  time <- 1:n
  w <- 2^(-(time - 1) / 10)

  s1 <- c(rnorm(n * 9 / 10, 10), rnorm(n * 1 / 10, 20))
  s2 <- rnorm(n, 10 + (1:n) / 100)
  s3 <- rnorm(n)
  s4 <- rcauchy(n)
  s5 <- ifelse(sample(0:9, n, TRUE) == 0 & 1:n > 20, runif(n, 2, 10), sin(time / 40) * 2 + runif(n)) + 1:n / 100
  s6 <- c(sapply(1:10, function(i) {
    sign <- (i %% 2) * 2 - 1
    base <- i * 0.5
    m <- n / 10
    sign * rnorm(m, base + ((i - 1) %% 100) / 100)
  }))
  
  build <- function(type, s) {
    m <- sapply(1:n, function(i) wquantile(s[1:i], rev(w[1:i]), 0.5))
    l <- sapply(1:n, function(i) wquantile(s[1:i], rev(w[1:i]), 0.25))
    u <- sapply(1:n, function(i) wquantile(s[1:i], rev(w[1:i]), 0.75))
    data.frame(time, s, m, l, u, type)
  }
  
  df <- rbind(
    build("(a)", s1),
    build("(b)", s2),
    build("(c)", s3),
    build("(d)", s4),
    build("(e)", s5),
    build("(f)", s6)
    )
  ggplot(df, aes(time)) +
    geom_point(aes(y = s), size = 0.8) +
    geom_line(aes(y = m), col = cbp$red, linewidth = 1.2) +
    facet_wrap(vars(type), ncol = 2, scales = "free_y") +
    labs(x = "Time (i)", y = "x")
}

figure_simulation_quartile_exponential_smoothing <- function() {
  build <- function(type, x, half_life = 10) {
    n <- length(x)
    time <- 1:n
    w <- 2^(-(time - 1) / half_life)
    p25 <- sapply(1:n, function(i) wquantile(x[1:i], rev(w[1:i]), 0.25))
    p50 <- sapply(1:n, function(i) wquantile(x[1:i], rev(w[1:i]), 0.5))
    p75 <- sapply(1:n, function(i) wquantile(x[1:i], rev(w[1:i]), 0.75))
    data.frame(type, time, x, p25, p50, p75)
  }
  
  set.seed(1729)
  data <- c(
    rnorm(100, 0),
    rnorm(100, 10),
    rnorm(100, 0),
    rnorm(200, sample(c(-10, -10, 0, 10, 10), 200, T))
    )
  df <- rbind(
    build("(a) Half-life =  5", data, 5),
    build("(b) Half-life = 10", data, 10),
    build("(c) Half-life = 30", data, 30)
  )
  dfq <- df[,c("time", "p25", "p50", "p75", "type")] %>% 
    gather("percentile", "value", -time, -type)
  dfq$percentile <- factor(dfq$percentile, levels = c("p75", "p50", "p25"))
  ggplot(df, aes(time)) +
    geom_point(aes(y = x), size = 0.8) +
    geom_line(data = dfq, aes(y = value, col = percentile), linewidth = 1) +
    facet_wrap(vars(type), ncol = 1, scales = "free_y") +
    scale_color_manual(values = cbp$values, labels = c("p=0.75", "p=0.50", "p=0.25")) +
    labs(col = "", x = "Time (i)")
}

figure_simulation_mixture <- function() {
  set.seed(1729)

  clamp <- function(x, a, b) pmin(pmax(x, a), b)
  build <- function(type, qDist, genX, genW) {
    trials <- 1:50
    probs <- seq(0.01, 0.99, by = 0.001)
    qA <- qDist(probs)
    df <- do.call("rbind", lapply(trials, function(trial) {
      x <- genX()
      w <- genW()
      qB <- wquantile(x, w, probs)
      data.frame(type = type, trial = trial, qA, qB, shift = qB - qA, probs = probs)
    }))
    df$trial <- factor(df$trial, levels = trials)
    df
  }
  build_norm_mix2 <- function(type_prefix, mean1, sd1, mean2, sd2, p.mix) {
    n <- 100
    genX <- function() c(rnorm(n, mean1, sd1), rnorm(n, mean2, sd2))
    genW <- function() c(rep(1 - p.mix, n), rep(p.mix, n))
    qDist <- function(probs) qnormMix(probs, mean1, sd1, mean2, sd2, p.mix)
    type <- paste0(type_prefix, " ",
                   1 - p.mix, "*N(", mean1, ", ", sd1, ")", " + ",
                   p.mix, "*N(", mean2, ", ", sd2, ")")
    build(type, qDist, genX, genW)
  }
  build_unif_mix2 <- function(type_prefix, a1, b1, a2, b2, p.mix) {
    n <- 100
    genX <- function() c(runif(n, a1, b1), runif(n, a2, b2))
    genW <- function() c(rep(1 - p.mix, n), rep(p.mix, n))
    qDist <- function(probs) 
      ifelse(probs < 1 - p.mix,
             qunif(clamp(probs / (1 - p.mix), 0, 1), a1, b1),
             qunif(clamp((probs - (1 - p.mix)) / p.mix, 0, 1), a2, b2))
    type <- paste0(type_prefix, " ",
                   1 - p.mix, "*U(", a1, ", ", b1, ")", " + ",
                   p.mix, "*U(", a2, ", ", b2, ")")
    build(type, qDist, genX, genW)
  }
  build_exp_mix3 <- function(type_prefix, r1, r2, r3, p1, p2, p3, s1, s2, s3) {
    n <- 100
    genX <- function() c(rexp(n, r1) + s1, rexp(n, r2) + s2, rexp(n, r3) + s3)
    genW <- function() c(rep(p1, n), rep(p2, n), rep(p3, n))
    cdf <- function(q) p1 * pexp(q - s1, r1) + p2 * pexp(q - s2, r2) + p3 * pexp(q - s3, r3)
    inverse <- function(f, lower = 0, upper = 100) {
      function(y) uniroot((function(x) f(x) - y), lower = lower, upper = upper)[1]$root
    }
    qDist <- function(probs) sapply(probs, function(p) inverse(cdf)(p))
    type <- paste0(type_prefix, " ",
                   p1, "*Exp(", r1, ", ", s1, ")", " + ",
                   p2, "*Exp(", r2, ", ", s2, ")", " + ",
                   p3, "*Exp(", r3, ", ", s3, ")")
    build(type, qDist, genX, genW)
  }
  
  df <- rbind(
    build_norm_mix2("(a)", 0, 1, 5, 3, 0.25),
    build_norm_mix2("(b)", 0, 1, 100, 10, 0.01),
    build_unif_mix2("(c)", 0, 1, 5, 10, 0.5),
    build_unif_mix2("(d)", 0, 1, 20, 30, 0.9),
    build_exp_mix3("(e)", 1, 2, 3, 0.7, 0.2, 0.1, 0, 0, 0),
    build_exp_mix3("(f)", 1, 1, 1, 0.3, 0.3, 0.4, 0, 10, 20)
  )

  p1 <-
    ggplot(df, aes(x = probs)) +
    geom_line(aes(y = qB, col = trial)) +
    geom_line(aes(y = qA), col = "black") +
    facet_wrap(vars(type), scales = "free", ncol = 2) +
    theme(legend.position = "none") +
    labs(
      title = "Quantile plots",
      x = "p",
      y = "Quantile"
    )
  
  p2 <-
    ggplot(df, aes(x = qA, y = qB, col = trial)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, col = "black") +
    facet_wrap(vars(type), scales = "free", ncol = 2) +
    theme(legend.position = "none") +
    labs(
      title = "Q-Q plots",
      x = "Theoretical quantiles",
      y = "Estimated quantiles")
  
  p3 <-
    ggplot(df, aes(x = probs)) +
    geom_line(aes(y = shift, col = trial)) +
    geom_hline(yintercept = 0, col = "black") +
    facet_wrap(vars(type), scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    theme(legend.position = "none") +
    labs(
      title = "Shift plots",
      x = "p",
      y = "Shift"
    )

  list(p1, p2, p3)
}