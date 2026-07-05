# ============================================================================
# Regenerates the three static benchmark figures shown in
# vignettes/benchmark.Rmd from the shipped data frames:
#
#   vignettes/benchmark.png        <- data/runtimes.rda       (head-to-head)
#   vignettes/benchmark_draws.png  <- data/runtimes_draws.rda (large-draw scaling)
#   vignettes/benchmark_loglik.png <- data/loglik_draws.rda   (logLik stability)
#
# The vignette does NOT regenerate these when it renders -- its plotting
# chunks are eval=FALSE and the figures are included as static PNGs -- so
# after rerunning data-raw/runtimes.R, run this script to refresh them:
#     Rscript data-raw/figures.R
#
# This is the same plotting code shown in the vignette. The data is loaded
# from data/*.rda directly (not the installed package), so a stale installed
# copy of logitr can't produce stale figures.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
})

load("data/runtimes.rda")
load("data/runtimes_draws.rda")
load("data/loglik_draws.rda")

# n shades of a base color (dark -> light), derived from the base color itself
shades <- function(base, n = 3) {
  v <- grDevices::col2rgb(base)[, 1] / 255
  dark <- grDevices::rgb(v[1] * .55, v[2] * .55, v[3] * .55)
  light <- grDevices::rgb(
    v[1] + (1 - v[1]) * .5,
    v[2] + (1 - v[2]) * .5,
    v[3] + (1 - v[3]) * .5
  )
  grDevices::colorRampPalette(c(dark, base, light))(n)
}

# ---- Figure 1: head-to-head across packages (benchmark.png) ----------------

# Assign each package a base color; packages benchmarked at several core counts
# (logitr, apollo, mixl) get a dark -> light ramp (dark = fewest cores). Built
# from the data so it adapts to whatever core counts this machine produced.
baseColor <- c(
  logitr = "#4DAF4A",
  apollo = "#E41A1C",
  mixl = "#377EB8",
  mlogit = "#984EA3",
  gmnl = "gold"
)

lv <- character(0)
plotColors <- character(0)
for (pkg in names(baseColor)) {
  labs <- runtimes %>%
    filter(sub(" .*", "", as.character(package)) == pkg) %>%
    distinct(package) %>%
    pull(package) %>%
    as.character()
  nCores <- as.integer(sub(".*\\(([0-9]+) cores\\)", "\\1", labs))
  labs <- labs[order(nCores, na.last = TRUE)]
  cols <- if (length(labs) == 1) {
    baseColor[[pkg]]
  } else {
    shades(baseColor[[pkg]], length(labs))
  }
  lv <- c(lv, labs)
  plotColors <- c(plotColors, cols)
}

benchmark <- runtimes %>%
  mutate(package = factor(as.character(package), levels = lv)) %>%
  ggplot(aes(x = numDraws, y = time_sec, color = package)) +
  geom_line() +
  geom_point(size = 1.5) +
  geom_text_repel(
    data = . %>% filter(numDraws == max(numDraws)),
    aes(label = package),
    hjust = 0,
    nudge_x = 80,
    direction = "y",
    size = 3.3,
    segment.size = 0.2,
    segment.color = "grey70",
    seed = 42
  ) +
  scale_x_continuous(
    breaks = sort(unique(runtimes$numDraws)),
    labels = scales::comma,
    expand = expansion(mult = c(0.05, 0.25))
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, 50),
    limits = c(0, 200)
  ) +
  scale_color_manual(values = plotColors) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Number of random draws",
    y = "Computation time (seconds)"
  )

ggsave("vignettes/benchmark.png", benchmark, width = 9, height = 5.5, dpi = 300)
cat("Saved vignettes/benchmark.png\n")

# ---- Figure 2: logitr-only large-draw scaling (benchmark_draws.png) --------

draws_plot <- runtimes_draws %>%
  ggplot(aes(x = numDraws, y = time_sec, color = config)) +
  geom_line() +
  geom_point(size = 1.5) +
  geom_text_repel(
    data = . %>% filter(numDraws == max(numDraws)),
    aes(label = config),
    hjust = 0,
    nudge_x = 0.05,
    direction = "y",
    size = 3.3,
    segment.size = 0.2,
    segment.color = "grey70",
    seed = 42
  ) +
  scale_x_log10(
    breaks = c(100, 500, 1000, 2500, 5000, 10000),
    labels = scales::comma,
    expand = expansion(mult = c(0.02, 0.25))
  ) +
  scale_color_manual(values = shades("#4DAF4A")) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Number of random draws (log scale)",
    y = "Computation time (seconds)"
  )

ggsave(
  "vignettes/benchmark_draws.png",
  draws_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)
cat("Saved vignettes/benchmark_draws.png\n")

# ---- Figure 3: logLik stability across seeds (benchmark_loglik.png) --------

loglik_plot <- loglik_draws %>%
  ggplot(aes(x = numDraws, y = logLik)) +
  geom_point(color = "#4DAF4A", alpha = 0.6, size = 1.8) +
  stat_summary(fun = mean, geom = "line", color = "#4DAF4A") +
  scale_x_log10(
    breaks = c(50, 100, 250, 500, 1000, 5000),
    labels = scales::comma
  ) +
  annotate(
    "curve",
    x = 1400,
    y = -741.5,
    xend = 530,
    yend = -735,
    curvature = -0.25,
    linewidth = 0.4,
    color = "grey30",
    arrow = arrow(length = unit(0.02, "npc"))
  ) +
  annotate(
    "text",
    x = 2400,
    y = -742,
    label = "logitr's new default\n(numDraws = 500)",
    size = 4.5,
    color = "grey30",
    lineheight = 0.95
  ) +
  theme_bw(base_size = 15) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    x = "Number of random draws (log scale)",
    y = "Converged log-likelihood"
  )

ggsave(
  "vignettes/benchmark_loglik.png",
  loglik_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)
cat("Saved vignettes/benchmark_loglik.png\n")
