# Parse the console log of accel_benchmark.R into a tidy results table.
# (Needed because the sweep was interrupted before its final CSV write.)
lines <- readLines("bench/results/full_sweep.log", warn = FALSE)
# Keep only fully-formed result lines (all three fields present)
pat <- "full-eval=([0-9.]+)s\\s+grad share=([0-9]+)%\\s+fit=([0-9.]+)s"
lines <- lines[grepl(pat, lines)]
m <- regmatches(lines, regexec(pat, lines))
eval_s   <- as.numeric(vapply(m, `[`, "", 2))
grad_pct <- as.numeric(vapply(m, `[`, "", 3))
fit_s    <- as.numeric(vapply(m, `[`, "", 4))

# Reconstruct the sweep grid in the exact order accel_benchmark.R used
grid <- expand.grid(
  nResp = c(300, 1000), nAlt = c(3, 5), nRand = c(3, 6),
  draws = c(100, 500, 2000), corr = c(FALSE, TRUE)
)
n <- length(eval_s)
df <- cbind(grid[seq_len(n), ], eval_s, grad_pct, fit_s)
df$rowX <- df$nResp * 10 * (df$nAlt - 1)
df$matmul_elems <- df$rowX * df$draws

write.csv(df, "bench/results/sweep_parsed.csv", row.names = FALSE)

cat("configs parsed:", n, "of 48\n\n")
cat("== grad-loop share of each LL+grad eval, by nRand ==\n")
print(aggregate(grad_pct ~ nRand, df, function(x) round(mean(x))))
cat("\n== mean per-eval(s), fit(s), grad% by draw count ==\n")
print(aggregate(cbind(eval_s, fit_s, grad_pct) ~ draws, df,
                function(x) round(mean(x), 2)))
cat("\n== largest cell (nResp=1000, nAlt=5, nRand=6): scaling in draws ==\n")
sel <- df$nResp == 1000 & df$nAlt == 5 & df$nRand == 6
print(df[sel, c("draws", "corr", "rowX", "eval_s", "grad_pct", "fit_s")],
      row.names = FALSE)
