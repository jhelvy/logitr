# ============================================================================
# GPU spike: is a torch/GPU backend worth building for logitr's mixed logit?
#
# This is a STANDALONE benchmark of the core computation only (the forward
# simulated log-likelihood). It does NOT use logitr and does NOT need the R
# package installed anywhere. Its only purpose is to measure how much faster
# this specific computation runs on a GPU versus multithreaded CPU.
#
# Run it on:
#   - your Mac:   python bench/gpu_spike.py        (uses the MPS/Metal GPU)
#   - Colab:      paste into a cell and run         (uses the NVIDIA CUDA GPU)
#
# Reads:
#   torch-CPU per-eval  -> compare this to logitr's cpp backend per-eval as a
#                          sanity anchor (they should be in the same ballpark;
#                          both use all CPU cores). If torch-CPU is much slower
#                          than logitr, the GPU/CPU ratio here OVERSTATES the
#                          real-world gain over logitr's backend.
#   GPU per-eval        -> the number that matters.
#   speedup             -> GPU vs torch-CPU. This is the honest upper bound on
#                          what a torch backend could buy over the current one.
# ============================================================================

import time
import torch

def make_data(n_obs, n_alt, n_panel, K, R, device, seed=1):
    g = torch.Generator().manual_seed(seed)
    row_x = n_obs * (n_alt - 1)               # differenced design rows
    X = torch.randn(row_x, K, generator=g)
    draws = torch.randn(R, K, generator=g)    # standard normal draws
    obsID = torch.arange(n_obs).repeat_interleave(n_alt - 1)          # row -> obs
    panelID = (torch.arange(n_obs) * n_panel) // n_obs                # obs -> panel
    weights = torch.ones(n_panel)
    mean = torch.full((K,), 0.1)
    sd = torch.full((K,), 0.3)
    # Move the constants that stay resident on the device
    return dict(
        X=X.to(device), draws=draws.to(device), obsID=obsID.to(device),
        panelID=panelID.to(device), weights=weights.to(device),
        mean=mean.to(device), sd=sd.to(device),
        n_obs=n_obs, n_panel=n_panel, R=R)

def neg_loglik(d):
    # betaDraws = mean + draws * sd  -> (R, K)
    beta = d["mean"] + d["draws"] * d["sd"]
    V = d["X"] @ beta.T                        # (rowX, R)  -- the "skinny" matmul
    eV = torch.exp(V)
    sumExp = torch.zeros(d["n_obs"], d["R"], device=V.device)
    sumExp.index_add_(0, d["obsID"], eV)       # segment sum over obs
    logit = 1.0 / (1.0 + sumExp)               # (n_obs, R)
    # Panel product across obs within each panel (in log space)
    logL = torch.zeros(d["n_panel"], d["R"], device=V.device)
    logL.index_add_(0, d["panelID"], torch.log(logit))
    pHat = torch.exp(logL).mean(dim=1)         # (n_panel,)
    return -(d["weights"] * torch.log(pHat)).sum()

def sync(device):
    if device.type == "cuda":
        torch.cuda.synchronize()
    elif device.type == "mps":
        torch.mps.synchronize()

def time_evals(d, device, reps=20):
    neg_loglik(d); sync(device)                # warm up
    t0 = time.perf_counter()
    for _ in range(reps):
        neg_loglik(d)
    sync(device)
    return (time.perf_counter() - t0) / reps

def pick_gpu():
    if torch.cuda.is_available():
        return torch.device("cuda")
    if getattr(torch.backends, "mps", None) and torch.backends.mps.is_available():
        return torch.device("mps")
    return None

def run(label, n_obs, n_alt, n_panel, K, R):
    cpu = torch.device("cpu")
    gpu = pick_gpu()
    print(f"\n=== {label}: n_obs={n_obs}, n_alt={n_alt}, K={K}, draws={R} ===")
    print(f"    torch threads (CPU): {torch.get_num_threads()}")
    t_cpu = time_evals(make_data(n_obs, n_alt, n_panel, K, R, cpu), cpu)
    print(f"    torch-CPU : {t_cpu*1000:8.2f} ms / eval")
    if gpu is None:
        print("    GPU       : not available")
        return
    t_gpu = time_evals(make_data(n_obs, n_alt, n_panel, K, R, gpu), gpu)
    print(f"    {gpu.type.upper():9s} : {t_gpu*1000:8.2f} ms / eval   "
          f"({t_cpu/t_gpu:.1f}x vs torch-CPU)")

if __name__ == "__main__":
    print("torch", torch.__version__, "| CUDA:", torch.cuda.is_available(),
          "| MPS:", getattr(torch.backends, "mps", None) and torch.backends.mps.is_available())
    # (1) Roughly the yogurt model you benchmarked (small dataset, many draws)
    run("yogurt-scale", n_obs=2412, n_alt=4, n_panel=100, K=5, R=10000)
    # (2) A larger dataset -- where a GPU has the best chance to shine. Sizes are
    # kept so the main matrices (~rowX x R) stay near ~1 GB; bump R or n_obs up
    # if your machine has the memory (the "yogurt-scale" case is the realistic
    # one for most choice models anyway).
    run("large-scale",  n_obs=20000, n_alt=4, n_panel=2000, K=8, R=2000)
