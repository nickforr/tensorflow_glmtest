
#env LD_PRELOAD=/usr/local/cuda-9.1/targets/x86_64-linux/lib/libnvblas.so Rscript vita_logreg.R

system.time(matrix(rnorm(4096*4096), nrow=4096, ncol=4096) %*% matrix(rnorm(4096*4096), nrow=4096, ncol=4096))
