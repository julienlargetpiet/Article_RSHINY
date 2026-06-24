#library("bench")
#
#print(bench::mark(
#    base = mean(1:1000000),
#    manual = sum(1:1000000) / 1000000,
#    iterations = 20,
#    check = TRUE
#), width = Inf, n = Inf)


library("microbenchmark")

result <- microbenchmark(
  base   = mean(1:1000000),
  manual = sum(1:1000000) / 1000000,
  times  = 20L,
  check  = "equal"
)

print(result)
