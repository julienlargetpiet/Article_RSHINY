library(data.table)

dt <- fread(
  "dataset_origin/out1.log",
  col.names = c("ip", "ts", "target", "status", "ua")
)

fuzz_ip <- function(ip, i) {
  parts <- tstrsplit(ip, ".", fixed = TRUE)

  a <- as.integer(parts[[1]])
  b <- as.integer(parts[[2]])
  c <- as.integer(parts[[3]])
  d <- as.integer(parts[[4]])

  a2 <- ((c + i * 28L) %% 254L) + 1L
  b2 <- ((d + i * 8L) %% 254L) + 1L
  c2 <- ((c + i * 17L) %% 254L) + 1L
  d2 <- ((d + i * 37L) %% 254L) + 1L

  paste(a2, b2, c2, d2, sep = ".")
}

cur_max <- max(dt$ts)

for (i in 2:20) {

  cat(paste("GENERATING:", i, "/ 20\n"))

  copies <- vector("list", i)

  for (i2 in seq_len(i - 1L)) {
    tmp <- copy(dt)
  
    tmp[, ts := ts + i2 * cur_max]
    tmp[, ip := fuzz_ip(ip, i2)]
  
    copies[[i2]] <- tmp
  }
  
  big <- rbindlist(c(list(dt), copies), use.names = TRUE)
  
  fwrite(big, paste0("logs/out", i, ".log"), sep = "\t", col.names = FALSE)

}





