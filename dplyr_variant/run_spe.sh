
rm -rf ../results/dplyr_fread_results
mkdir -p ../results/dplyr_fread_results

PORT=7665
URL="http://127.0.0.1:${PORT}"

export BENCH_GLOBAL="speed_global.R"
export BENCH_INGESTION="fread_ing.R"
export BENCH_VARIANT="../results/dplyr_fread_results"

for i in $(seq 1 20); do
  echo
  echo "================================"
  echo "Running DPLYR - FREAD Speed Benchmark for log file $i"
  echo "================================"

  profile_dir="$(mktemp -d)"

  BENCH_INDEX="$i" Rscript -e "shiny::runApp('.', host='127.0.0.1', port=${PORT}, launch.browser=FALSE)" &
  r_pid=$!

  echo "Waiting for Shiny server..."

  until nc -z 127.0.0.1 "$PORT"; do
    sleep 0.2
  done

  echo "Opening browser..."

  firefox \
    -no-remote \
    -profile "$profile_dir" \
    --new-window "$URL" >/dev/null 2>&1 &

  firefox_pid=$!

  wait "$r_pid"

  echo "Shiny stopped for BENCH_INDEX=$i"

  kill "$firefox_pid" >/dev/null 2>&1 || true
  rm -rf "$profile_dir"

  sleep 1
done

echo
echo "All Speed Benchmarks finished."

#rm -rf ../results/dplyr_mem_vroom_results
#mkdir -p ../results/dplyr_mem_vroom_results
#
#PORT=7665
#URL="http://127.0.0.1:${PORT}"
#
#export BENCH_GLOBAL="mem_global.R"
#export BENCH_VARIANT="../results/dplyr_mem_vroom_results"
#
#for i in $(seq 1 20); do
#  echo
#  echo "================================"
#  echo "Running DPLYR - VROOM Mem Benchmark for log file $i"
#  echo "================================"
#
#  profile_dir="$(mktemp -d)"
#
#  BENCH_INDEX="$i" Rscript -e "shiny::runApp('.', host='127.0.0.1', port=${PORT}, launch.browser=FALSE)" &
#  r_pid=$!
#
#  echo "Waiting for Shiny server..."
#
#  until nc -z 127.0.0.1 "$PORT"; do
#    sleep 0.2
#  done
#
#  echo "Opening browser..."
#
#  firefox \
#    -no-remote \
#    -profile "$profile_dir" \
#    --new-window "$URL" >/dev/null 2>&1 &
#
#  firefox_pid=$!
#
#  wait "$r_pid"
#
#  echo "Shiny stopped for BENCH_INDEX=$i"
#
#  kill "$firefox_pid" >/dev/null 2>&1 || true
#  rm -rf "$profile_dir"
#
#  sleep 1
#done
#
#echo
#echo "All Speed Benchmarks finished."




