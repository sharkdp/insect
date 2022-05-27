#!/usr/bin/sh

benchmark_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$benchmark_dir"

# Check that Hyperfine is installed.
if ! command -v hyperfine > /dev/null 2>&1; then
	echo "'hyperfine' does not seem to be installed."
	echo "You can get it here: https://github.com/sharkdp/hyperfine"
	exit 1
fi

rm -rf results/
mkdir results/

hyperfine \
    --warmup=5 \
    --time-unit=millisecond \
    --export-json results/startup.json \
    --export-markdown results/startup.md \
    --command-name "Startup time (insect '1+1')" \
    "node ../index.js '1+1'"

hyperfine \
    --warmup=5 \
    --time-unit=millisecond \
    --export-json results/startup-interactive.json \
    --export-markdown results/startup-interactive.md \
    --command-name "Startup time, interactive mode (insect < computations-1.ins)" \
    "node ../index.js < computations-1.ins"

hyperfine \
    --warmup=2 \
    --time-unit=millisecond \
    --export-json results/evaluation.json \
    --export-markdown results/evaluation.md \
    --command-name "Evaluation speed (insect < computations-160.ins)" \
    "node ../index.js < computations-160.ins"

for benchmark in startup startup-repl evaluation; do
    cat results/$benchmark.md >> results/report.md
    echo >> results/report.md
done
cat results/report.md
