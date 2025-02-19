.PHONY: bench_c bench_cuda bench_opencl bench_mc bench_opencl clean fmt


define bench_wrapper
	echo "Running benchs... with backend=$(1)"
	futhark bench --backend=$(1) --concurrency=4 *_bench.fut
	echo "benchs completed."
endef

bench_c: 
	@$(call bench_wrapper,c)

bench_cuda: 
	@$(call bench_wrapper,cuda)

bench_mc: 
	@$(call bench_wrapper,multicore)

bench_opencl:
	@$(call bench_wrapper,opencl)

clean:
	rm -f *.c
	find . -maxdepth 1 -type f -executable | xargs rm

fmt : 
	futhark fmt *.fut