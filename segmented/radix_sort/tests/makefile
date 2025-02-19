.PHONY: test_c test_cuda test_opencl test_mc test_opencl clean fmt


define test_wrapper
	echo "Running tests... with backend=$(1)"
	futhark test --backend=$(1) *_tests.fut
	echo "Tests completed."
endef

test_c: 
	@$(call test_wrapper,c)

test_cuda: 
	@$(call test_wrapper,cuda)

test_mc: 
	@$(call test_wrapper,multicore)

test_opencl:
	@$(call test_wrapper,opencl)

clean:
	rm -f *.c
	rm -f *.actual
	rm -f *.expected
	find . -maxdepth 1 -type f -executable | xargs rm

fmt : 
	futhark fmt *.fut