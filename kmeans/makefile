# Check if futhark is installed
FUTHARK := $(shell command -v futhark 2> /dev/null)

# Default target
all: install

# Install target - compile kmeans.fut
install:
ifndef FUTHARK
	$(error "futhark compiler not found. Please install futhark first.")
endif
	futhark opencl kmeans.fut -o kmeans
	@echo "Compilation complete. Binary created as 'kmeans'"

# Clean target
clean:
	rm -f kmeans
	rm -f *.c
	rm -f *.h
	rm -f *.o

# Declare phony targets
.PHONY: all install clean