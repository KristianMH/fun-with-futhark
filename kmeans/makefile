# Check if futhark is installed
FUTHARK := $(shell command -v futhark 2> /dev/null)

# Default target
all: install

# Install target - compile knn.fut
install:
ifndef FUTHARK
	$(error "futhark compiler not found. Please install futhark first.")
endif
	futhark opencl knn.fut -o knn
	@echo "Compilation complete. Binary created as 'knn'"

# Clean target
clean:
	rm -f knn
	rm -f *.c
	rm -f *.h
	rm -f *.o

# Declare phony targets
.PHONY: all install clean