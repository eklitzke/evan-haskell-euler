object_files = $(shell find . -name '*.o')
interp_files = $(shell find . -name '*.hi')
exe_files = $(shell find . -perm -o=x -type f)
clean:
	-rm -f $(object_files) $(interp_files) $(exe_files)
