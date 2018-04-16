all:
	$(MAKE) -C src # FIXME also installs and builds bin/ currently 

clean:
	$(MAKE) -C src clean
	$(MAKE) -C bin clean
