build:
	$(MAKE) -C src # FIXME also installs and builds bin/ currently 

install:
	$(MAKE) -C src install

uninstall:
	$(MAKE) -C src uninstall

clean:
	$(MAKE) -C src clean
	$(MAKE) -C bin clean
