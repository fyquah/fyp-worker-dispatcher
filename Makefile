.PHONY: all
all:
	jbuilder build

.PHONY: tools
tools:
	jbuilder build tools/tree_tools.exe

.PHONY: worker
worker:
	jbuilder build worker/main.exe

.PHONY: controller
controller:
	jbuilder build controller/main.exe

.PHONY: test
test:
	jbuilder build test/main.exe

.PHONY: display
display:
	jbuilder build display/main.exe
	
.PHONY: test
test:
	jbuilder build test/main.exe
	OPAMROOT=~/fyp/opam-root jbuilder runtest test

.PHONY: rsync
rsync:
	./fyp_compiler_lib/rsync.sh
