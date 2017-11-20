.PHONY: all
all:
	jbuilder build

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
	OPAMROOT=~/fyp/opam-root opam switch 4.05.0+fyp
	OPAMROOT=~/fyp/opam-root jbuilder runtest test
