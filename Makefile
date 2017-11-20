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
	jbuilder runtest test
