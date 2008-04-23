paths = \
graph \
http \
types.scm \
packages.scm \
utility \


files = $(shell find $(paths) -name '*.sc[hm]' -o -name '*.c')
CLIB_DIR=c-lib

all: TAGS bin/ykk-development.img $(CLIB_DIR)/dates.so

TAGS: $(files)
	etags $(files)

bin/ykk-development.img: bin/ykk-development.s48 bin/cached-packages.s48 \
			 ssax-5.1/packages.scm packages.scm
	{ cat bin/ykk-development.s48 bin/cached-packages.s48; echo ",dump $@"; } | scheme48 -a batch

$(CLIB_DIR)/dates.so: $(CLIB_DIR)/dates.c
	MACOS_DEPLOYMENT_TARGET='10.3' && export MACOS_DEPLOYMENT_TARGET
	cd $(CLIB_DIR) && gcc -bundle -undefined dynamic_lookup -I/usr/local/scheme48/current/c -o dates.so dates.c
