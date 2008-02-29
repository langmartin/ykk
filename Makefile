paths = \
http \
persisted-graph.scm \
prim \
repl.scm \
type-destructuring.scm \
type-structure-parser.scm \
types.scm \
utility \
zipper \


files = $(shell find $(paths) -name '*.sc[hm]' -o -name '*.c')

TAGS: $(files)
	etags $(files)
