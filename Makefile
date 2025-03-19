.PHONY: all doc install clean odig
all:
	dune b -p ostap $(DUNE_OPTIONS)

doc:
	dune build @doc $(DUNE_OPTIONS)

install:
	dune b @install $(DUNE_OPTIONS)
	dune install ostap

clean:
	@dune clean
	@$(RM) sample/*.sty sample/*.log sample/*.synctex.gz sample/*_latexmk sample/*.fls sample/*.bbl sample/*.aux

ODIG_SWITCHES = --odoc-theme=odig.gruvbox.light
ODIG_SWITCHES += --no-tag-index
ODIG_SWITCHES += --no-pkg-deps
odig:
	odig odoc $(ODIG_SWITCHES) ostap
