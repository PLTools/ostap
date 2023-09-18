.PHONY: all doc install clean
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
