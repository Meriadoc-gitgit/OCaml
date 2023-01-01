export PATH := /usr/local/_opam/bin:$(PATH)

UTOP = /usr/bin/utop

tme:
				which ocamlc
				rm -f main.exe
				dune build && cp ./_build/default/tme5.exe main.exe

utop: tme
				 @$(UTOP) -init ocamlinit.ml

clean:
				rm -rf _build/ main.exe
