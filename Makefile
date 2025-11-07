.PHONY: gen-opcodes build test clean

gen-opcodes:
	@opam exec -- dune build tools/gen_opcodes.exe
	@opam exec -- dune exec tools/gen_opcodes.exe -- lib/codegen/instr.ml runtime/src/opcode.hpp

build: gen-opcodes
	@opam exec -- dune build
	@xmake build

test:
	@opam exec -- dune test

clean:
	@opam exec -- dune clean
	@xmake clean
