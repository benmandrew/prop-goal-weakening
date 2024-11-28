lattice: | out-dir
	dune exec lattice
	dot -Tpng -Gdpi=200 -o'out/out.png' 'out/lattice.gv'

out-dir:
	mkdir -p out
