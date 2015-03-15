all: testA testB testC testD testE doc pdflatex clean
doc: docA docB docC docD docE
tarefaA: docA testA clean
tarefaB: docB testB clean
tarefaC: docC testC clean
tarefaD: docD testD clean
tarefaE: docE testE clean

srcA:
	ghc src/tarefaA.hs
srcB:
	ghc src/tarefaB.hs
srcC:
	ghc src/tarefaC.hs
srcD:
	ghc src/tarefaD.hs
srcE:
	ghc src/tarefaE.hs

docA: 
	haddock -h -o doc/tarefaA src/tarefaA.hs
docB:
	haddock -h -o doc/tarefaB src/tarefaB.hs
docC:
	haddock -h -o doc/tarefaC src/tarefaC.hs
docD:
	haddock -h -o doc/tarefaD src/tarefaD.hs
docE:
	haddock -h -o doc/tarefaE src/tarefaE.hs

testA: srcA
	cd tests/tarefaA; bash runtests.sh teste ../../src/tarefaA
	rm -f tests/tarefaA/*.res

testB: srcB
	cd tests/tarefaB; bash runtests.sh teste ../../src/tarefaB
	rm -f tests/tarefaB/*.res

testC: srcC
	cd tests/tarefaC; bash runtests.sh teste ../../src/tarefaC
	rm -f tests/tarefaC/*.res

testD: srcD
	mkdir tarefaD_results; cd tests/tarefaD; bash runtests.sh teste ../../src/tarefaD

testE: srcE
	mkdir tarefaE_results; cd src; bash ../tests/tarefaE/runtests.sh teste ./tarefaE



pdflatex:
	cd tex; pdflatex relatorio.tex; pdflatex relatorio.tex;

clean:	
	rm -f src/*.hi
	rm -f src/*.o
	rm -f tex/*.aux
	rm -f tex/*.log
	rm -f tex/*.out
	rm -f tex/*.toc
	rm -f tex/*.lof

realclean: clean
	rm -rf doc/tarefaA
	rm -rf doc/tarefaB
	rm -rf doc/tarefaC
	rm -rf doc/tarefaD
	rm -rf doc/tarefaE
	rm -rf tarefaE_results
	rm -rf tarefaD_results
	rm -f tex/relatorio.pdf
	rm -f src/tarefaA
	rm -f src/tarefaB
	rm -f src/tarefaC
	rm -f src/tarefaD
	rm -f src/tarefaE
	rm -f tests/tarefaD/*.out
