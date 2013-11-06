all:
	hsc2hs System/NanoMsg/C/NanoMsgStruct.hsc ; c2hs System/NanoMsg/C/NanoMsg.chs ; ghc -threaded -lnanomsg -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d/ --make  ./main.hs ./System/NanoMsg/C/inlinemacro.c 
tests:
	ghc -lnanomsg -threaded -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d/ --make ./test/main.hs
clean:
	rm -f hpmd
	rm -f test/runTest
	rm -f */*.hi
	rm -f *.o
	rm -f *.hi
	rm -f */*.o
all2:
	gcc -O3 -c System/NanoMsg/C/inlinemacro.c -o System/NanoMsg/C/inlinemacro.o ; c2hs System/NanoMsg/C/NanoMsg.chs ; ghc -lnanomsg -o System/NanoMsg/C/inlinemacro.o -threaded -package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d/ --make  ./main.hs
