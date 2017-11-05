LIBS=-I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/bitlbee/

all:
	stack exec hsc2hs -- src/Bitlbee.hsc $(LIBS)
	stack ghc -- -no-hs-main -optc-O -O2 -dynamic -shared -fPIC -o 4chan-bitlbee.so src/Lib.hs src/Bitlbee.hs src/Chan.hs c-lib.c -lHSrts-ghc8.0.2 -stubdir stub $(LIBS)

test:
	stack ghc -- -g -no-hs-main prueba.c -o salida -ldl

install:
	cp 4chan-bitlbee.so /usr/lib/bitlbee/4chan-bitlbee.so
