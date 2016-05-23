pong.nes: pong.o
	ld65 -C nes_nrom.cfg -o pong.nes pong.o

pong.o: pong.s
	ca65 pong.s

clean:
	rm -f pong.o pong.nes
