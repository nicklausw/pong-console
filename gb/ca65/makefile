pong.gb: pong.o
	ld65 -C gb.cfg -o pong.gb pong.o
	rgbfix -v -t "PONG NICKLAUSW" pong.gb

pong.o: pong.z80
	ca65 -o pong.o pong.z80

clean:
	rm -f pong.o pong.gb
