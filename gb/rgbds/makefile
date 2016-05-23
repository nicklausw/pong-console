pong.gb: pong.o
	rgblink -t -o pong.gb pong.o
	rgbfix -v -t "PONG NICKLAUSW" pong.gb

pong.o: pong.z80
	rgbasm -o pong.o pong.z80

clean:
	rm -f pong.o pong.gb
