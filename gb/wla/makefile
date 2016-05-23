pong.gb: pong.o
	wlalink -v -d linkfile.txt pong.gb

pong.o: pong.z80
	wla-gb -o pong.o pong.z80

clean:
	rm -f pong.o pong.gb
