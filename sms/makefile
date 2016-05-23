pong.sms: pong.o
	wlalink -v -d linkfile.txt pong.sms

pong.o: pong.asm
	wla-z80 -o pong.o pong.asm

clean:
	rm -f pong.o pong.sms
