all:
	gprbuild -p -Pharness
clean:
	-gprclean -Pharness
	-gprclean -Palog/alog
	rm -rf obj alog/obj alog/lib
	