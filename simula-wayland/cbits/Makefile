MOTORCAR_PROTOCOL_XML=motorcar.xml


motorcar-protocol: $(MOTORCAR_PROTOCOL_XML)
	wayland-scanner client-header < $(MOTORCAR_PROTOCOL_XML) > motorcar-client-protocol.h
	wayland-scanner server-header < $(MOTORCAR_PROTOCOL_XML) > motorcar-server-protocol.h
	wayland-scanner code < $(MOTORCAR_PROTOCOL_XML) > motorcar-wayland-extensions.c
	gcc -c -Wall -Werror -fpic motorcar-wayland-extensions.c
	gcc -shared -o libmotorcar-server.so motorcar-wayland-extensions.o

clean:
	rm -rf  ./*.h ./*.c ./*.o ./*.so
