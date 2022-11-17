# Définition des Variables (à compléter)
CC = c99
CFLAGS = -Wextra -pedantic -c  
LDFLAGS = 
EXE = 

# Règles (à compléter avec des variables automatiques)
all: test_file exemple_file

test_file: file.o test_file.o
	$(CC) $(LDFLAGS) $< $@.o -o $@

exemple_file: file.o exemple_file.o
	$(CC) $(LDFLAGS) $< $@.o -o $@

depend:
	makedepend *.c -Y.

clean:
	rm .*o test_file exemple_file
.PHONY: clean all depend

# DO NOT DELETE

exemple_file.o: file.h
file.o: file.h
test_file.o: file.h
