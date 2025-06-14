SRCDIR = src

all: prepare 

prepare: # Regla para preparar los directorios
	cd $(SRCDIR) && swipl main.pl 


