IRPF90 = irpf90 # -a -d
FC     = ifort
FCFLAGS= -O2 -traceback

SRC=
OBJ=
LIB=

include irpf90.make

irpf90.make: $(wildcard *.irp.f)
	$(IRPF90)
