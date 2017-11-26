FC = gfortran
CC = gcc

FFLAGS = -std=f2008 -fcheck=all -fbacktrace -g
CFLAGS = 
LDFLAGS = 
BINDIR = bin

all: $(BINDIR) $(BINDIR)/stencil_space_test

$(BINDIR)/stencil_space_test: stencil_space.o display.o stencil_space_test.o
	$(FC) -o $@ $^ $(LDFLAGS)

%.o: %.f*
	$(FC) -o $@ -c $< $(FFLAGS)

clean:
	@rm -rf *.o

cleanall: clean
	@rm -rf $(BINDIR)/*

$(BINDIR):
	@mkdir -p $(BINDIR)
