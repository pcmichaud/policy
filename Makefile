# ============================================================================
# Name        : Makefile
# Author      : Pierre-Carl Michaud
# Version     : 0.0
# Copyright   : Your copyright notice
# Description : Makefile for SP Project
# ============================================================================

.PHONY: all clean

FORTRAN_COMPILER = mpif90
FFLAGS = -funroll-all-loops -fopenmp -finit-local-zero -mfpmath=sse -fstrength-reduce  -fbounds-check -llapack  -lblas
OBJ = obj
MAIN1 = libs/dcdflib.a
MAIN2 =  ./runtime/runscenario

SRC2 = $(OBJ)/sorting.o $(OBJ)/quadrule.o $(OBJ)/know.o src/runscenario.f95 

all: $(MAIN1) $(MAIN2) 

$(MAIN1):
	gfortran -c libs/dcdflib.f/src/*.f
	ar r libs/dcdflib.a *.o
	rm *.o 

$(MAIN2): $(SRC2)
	mpif90 $(SRC2) libs/dcdflib.a -J$(OBJ) -O3 -o ./runtime/runscenario  $(FFLAGS)  

$(OBJ)/know.o: src/know.f95
	mpif90 -J$(OBJ) -O3 -c $< -o $@  $(FFLAGS) #-g
$(OBJ)/sorting.o: src/sorting.f95
	gfortran -J$(OBJ) -O3 -c $< -o $@  $(FFLAGS) #-g
$(OBJ)/quadrule.o: src/quadrule.f90
	gfortran -J$(OBJ) -O3 -c $< -o $@  $(FFLAGS) #-g

clean:
	rm -f runtime/runscenario obj/* libs/dcdflib.a