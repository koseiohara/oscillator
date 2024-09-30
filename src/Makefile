TARGET = OSCILLATOR

OBJS = BinIO.o params.o namelist.o oscillator.o main.o
PYPATH = ../plot/
PY = plot.py
NML = ../nml/simulation.nml

ASS = ${OBJS:.o=.s}


FC = gfortran
#FC = ifort

ifeq (${FC}, ifort)
	FLAGS = -warn all -O1 -convert little_endian -assume byterecl -traceback
else
	FLAGS = -Wall -O1 -fbacktrace -fconvert=little-endian
endif



%.o : %.f90
	${FC} -c $< ${FLAGS}

%.s : %.f90
	${FC} -S $< ${FLAGS}


all : ${TARGET}

${TARGET} : ${OBJS}
	${FC} -o $@ $^



.PHONY : run clean plot as re


run : 
	./${TARGET} < ${NML}

clean :
	rm -fv *.o *.mod *.s ${TARGET}

plot :
	cd ${PYPATH}; python3 ${PY} ${NML}

re : clean all

as : ${ASS}

