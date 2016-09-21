#
#	Compiler options ..
#
#
FC = gfortran -O3
LD = gfortran -O3
CC = gcc -O3
FFLAGS   = ${CFLAGS}

ADSTACK= ${TAPENADE_HOME}/ADFirstAidKit/adStack.c
ADBUFFER= ${TAPENADE_HOME}/ADFirstAidKit/adBuffer.f

all:run 

SRC = iterativeSolver.f func_d.f obj_d.f func_b.f obj_b.f 

OBJ = iterativeSolver.o	func_d.o obj_d.o func_b.o obj_b.o adStack.o adBuffer.o 

run: $(OBJ) 
	$(LD) $(OBJ) -o run 

adStack.o: ${ADSTACK}
		${CC} ${CFLAGS} -c ${ADSTACK}

adBuffer.o: ${ADBUFFER}
		${FC} ${FFLAGS} -c ${ADBUFFER}
#
#
#
#	Make clean ..
#
#
clean:
	rm -f *.o
#
#
extra_clean:
	rm -f *.*~
#  

