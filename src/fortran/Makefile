#
# Automagically generated by Approximatrix Simply Fortran 2.25
#
FC="C:\Program Files (x86)\Simply Fortran 2\mingw-w64\bin\gfortran.exe"
CC="C:\Program Files (x86)\Simply Fortran 2\mingw-w64\bin\gcc.exe"
AR="C:\Program Files (x86)\Simply Fortran 2\mingw-w64\bin\ar.exe"
WRC="C:\PROGRA~2\SIMPLY~1\MINGW-~1\bin\windres.exe"
RM=rm -f

IDIR=

LDIR=


OPTFLAGS= -g

SPECIALFLAGS=-m32 $(IDIR)

RCFLAGS=-O coff -F pe-i386

PRJ_FFLAGS=

PRJ_CFLAGS=

PRJ_LFLAGS=

FFLAGS=$(SPECIALFLAGS) $(OPTFLAGS) $(PRJ_FFLAGS) -Jmodules 

CFLAGS=$(SPECIALFLAGS) $(OPTFLAGS) $(PRJ_CFLAGS)

"build\cloc.o": ".\cloc.f" "E:\projects\GitHub\cloc\src\fortran\global.f"
	@echo Compiling .\cloc.f
	@$(FC) -c -o "build\cloc.o" $(FFLAGS) ".\cloc.f"

clean: .SYMBOLIC
	@echo Deleting build\cloc.o and related files
	@$(RM) "build\cloc.o"
	@echo Deleting build\global.o and related files
	@$(RM) "build\global.o"
	@echo Deleting cloc.exe
	@$(RM) "cloc.exe"

"cloc.exe":  "build\cloc.o" "build\cloc.prj.target"
	@echo Generating cloc.exe
	@$(FC) -o "cloc.exe" -static -m32 "build\cloc.o" $(LDIR) $(PRJ_LFLAGS)

all: "cloc.exe" .SYMBOLIC

