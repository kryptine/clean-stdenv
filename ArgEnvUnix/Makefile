#
#	Version 1.0.1
#	Ronny Wichers Schreur
#	ronny@cs.kun.nl
#
CLM=clm

CC=gcc
COPTIONS=-Wall -pedantic

printenv : printenv.o ArgEnv.o ArgEnvC.o
	$(CLM) -b -nt -l ArgEnvC.o printenv -o printenv

ArgEnvC.o : ArgEnvC.c
	$(CC) $(COPTIONS) -c ArgEnvC.c

ArgEnv.o : ArgEnv.icl ArgEnv.dcl
	$(CLM) -O ArgEnv

printenv.o : printenv.icl ArgEnv.dcl
	$(CLM) -O printenv

cleanup :
	$(RM) printenv *.o *.abc
