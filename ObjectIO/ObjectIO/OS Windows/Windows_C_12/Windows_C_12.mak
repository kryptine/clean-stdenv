# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Windows_C_12 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Windows_C_12 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Windows_C_12 - Win32 Release" && "$(CFG)" !=\
 "Windows_C_12 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "Windows_C_12.mak" CFG="Windows_C_12 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Windows_C_12 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Windows_C_12 - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "Windows_C_12 - Win32 Debug"
CPP=cl.exe
RSC=rc.exe
MTL=mktyplib.exe

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\Windows_C_12.exe" "$(OUTDIR)\Windows_C_12.bsc"

CLEAN : 
	-@erase ".\Release\Windows_C_12.bsc"
	-@erase ".\Release\cCrossCall_12.sbr"
	-@erase ".\Release\cdebug_12.sbr"
	-@erase ".\Release\util_12.sbr"
	-@erase ".\Release\cpicture_12.sbr"
	-@erase ".\Release\ddutil.sbr"
	-@erase ".\Release\cGameLib_12.sbr"
	-@erase ".\Release\cprinter_12.sbr"
	-@erase ".\Release\Dsutil.sbr"
	-@erase ".\Release\cOSGameLib_12.sbr"
	-@erase ".\Release\Windows_C_12.exe"
	-@erase ".\Release\cOSGameLib_12.obj"
	-@erase ".\Release\cCrossCall_12.obj"
	-@erase ".\Release\cdebug_12.obj"
	-@erase ".\Release\util_12.obj"
	-@erase ".\Release\cpicture_12.obj"
	-@erase ".\Release\ddutil.obj"
	-@erase ".\Release\cGameLib_12.obj"
	-@erase ".\Release\cprinter_12.obj"
	-@erase ".\Release\Dsutil.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "C:\dx5sdk\sdk\inc" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "HILT" /FR /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "C:\dx5sdk\sdk\inc" /D "WIN32" /D "NDEBUG"\
 /D "_WINDOWS" /D "HILT" /FR"$(INTDIR)/" /Fp"$(INTDIR)/Windows_C_12.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\Release/
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Windows_C_12.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/cCrossCall_12.sbr" \
	"$(INTDIR)/cdebug_12.sbr" \
	"$(INTDIR)/util_12.sbr" \
	"$(INTDIR)/cpicture_12.sbr" \
	"$(INTDIR)/ddutil.sbr" \
	"$(INTDIR)/cGameLib_12.sbr" \
	"$(INTDIR)/cprinter_12.sbr" \
	"$(INTDIR)/Dsutil.sbr" \
	"$(INTDIR)/cOSGameLib_12.sbr"

"$(OUTDIR)\Windows_C_12.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/Windows_C_12.pdb" /machine:I386\
 /out:"$(OUTDIR)/Windows_C_12.exe" 
LINK32_OBJS= \
	"$(INTDIR)/cOSGameLib_12.obj" \
	"$(INTDIR)/cCrossCall_12.obj" \
	"$(INTDIR)/cdebug_12.obj" \
	"$(INTDIR)/util_12.obj" \
	"$(INTDIR)/cpicture_12.obj" \
	"$(INTDIR)/ddutil.obj" \
	"$(INTDIR)/cGameLib_12.obj" \
	"$(INTDIR)/cprinter_12.obj" \
	"$(INTDIR)/Dsutil.obj"

"$(OUTDIR)\Windows_C_12.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\Windows_C_12.exe"

CLEAN : 
	-@erase ".\Debug\vc40.pdb"
	-@erase ".\Debug\vc40.idb"
	-@erase ".\Debug\Windows_C_12.exe"
	-@erase ".\Debug\util_12.obj"
	-@erase ".\Debug\cpicture_12.obj"
	-@erase ".\Debug\cGameLib_12.obj"
	-@erase ".\Debug\cOSGameLib_12.obj"
	-@erase ".\Debug\cdebug_12.obj"
	-@erase ".\Debug\cCrossCall_12.obj"
	-@erase ".\Debug\ddutil.obj"
	-@erase ".\Debug\cprinter_12.obj"
	-@erase ".\Debug\Dsutil.obj"
	-@erase ".\Debug\Windows_C_12.ilk"
	-@erase ".\Debug\Windows_C_12.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/Windows_C_12.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/Windows_C_12.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/Windows_C_12.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/Windows_C_12.exe" 
LINK32_OBJS= \
	"$(INTDIR)/util_12.obj" \
	"$(INTDIR)/cpicture_12.obj" \
	"$(INTDIR)/cGameLib_12.obj" \
	"$(INTDIR)/cOSGameLib_12.obj" \
	"$(INTDIR)/cdebug_12.obj" \
	"$(INTDIR)/cCrossCall_12.obj" \
	"$(INTDIR)/ddutil.obj" \
	"$(INTDIR)/cprinter_12.obj" \
	"$(INTDIR)/Dsutil.obj"

"$(OUTDIR)\Windows_C_12.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "Windows_C_12 - Win32 Release"
# Name "Windows_C_12 - Win32 Debug"

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"

!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\cCrossCall_12.c

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"

DEP_CPP_CCROS=\
	".\cCrossCall_12.h"\
	".\cpicture_12.h"\
	".\cGameLib_12.h"\
	".\cprinter_12.c"\
	{$(INCLUDE)}"\htmlhelp.h"\
	".\util_12.h"\
	".\intrface_12.h"\
	".\cOSGameLib_12.h"\
	"\dx5sdk\sdk\inc\ddraw.h"\
	"\dx5sdk\sdk\inc\dsound.h"\
	".\Ddutil.h"\
	".\Dsutil.h"\
	"\dx5sdk\sdk\inc\d3dtypes.h"\
	".\..\..\..\..\dx5sdk\sdk\inc\d3dvec.inl"\
	".\cprinter_12.h"\
	
NODEP_CPP_CCROS=\
	".\..\..\..\..\dx5sdk\sdk\inc\subwtype.h"\
	

"$(INTDIR)\cCrossCall_12.obj" : $(SOURCE) $(DEP_CPP_CCROS) "$(INTDIR)"\
 ".\cprinter_12.c"

"$(INTDIR)\cCrossCall_12.sbr" : $(SOURCE) $(DEP_CPP_CCROS) "$(INTDIR)"\
 ".\cprinter_12.c"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"

DEP_CPP_CCROS=\
	".\cCrossCall_12.h"\
	".\cpicture_12.h"\
	".\cGameLib_12.h"\
	".\cprinter_12.c"\
	{$(INCLUDE)}"\htmlhelp.h"\
	".\util_12.h"\
	".\intrface_12.h"\
	".\cOSGameLib_12.h"\
	".\Ddutil.h"\
	".\Dsutil.h"\
	".\cprinter_12.h"\
	

"$(INTDIR)\cCrossCall_12.obj" : $(SOURCE) $(DEP_CPP_CCROS) "$(INTDIR)"\
 ".\cprinter_12.c"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cdebug_12.c
DEP_CPP_CDEBU=\
	".\cdebug_12.h"\
	".\util_12.h"\
	".\intrface_12.h"\
	

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"


"$(INTDIR)\cdebug_12.obj" : $(SOURCE) $(DEP_CPP_CDEBU) "$(INTDIR)"

"$(INTDIR)\cdebug_12.sbr" : $(SOURCE) $(DEP_CPP_CDEBU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"


"$(INTDIR)\cdebug_12.obj" : $(SOURCE) $(DEP_CPP_CDEBU) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cpicture_12.c
DEP_CPP_CPICT=\
	".\cpicture_12.h"\
	".\util_12.h"\
	".\intrface_12.h"\
	

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"


"$(INTDIR)\cpicture_12.obj" : $(SOURCE) $(DEP_CPP_CPICT) "$(INTDIR)"

"$(INTDIR)\cpicture_12.sbr" : $(SOURCE) $(DEP_CPP_CPICT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"


"$(INTDIR)\cpicture_12.obj" : $(SOURCE) $(DEP_CPP_CPICT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\util_12.c
DEP_CPP_UTIL_=\
	".\util_12.h"\
	".\intrface_12.h"\
	

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"


"$(INTDIR)\util_12.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"

"$(INTDIR)\util_12.sbr" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"


"$(INTDIR)\util_12.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cprinter_12.c
DEP_CPP_CPRIN=\
	".\cpicture_12.h"\
	".\util_12.h"\
	".\cCrossCall_12.h"\
	".\cprinter_12.h"\
	".\intrface_12.h"\
	

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"


"$(INTDIR)\cprinter_12.obj" : $(SOURCE) $(DEP_CPP_CPRIN) "$(INTDIR)"

"$(INTDIR)\cprinter_12.sbr" : $(SOURCE) $(DEP_CPP_CPRIN) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"


"$(INTDIR)\cprinter_12.obj" : $(SOURCE) $(DEP_CPP_CPRIN) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cGameLib_12.c

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"

DEP_CPP_CGAME=\
	".\util_12.h"\
	".\intrface_12.h"\
	".\cGameLib_12.h"\
	".\cOSGameLib_12.h"\
	"\dx5sdk\sdk\inc\ddraw.h"\
	"\dx5sdk\sdk\inc\dsound.h"\
	".\Ddutil.h"\
	".\Dsutil.h"\
	"\dx5sdk\sdk\inc\d3dtypes.h"\
	".\..\..\..\..\dx5sdk\sdk\inc\d3dvec.inl"\
	
NODEP_CPP_CGAME=\
	".\..\..\..\..\dx5sdk\sdk\inc\subwtype.h"\
	

"$(INTDIR)\cGameLib_12.obj" : $(SOURCE) $(DEP_CPP_CGAME) "$(INTDIR)"

"$(INTDIR)\cGameLib_12.sbr" : $(SOURCE) $(DEP_CPP_CGAME) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"

DEP_CPP_CGAME=\
	".\util_12.h"\
	".\intrface_12.h"\
	".\cGameLib_12.h"\
	".\cOSGameLib_12.h"\
	".\Ddutil.h"\
	".\Dsutil.h"\
	

"$(INTDIR)\cGameLib_12.obj" : $(SOURCE) $(DEP_CPP_CGAME) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cOSGameLib_12.c

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"

DEP_CPP_COSGA=\
	".\cOSGameLib_12.h"\
	".\util_12.h"\
	".\intrface_12.h"\
	"\dx5sdk\sdk\inc\ddraw.h"\
	"\dx5sdk\sdk\inc\dsound.h"\
	".\Ddutil.h"\
	".\Dsutil.h"\
	"\dx5sdk\sdk\inc\d3dtypes.h"\
	".\..\..\..\..\dx5sdk\sdk\inc\d3dvec.inl"\
	
NODEP_CPP_COSGA=\
	".\..\..\..\..\dx5sdk\sdk\inc\subwtype.h"\
	

"$(INTDIR)\cOSGameLib_12.obj" : $(SOURCE) $(DEP_CPP_COSGA) "$(INTDIR)"

"$(INTDIR)\cOSGameLib_12.sbr" : $(SOURCE) $(DEP_CPP_COSGA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"

DEP_CPP_COSGA=\
	".\cOSGameLib_12.h"\
	".\util_12.h"\
	".\intrface_12.h"\
	".\Ddutil.h"\
	".\Dsutil.h"\
	

"$(INTDIR)\cOSGameLib_12.obj" : $(SOURCE) $(DEP_CPP_COSGA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ddutil.cpp

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"

DEP_CPP_DDUTI=\
	"\dx5sdk\sdk\inc\ddraw.h"\
	".\Ddutil.h"\
	

"$(INTDIR)\ddutil.obj" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"

"$(INTDIR)\ddutil.sbr" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"

DEP_CPP_DDUTI=\
	".\Ddutil.h"\
	

"$(INTDIR)\ddutil.obj" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Dsutil.c

!IF  "$(CFG)" == "Windows_C_12 - Win32 Release"

DEP_CPP_DSUTI=\
	"\dx5sdk\sdk\inc\dsound.h"\
	".\Dsutil.h"\
	"\dx5sdk\sdk\inc\d3dtypes.h"\
	"\dx5sdk\sdk\inc\ddraw.h"\
	".\..\..\..\..\dx5sdk\sdk\inc\d3dvec.inl"\
	
NODEP_CPP_DSUTI=\
	".\..\..\..\..\dx5sdk\sdk\inc\subwtype.h"\
	

"$(INTDIR)\Dsutil.obj" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"

"$(INTDIR)\Dsutil.sbr" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Windows_C_12 - Win32 Debug"

DEP_CPP_DSUTI=\
	".\Dsutil.h"\
	

"$(INTDIR)\Dsutil.obj" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
