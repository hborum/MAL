@echo off
setlocal enabledelayedexpansion
set "flags="
for %%A in (%*) do (
    if "%%A"=="-dd"           set flags=!flags!;DUMMYDEP
    if "%%A"=="--DUMMYDEP"    set flags=!flags!;DUMMYDEP
    if "%%A"=="-fs"           set flags=!flags!;FMALSTATIC
    if "%%A"=="--FMALSTATIC"  set flags=!flags!;FMALSTATIC
    if "%%A"=="-ed"           set flags=!flags!;EDLUNDEXAMPLE
    if "%%A"=="--EDLUND"      set flags=!flags!;EDLUNDEXAMPLE
    if "%%A"=="-t"            set flags=!flags!;TEST
    if "%%A"=="--TEST"        set flags=!flags!;TEST

    if "%%A"=="-r"           goto RELEASE
    if "%%A"=="/r"           goto RELEASE
    if "%%A"=="--release"    goto RELEASE

    if "%%A"=="-c"           goto CLEAN
    if "%%A"=="/c"           goto CLEAN
    if "%%A"=="--clean"      goto CLEAN

    if "%%A"=="-h"           goto HELP
    if "%%A"=="/h"           goto HELP
    if "%%A"=="--help"       goto HELP
)


:DEBUG
@echo on
call msbuild /p:Configuration=DEBUG /p:DefineConstants="%flags%" /verbosity:m /m
@echo off
goto END

:RELEASE
@echo on
msbuild /p:Configuration=Release /p:DefineConstants="%flags%" /t:Rebuild /verbosity:m /m
@echo off
goto END

:CLEAN
@echo on
msbuild /property:Configuration=Debug /t:Clean /m
msbuild /property:Configuration=Release /t:Clean /m
@echo off
goto END

:HELP
echo Build FMAL in debug mode.
echo Usage:
echo  "> build [OPTION]"
echo Options:
echo   -r    /r     --release     - Build in release mode.
echo   -c    /c     --clean       - Clean project in all modes.
:END


