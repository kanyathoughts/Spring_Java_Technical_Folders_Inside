@echo off
set freePort=
set startPort=%1

:SEARCHPORT
netstat -o -n -a | find ":%startPort% " | findstr -i "abh.ren listening" > NUL
if "%ERRORLEVEL%" equ "0" (
  set /a startPort +=1
  goto :SEARCHPORT
) else (
  set freePort=%startPort%
  goto :FOUNDPORT
)

:FOUNDPORT
echo %freePort%