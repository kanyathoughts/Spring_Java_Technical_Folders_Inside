@echo off

rem the description shown by the service info
set SERVICE_DESCRIPTION="This is the innoWake Mining-Api server."

rem displayname, visible name for the windows service administration view
set SERVICE_DISPLAY_NAME=innoWake-Mining-Api

rem list of additional jvm options. Options are separated using either # or ; characters
set SERVICE_JVM_OPTIONS="-Dlogging.config=log4j.xml"

rem handle user specified variables
rem if no mining jar or orientdb ip specified then fail execution
if "%1" == "" goto error
set USER=%1
echo %USER%
if %2 == "" goto error
set PASSWORD=%2
echo %PASSWORD%
if "%3" == "" goto error
set MINING_JAR=%3
if "%4" == "" goto error
set ORIENTDB_IP=%4
if /i "%5" == "" (
    set SERVICE_ENVIRONMENT_VARS="--spring.datasource.url=jdbc:orient:remote:%ORIENTDB_IP%:2424/mining"
    goto run
)

rem list of additional envrionment variables. Vars are separated using either # or ; charcaters
set KEYCLOAK_IP=%5
set SERVICE_ENVIRONMENT_VARS="--spring.profiles.active=authorized-access";"--keycloak.auth-server-url=http://%KEYCLOAK_IP%:8080/auth";"--spring.datasource.url=jdbc:orient:remote:%ORIENTDB_IP%:2424/mining"

:run
echo %SERVICE_ENVIRONMENT_VARS%
rem The main part of the script.

rem set JVM location
set JVM="C:\jse\jdk\zulu 11.0.9.1\bin\server\jvm.dll"
echo JVM home is: %JVM%

rem Find the application home.
set APPLICATION_PATH=%~dp0
echo %APPLICATION_PATH%

rem For parameter information, visit http://commons.apache.org/proper/commons-daemon/procrun.html
rem set the servicename and the executable name
set SERVICE_NAME=MiningApi
set EXECUTABLE=%~dp0%MiningApi.exe
echo Using executable %EXECUTABLE%

rem the main class used by the service wrapper to launch the java application
set SERVICE_START_CLASS=org.springframework.boot.loader.JarLauncher
rem since we dont need a different class to sopt the service, use the same as the startclass
set SERVICE_STOP_CLASS=%SERVICE_START_CLASS%

@REM rem the start and stop methode called by the service wrapper to run the java application
@REM set SERVICE_START_METHOD=main
@REM set SERVICE_STOP_METHOD=main

rem classpath containing anything for the wrapped application to run smoothly
set "SERVICE_CLASSPATH=%~dp0%MINING_JAR%"

echo Classpath: %SERVICE_CLASSPATH%

rem startup typ of the service, the service will start automatically on startup
set SERVICE_STARTUP_TYPE=auto

rem if a jvm is specified by the JVM_HOME, the defined jvm is used. otherwise the service wrapper
rem will search for the default jvm.
set SERVICE_PATH_TO_JVM=%JVM%

rem the install image
set SERVICE_INSTALL=%EXECUTABLE%

rem path for logging files
set SERVICE_LOGPATH=%APPLICATION_PATH%\logs

rem Install the Service -------
echo Installing service %SERVICE_NAME% ...

%EXECUTABLE% install %SERVICE_NAME% ^
    --DisplayName %SERVICE_DISPLAY_NAME% ^
    --Description %SERVICE_DESCRIPTION% ^
    --Install %SERVICE_INSTALL% ^
    --Classpath %SERVICE_CLASSPATH% ^
    --LogPath %SERVICE_LOGPATH% ^
    --Startup %SERVICE_STARTUP_TYPE% ^
    --StartClass %SERVICE_START_CLASS% ^
        --StopClass %SERVICE_STOP_CLASS% ^
    --StartMode jvm ^
        --StopMode jvm ^
         --Jvm %SERVICE_PATH_TO_JVM% ^
    ++JvmOptions %SERVICE_JVM_OPTIONS% ^
    ++Environment %SERVICE_ENVIRONMENT_VARS% ^
    --ServiceUser %USER% ^
    --ServicePassword %PASSWORD% ^
    --StdOutput auto ^
    --StdError auto


if errorlevel 1 goto error

echo.
echo The service '%SERVICE_DISPLAY_NAME%` has been installed successfully.
goto eof

:error
echo.
echo The service '%SERVICE_DISPLAY_NAME%' was unable to be installed.

:eof