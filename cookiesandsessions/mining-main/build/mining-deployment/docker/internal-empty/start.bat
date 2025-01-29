@echo off

setLOCAL ENABLEDELAYEDEXPANSION

CALL :FindLicense
copy %LicenseactivationLocation% docker\innowake.lic
call :obtain_ports

if "%1" equ "--reinitialize" (
	echo reinitializing dockerized mining
	goto :initialize
	exit /B
)

REM if there is no orientdb container, potential client data in a potential postgres container can't have matching data in an orientdb container
REM -> potentially existing postgres container gets removed
docker ps -a | find /i "dockerized-mining_orientdb_1" > NUL
if "%ERRORLEVEL%" equ "1" (
	echo no dockerized-mining_orientdb_1 container exists. Removing potentially existing dockerized-mining_postgres_1 container to avoid non matching client data.
	docker rm dockerized-mining_postgres_1
)

set available_images=Available images:
for /F "tokens=* USEBACKQ" %%F in (`docker images --format "{{.Repository}}"`) do (
	call :concat %%F
)

echo %available_images%

echo.%available_images% | find /I "mining_orientdb" > Nul && (
	echo.Found "mining_orientdb"
	echo.%available_images% | find /I "mining_apiserver" > Nul && (
		echo.Found "mining_apiserver"
		echo.%available_images% | find /I "postgres" > Nul && (
			echo.Found "postgres"
			echo.%available_images% | find /I "mining_keycloak" > Nul && (
				echo.Found "mining_keycloak"
				echo.Found everything.
				goto :run
			) || (
			echo.Did not find "mining_keycloak"
			)
		) || (
			echo.Did not find "postgres"
		)
	) || (
		echo.Did not find "mining_apiserver"
	)
) || (
	echo.Did not find "mining_orientdb"
)

echo.Did not find all required images
goto :initialize
exit /B

:concat
set available_images=%available_images% %1
goto :eof

:initialize
echo initalize
echo.
echo WARNING:
echo This script is intended to create/recreate all dockerized-mining images.
echo It will DELETE ALL DATA in dockerized-mining containers that is not stored in external volumes!
echo.
echo If you are running this script for the first time on this computer then it is safe to execute it.
echo.

set run_script="N"
set /p run_script="Are you sure that you want to run this script? (Y|[N]) "

if /I not "%run_script%"=="Y" exit /B


echo.
echo Making sure that dockerized-mining is down.
echo.
call docker-compose -f docker/docker-compose.yml -p "dockerized-mining" down

call :obtain_check_ports
cd docker

echo orient_db_studio_port=%orient_db_studio_port_check%> .env
echo orient_db_connection_port=%orient_db_connection_port_check%>> .env
echo keycloak_port=%keycloak_port_check%>> .env
echo apiserver_port=%apiserver_port_check%>> .env
echo License_location=%LicenseactivationLocation%>> .env
cd ..

echo.
echo Making sure that all images exist and that they are up to date.
echo.
call docker build -f docker\apiserver.docker -t mining_apiserver ..
call docker build -f docker\orientdb.docker -t mining_orientdb ..
call docker build -f docker\keycloak.docker -t mining_keycloak ..

echo.
echo Removing potentially existing postgres container
echo.
docker rm dockerized-mining_postgres_1

echo.
echo Starting Keycloak, OrientDB and API-Server.
echo Press CTRL-C to stop.
echo.
echo Available at the following ports:
echo apiserver port: %apiserver_port_check%
echo keycloak port: %keycloak_port_check%
echo orient_db studio port: %orient_db_studio_port_check%
echo orient_db connection port: %orient_db_connection_port_check%
timeout /T 3

call docker-compose -f docker/docker-compose.yml -p "dockerized-mining" up --force-recreate

exit /B

:run
echo.
call :obtain_check_ports
echo Checking if ports are available

set used_port=false
if /i not "%apiserver_port%"=="%apiserver_port_check%" (
	echo api server port is not available. Make sure port %apiserver_port% is not in use.
	set used_port=true
)
if /i not "%orient_db_studio_port%"=="%orient_db_studio_port_check%" (
	echo orientdb studio port is not available. Make sure port %orient_db_studio_port% is not in use.
	set used_port=true
)
if /i not "%orient_db_connection_port%"=="%orient_db_connection_port_check%" (
	echo orientdb connection port is not available. Make sure port %orient_db_connection_port% is not in use.
	set used_port=true
)
if /i not "%keycloak_port%"=="%keycloak_port_check%" (
	echo keycloak port is not available. Make sure port %keycloak_port% is not in use.
	set used_port=true
)

if /i %used_port%==true (
	echo "Exiting the script because port is not available. Make sure missing port is available and rerun the script!"
	exit /B
)

echo.
echo Starting Keycloak, OrientDB and API-Server.
echo Execute stop.bat to stop.
echo.
echo Available at the following ports:
echo apiserver port: %apiserver_port%
echo keycloak port: %keycloak_port%
echo orient_db studio port: %orient_db_studio_port%
echo orient_db connection port: %orient_db_connection_port%
echo.
echo Execute "start.bat --reinitialize" to force reinitialization (f.e. if you want to reset your orientdb)
echo.
timeout /T 3
call docker-compose -f docker/docker-compose.yml -p "dockerized-mining" up
exit /B


:obtain_ports
for /f "delims='=' tokens=1*" %%A in (docker\.env) do (
	if /i "%%A"=="orient_db_studio_port" (
		set orient_db_studio_port=%%B
	)
	if /i "%%A"=="orient_db_connection_port" (
		set orient_db_connection_port=%%B
	)
	if /i "%%A"=="keycloak_port" (
		set keycloak_port=%%B
	)
	if /i "%%A"=="apiserver_port" (
		set apiserver_port=%%B
	)
)
goto :eof

:obtain_check_ports
echo "checking if ports are available"
echo %apiserver_port%
for /F "tokens=* USEBACKQ" %%F in (`helper\find_open_port.bat %apiserver_port%`) do (
	set apiserver_port_check=%%F
)

for /F "tokens=* USEBACKQ" %%F in (`helper\find_open_port.bat %keycloak_port%`) do (
	set keycloak_port_check=%%F
)

for /F "tokens=* USEBACKQ" %%F in (`helper\find_open_port.bat %orient_db_studio_port%`) do (
	set orient_db_studio_port_check=%%F
)

for /F "tokens=* USEBACKQ" %%F in (`helper\find_open_port.bat %orient_db_connection_port%`) do (
	set orient_db_connection_port_check=%%F
)
goto :eof

ECHO apiserver port: %apiserver_port%
ECHO keycloak port: %keycloak_port%
ECHO orient_db studo port: %orient_db_studio_port%
ECHO orient_db connection port: %orient_db_connection_port%

:FindLicense 
for /r %innowake.license.activation% %%a in (*) do if "%%~nxa"=="innowake.lic" set LicenseactivationLocation=%%~dpnxa
	if defined LicenseactivationLocation (
		echo License File location - %LicenseactivationLocation%
		exit /B 0
	) else (
		echo License File not found at innowake.license.activation - %innowake.license.activation%
	)

for /r %innowake.license.activation.location% %%a in (*) do if "%%~nxa"=="innowake.lic" set LicenseactivationLocation=%%~dpnxa
	if defined LicenseactivationLocation (
		echo License File location - %LicenseactivationLocation%
		exit /B 0
	) else (
		echo License File not found at innowake.license.activation.location - %innowake.license.activation.location%
	)

if exist %USERPROFILE%\innowake.lic (
  set LicenseactivationLocation=%USERPROFILE%\innowake.lic
  echo License found at %USERPROFILE%\innowake.lic
  exit /B 0
) else (
  echo No License found at %USERPROFILE%\innowake.lic
)

if exist %USERPROFILE%%\appdata\local\temp\%\innowake.lic (
  set LicenseactivationLocation=%USERPROFILE%%\appdata\local\temp\%\innowake.lic
  echo License found at %USERPROFILE%%\appdata\local\temp\%\innowake.lic
  exit /B 0
) else (
  echo No License found at %USERPROFILE%%\appdata\local\temp\%\innowake.lic
)

echo License File not found at any location
exit /B 0