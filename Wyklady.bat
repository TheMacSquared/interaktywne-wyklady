@echo off
rem Dwuklik uruchamia hub wykladow i otwiera go w przegladarce (Windows).
rem To okno musi zostac otwarte przez cale zajecia.
chcp 65001 >nul
setlocal
cd /d "%~dp0"

set "RSCRIPT="

rem 1) Rscript w PATH
where Rscript >nul 2>nul && set "RSCRIPT=Rscript"

rem 2) Rejestr (instalator R zapisuje tam sciezke)
if not defined RSCRIPT for %%K in (HKLM HKCU) do (
  if not defined RSCRIPT for /f "tokens=2,*" %%A in ('reg query "%%K\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do (
    if exist "%%B\bin\Rscript.exe" set "RSCRIPT=%%B\bin\Rscript.exe"
  )
)

rem 3) Typowe katalogi instalacji
if not defined RSCRIPT for %%P in ("%ProgramFiles%\R" "%LOCALAPPDATA%\Programs\R") do (
  if not defined RSCRIPT for /f "delims=" %%D in ('dir /b /ad /o-n "%%~P\R-*" 2^>nul') do (
    if not defined RSCRIPT if exist "%%~P\%%D\bin\Rscript.exe" set "RSCRIPT=%%~P\%%D\bin\Rscript.exe"
  )
)

if not defined RSCRIPT (
  echo Nie znaleziono R na tym komputerze.
  echo Zainstaluj R ze strony https://cran.r-project.org i uruchom ten plik ponownie.
  pause
  exit /b 1
)

"%RSCRIPT%" hub\start.R
if errorlevel 1 pause
