@echo off
REM One-click BIOS-Rank app (Windows)
cd /d "%~dp0"

set "RSCRIPT=C:\Program Files\R\R-4.6.0\bin\Rscript.exe"
if not exist "%RSCRIPT%" set "RSCRIPT=C:\Program Files\R\R-4.5.0\bin\Rscript.exe"
if not exist "%RSCRIPT%" set "RSCRIPT=C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
if not exist "%RSCRIPT%" (
  echo Rscript.exe not found. Install R or edit this .bat with your R path.
  pause
  exit /b 1
)

echo Launching BIOS-Rank app...
"%RSCRIPT%" "%~dp0run_BIOS_Rank_app.R"
if errorlevel 1 pause
)
