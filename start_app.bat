@echo off
echo Starting ML Model Builder...
echo.

REM Check if R is installed and in PATH
where Rscript >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo Error: Rscript not found in PATH.
    echo Please make sure R is installed and added to your system PATH.
    echo You can download R from: https://cran.r-project.org/
    pause
    exit /b 1
)

REM Run the R startup script
Rscript run_app.R

REM Keep window open if there was an error
if %ERRORLEVEL% NEQ 0 (
    echo.
    echo An error occurred. Press any key to exit.
    pause >nul
)
