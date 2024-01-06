@echo off
rem Run R script
Rscript.exe "path/to/your/R/script.R"

rem Pause for 60 seconds
timeout /t 60 /nobreak

rem Run Python script
python.exe "path/to/your/Python/script.py"
