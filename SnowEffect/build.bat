IF NOT EXIST bin mkdir bin
IF NOT EXIST lst mkdir lst

..\tools\gfx -textmc art\somekindoftreeman.png 14 1 10 -out=bin\somekindoftreeman
if %errorlevel% neq 0 exit /b %errorlevel%
..\tools\gfx -columns art\snowback.png bin\snowback.bin 5 4x1 1x32
if %errorlevel% neq 0 exit /b %errorlevel%
..\tools\gfx -columns art\snowflakes.png bin\snowflakes.bin 5 48x1 1x8
if %errorlevel% neq 0 exit /b %errorlevel%
..\tools\x65.exe SnowEffect.s SnowEffect.prg -vice SnowEffect.vs -sym SnowEffect.sym -lst=lst\SnowEffect.lst
if %errorlevel% neq 0 exit /b %errorlevel%
\vice\x64sc -moncommands SnowEffect.vs -remotemonitor SnowEffect.prg 

