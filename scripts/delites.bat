@echo off

SET SCRIPT=%TEMP%\%~n0
if exist "%SCRIPT%" del "%SCRIPT%"
copy "%~dp0\shared" "%SCRIPT%.prefix" > NUL
echo try { >> "%SCRIPT%.prefix"
echo.|set /P ="/" >> "%SCRIPT%.prefix"
echo * >> "%SCRIPT%.prefix"
copy "%~dpn0" "%SCRIPT%.middle" > NUL
echo } catch { > "%SCRIPT%.postfix"
echo case e: Exception =^> ^{ >> "%SCRIPT%.postfix"
echo var message = ^"%%s^".format^(if ^(e.getMessage != null^) e.getMessage else e.getCause^) >> "%SCRIPT%.postfix"
echo message = ^"%%s%%n%%s^".format^(message, ^"It may help to run delitecfg or manually adjust %%s.^" .format^(config.file.getAbsolutePath^)^) >> "%SCRIPT%.postfix"
echo println(message) >> "%SCRIPT%.postfix"
echo } >> "%SCRIPT%.postfix"
echo } >> "%SCRIPT%.postfix"
copy "%SCRIPT%.prefix"+"%SCRIPT%.middle"+"%SCRIPT%.postfix" "%SCRIPT%" > NUL

SET SCRIPT_HOME=%~dp0
SET SCRIPT_NAME=%~nx0
scala "%SCRIPT%" %*
