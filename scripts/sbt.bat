@echo off
set SCRIPT_DIR=%~dp0
java -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -XX:ReservedCodeCacheSize=256m -Xmx1024M -Xss2M -Djline.WindowsTerminal.directConsole=false -jar "%SCRIPT_DIR%sbt.jar" %*
