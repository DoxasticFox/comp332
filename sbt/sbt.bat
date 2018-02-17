set SCRIPT_DIR=%~dp0
java -Xmx4096M -Xss2M -jar "%SCRIPT_DIR%\sbt-launch.jar" %*
