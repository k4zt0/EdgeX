@echo off
setlocal
cd /d "%~dp0"

set "CSC=%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe"
if exist "%CSC%" goto HAVE_CSC
set "CSC=%WINDIR%\Microsoft.NET\Framework\v4.0.30319\csc.exe"
if exist "%CSC%" goto HAVE_CSC

echo [ERROR] csc.exe was not found.
echo Install/enable Microsoft .NET Framework 4.x and run again.
pause
exit /b 1

:HAVE_CSC
echo Building XGB XGT HMI Designer v5...
"%CSC%" /nologo /target:winexe /platform:anycpu /out:"XGB_XGT_HMI_Designer.exe" /reference:System.dll /reference:System.Drawing.dll /reference:System.Windows.Forms.dll /reference:System.Xml.dll "XGB_XGT_HMI_Designer.cs"
if errorlevel 1 goto BUILD_ERROR

echo Build OK. Starting program...
start "" "%CD%\XGB_XGT_HMI_Designer.exe"
exit /b 0

:BUILD_ERROR
echo.
echo [ERROR] Build failed.
echo Take a screenshot of the lines above and send it to me.
pause
exit /b 1
