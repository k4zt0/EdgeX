@echo off
REM 배포용 단일 실행 파일 만들기 (대상 PC 에 .NET 설치 불필요)
REM   publish.cmd            -> win-x64
REM   publish.cmd win-arm64  -> 특정 런타임용
setlocal
cd /d "%~dp0"

set RID=%1
if "%RID%"=="" set RID=win-x64

echo Publishing for %RID% ...
dotnet publish src\XgbHmi.App\XgbHmi.App.fsproj -c Release -r %RID% --self-contained true ^
  -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -o dist\%RID%
if errorlevel 1 goto FAIL

copy /y r004_hmi_project.xml dist\%RID%\ >nul 2>nul
echo Done: dist\%RID%
exit /b 0

:FAIL
echo [ERROR] publish 실패
pause
exit /b 1
