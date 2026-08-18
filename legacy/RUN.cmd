@echo off
REM XGB/XGT HMI Designer - Windows 실행 스크립트
REM .NET 10 SDK 가 필요합니다: https://dotnet.microsoft.com/download
setlocal
cd /d "%~dp0"

where dotnet >nul 2>nul
if errorlevel 1 (
  echo [ERROR] dotnet 을 찾을 수 없습니다. .NET 10 SDK 를 설치한 뒤 다시 실행하십시오.
  pause
  exit /b 1
)

dotnet run --project src\XgbHmi.App\XgbHmi.App.fsproj -c Release %*
if errorlevel 1 pause
