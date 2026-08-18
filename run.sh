#!/usr/bin/env bash
# XGB/XGT HMI Designer - macOS / Linux 실행 스크립트
# .NET 10 SDK 만 있으면 됩니다: https://dotnet.microsoft.com/download
set -e
cd "$(dirname "$0")"

if ! command -v dotnet >/dev/null 2>&1; then
  echo "[ERROR] dotnet 을 찾을 수 없습니다. .NET 10 SDK 를 설치한 뒤 다시 실행하십시오."
  echo "        macOS:  brew install --cask dotnet-sdk"
  echo "        Linux:  https://learn.microsoft.com/dotnet/core/install/linux"
  exit 1
fi

exec dotnet run --project src/XgbHmi.App/XgbHmi.App.fsproj -c Release "$@"
