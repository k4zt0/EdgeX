#!/usr/bin/env bash
# 배포용 단일 실행 파일 만들기 (대상 PC 에 .NET 설치 불필요)
#   ./publish.sh                -> 현재 OS 용
#   ./publish.sh win-x64        -> 특정 런타임용
# 결과물: dist/<rid>/XgbHmiDesigner(.exe)
set -e
cd "$(dirname "$0")"

RID="$1"
if [ -z "$RID" ]; then
  case "$(uname -s)-$(uname -m)" in
    Darwin-arm64) RID="osx-arm64" ;;
    Darwin-x86_64) RID="osx-x64" ;;
    Linux-aarch64) RID="linux-arm64" ;;
    Linux-x86_64) RID="linux-x64" ;;
    *) RID="linux-x64" ;;
  esac
fi

echo "Publishing for $RID ..."
dotnet publish src/XgbHmi.App/XgbHmi.App.fsproj \
  -c Release -r "$RID" --self-contained true \
  -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true \
  -o "dist/$RID"

cp -f r004_hmi_project.xml "dist/$RID/" 2>/dev/null || true
echo "Done: dist/$RID"
