#!/bin/zsh
# Mac용 빌드+실행 스크립트 (RUN.cmd의 macOS 버전)
cd "$(dirname "$0")"
MCS="$HOME/Mono.framework/Versions/6.12.0/bin/mcs"
WINE="$HOME/wine-local/Wine Devel.app/Contents/Resources/wine/bin/wine"
if [ ! -f XGB_XGT_HMI_Designer.exe ] || [ XGB_XGT_HMI_Designer.cs -nt XGB_XGT_HMI_Designer.exe ]; then
  echo "Building..."
  "$MCS" -target:winexe -out:XGB_XGT_HMI_Designer.exe -r:System.dll -r:System.Drawing.dll -r:System.Windows.Forms.dll -r:System.Xml.dll XGB_XGT_HMI_Designer.cs || { echo "빌드 실패"; read -k1; exit 1; }
fi
export WINEPREFIX="$HOME/.wine-hmi" WINEDEBUG=-all
exec "$WINE" XGB_XGT_HMI_Designer.exe
