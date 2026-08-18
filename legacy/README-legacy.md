# legacy — 기존 Windows 전용 v6 원본 (참고 보관용)

이 폴더의 파일들은 F#/Avalonia 로 다시 만들기 전의 원본입니다. 새 앱은 이 파일들을 사용하지 않습니다.

| 파일 | 내용 |
|---|---|
| `XGB_XGT_HMI_Designer.cs` | C# / WinForms v6 전체 소스 (1,659줄). 통신 프레임과 화면 동작의 기준 문서 역할 |
| `XGB_XGT_HMI_Designer.exe` | 위 소스를 mono 로 빌드했던 실행 파일 (Windows 전용) |
| `RUN.cmd`, `실행.bat` | Windows 에서 csc.exe 로 빌드 후 실행하던 스크립트 |
| `실행_mac.command` | macOS 에서 mono + Wine 으로 우회 실행하던 스크립트 |
| `README.txt` | v3~v6 기능/수정 이력 원문 |

새 판의 실행 방법은 상위 폴더의 `README.md` 를 보십시오.
프로젝트 파일 `r004_hmi_project.xml` 형식은 그대로라서 이 v6 판과 새 판이 같은 파일을 함께 쓸 수 있습니다.
