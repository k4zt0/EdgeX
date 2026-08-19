# legacy — 기존 Windows 전용 v6 원본 (참고 보관용)

이 폴더는 F#/Avalonia 로 다시 만들기 전의 **문서**만 남겨 둔 곳입니다. 새 앱은 이 파일들을 사용하지 않습니다.

| 파일 | 내용 |
|---|---|
| `XGB_XGT_HMI_Designer.cs` | C# / WinForms v6 전체 소스 (1,659줄). 통신 프레임과 화면 동작의 기준 문서 역할 |
| `README.txt` | v3~v6 기능/수정 이력 원문 |

빌드 산출물(`XGB_XGT_HMI_Designer.exe`)과 실행 스크립트(`RUN.cmd`, `실행.bat`, `실행_mac.command`)는
csc.exe / mono / Wine 에 기대던 것이라 더 쓰지 않으므로 지웠습니다. 필요하면 git 이력에서 꺼내 쓰십시오.

새 판의 실행 방법은 상위 폴더의 `README.md` 를 보십시오.
프로젝트 파일 `r004_hmi_project.xml` 형식은 그대로라서 이 v6 판과 새 판이 같은 파일을 함께 쓸 수 있습니다.
