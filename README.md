# codex-ide

현재 WSL에서 사용 중인 Emacs Codex IDE의 **원본 코드 스냅샷**입니다.
리팩터링, 함수/변수 이름 변경, 실행 옵션 변경 없이 런타임 소스 4개를 그대로 복사했습니다.
수정·보완 내용은 [CHANGES.ko.md](CHANGES.ko.md)에 따로 기록했습니다.

## 포함 파일

| 파일 | 역할 |
|---|---|
| `codex-ide.el` | 편집기 컨텍스트, Codex App Server 클라이언트, 메뉴·승인·Ediff |
| `codex-ide-terminal.el` | 실제 Codex CLI 터미널, Ghostel·vterm·term, 한글 입력·표시 보정 |
| `codex-ide-mcp.py` | 현재 파일·커서·선택 영역·진단을 읽는 stdio MCP 브리지 |
| `codex-ide-picker.py` | 프로젝트별 대화 재개·삭제 선택기 |
| `config/current-init-excerpt.el` | 실제 init.el의 한글·폰트·Codex 시작 설정을 그대로 발췌 |
| `SOURCE-SNAPSHOT.json` | 런타임 소스의 SHA-256 및 스냅샷 환경 정보 |

## 현재 환경

Ubuntu 26.04 / WSL2 + WSLg, Emacs PGTK 30.2, Codex CLI 0.166.0,
Ghostel 패키지 20261027.833 / 네이티브 모듈 0.59.0에서 사용한 설정입니다.
현재 설정은 `codex-ide-interface`가 `terminal`, 터미널 백엔드가 `ghostel`입니다.
Emacs 패키지 헤더는 28.1 이상을 요구하지만 이 스냅샷은 실제 사용 환경 30.2를 기준으로 합니다.

## 설치와 실행

Codex CLI, Python 3, GUI Emacs, Ghostel 및 그 의존성 compat를 별도로 준비하세요.
이 Git 저장소에는 계정 인증, CLI 바이너리, Ghostel 바이너리, 대화 기록,
Windows 폰트 파일 또는 전체 개인 Emacs 설정을 넣지 않았습니다.

```bash
git clone https://github.com/gscho74/codex-ide.git
mkdir -p ~/.emacs.d/lisp
cp codex-ide/codex-ide.el codex-ide/codex-ide-terminal.el codex-ide/codex-ide-mcp.py codex-ide/codex-ide-picker.py ~/.emacs.d/lisp/
```

Emacs init에 다음 설정을 추가합니다. 이미 같은 설정이 있으면 중복으로 추가하지 마세요.

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'codex-ide)
(setq codex-ide-interface 'terminal
      codex-ide-terminal-backend 'ghostel)
(codex-ide-context-mode 1)
(global-set-key (kbd "C-c C-o") #'codex-ide-menu)
```

`C-c C-o` 또는 `M-x codex-ide-menu`로 실행합니다.
현재 파일/선택 영역을 붙인 질문은 `codex-ide-send-prompt`,
선택 영역 질문은 `codex-ide-send-region`을 사용합니다.

실제로 사용 중인 한글 키·폰트 보정 전체는 `config/current-init-excerpt.el`을 참고하세요.
Consolas/Malgun Gothic 등 폰트 설치 상태와 PGTK 여부에 따라 적용 결과가 달라질 수 있습니다.
이 설정 발췌본도 수정하지 않았으며, 다른 컴퓨터용으로 일반화한 설치 설정은 아닙니다.

기본 `codex-ide-sandbox` 값은 원본 그대로 `read-only`입니다.
작업 폴더 쓰기가 필요하면 Codex의 권한 흐름과 이 옵션을 환경에 맞게 설정하세요.
이 업로드를 위해 권한 우회나 자동 승인 설정을 추가하지 않았습니다.

## Claude Code IDE와의 관계

`claude-code-ide`를 기반/참고로 맞춰왔던 사용 흐름과 이후 보완 내용은
[변경 설명](CHANGES.ko.md)에 함께 기록했습니다.
현재 소스 헤더는 독립적인 Codex App Server JSON-RPC 클라이언트이며
`claude-code-ide`를 수정하지 않는다고 명시합니다. 따라서 이 저장소를
상류 `claude-code-ide` 자체의 패치나 포크라고 단정하지 않습니다.

별도로 만든 [Claude Code RHEL 8용 어댑터](https://github.com/gscho74/claude-code-ide-rhel8)와는
구분되는, 현재 사용 중인 **Codex용 원본**입니다.

## 스냅샷 확인

`SOURCE-SNAPSHOT.json`의 런타임 파일 SHA-256과 현재 파일을 비교할 수 있습니다.
공개 준비 과정에서 소스 동일성, Emacs 배치 로딩, Python 구문, MCP UTF-8 응답을 확인했습니다.
이 확인은 실제 모델 호출·승인·GUI 한글 입력의 전체 재검증을 의미하지 않습니다.

## 라이선스

원본 런타임 소스의 SPDX 표기는 MIT입니다. 기존 주석과 표기를 그대로 유지했습니다.
[LICENSE-NOTES.md](LICENSE-NOTES.md)를 참고하세요.
