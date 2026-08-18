# -*- coding: utf-8 -*-
"""언어 파일 생성기. 각 언어 사전을 받아 src/XgbHmi.Core/Assets/lang/<code>.json 으로 쓴다."""
import json, os, sys

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
LANG_DIR = os.path.join(ROOT, "src", "XgbHmi.Core", "Assets", "lang")
KEYS_FILE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "i18n_keys.json")


def keys():
    with open(KEYS_FILE, encoding="utf-8") as f:
        return list(json.load(f).keys())


def write(code, table, fill=True):
    with open(KEYS_FILE, encoding="utf-8") as f:
        base = json.load(f)
    order = list(base.keys())
    missing = [k for k in order if k not in table]
    extra = [k for k in table if k not in order]
    out = {}
    for k in order:
        if k in table:
            out[k] = table[k]
        elif fill:
            out[k] = base[k]
    os.makedirs(LANG_DIR, exist_ok=True)
    with open(os.path.join(LANG_DIR, code + ".json"), "w", encoding="utf-8") as f:
        json.dump(out, f, ensure_ascii=False, indent=2)
        f.write("\n")
    print("%-8s keys=%d missing=%d extra=%d %s%s" % (
        code, len(out), len(missing), len(extra),
        ("MISSING:" + ",".join(missing[:6]) if missing else ""),
        ("  EXTRA:" + ",".join(extra[:6]) if extra else "")))


def merge(code, table):
    """이미 있는 언어 파일에 새 키만 덧붙인다(키 순서는 영어 카탈로그 기준)."""
    with open(KEYS_FILE, encoding="utf-8") as f:
        base = json.load(f)
    path = os.path.join(LANG_DIR, code + ".json")
    with open(path, encoding="utf-8") as f:
        current = json.load(f)
    current.update(table)
    out = {k: current.get(k, base[k]) for k in base.keys()}
    with open(path, "w", encoding="utf-8") as f:
        json.dump(out, f, ensure_ascii=False, indent=2)
        f.write("\n")
    filled = [k for k in base if k not in current]
    print("%-8s keys=%d english-fallback=%d" % (code, len(out), len(filled)))
