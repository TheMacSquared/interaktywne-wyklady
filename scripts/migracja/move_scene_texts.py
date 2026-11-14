#!/usr/bin/env python3
"""Przenosi opisy kroków scen z serwera do dokumentu.

    scene_texts(input, output, "ID", list(...))   (w funkcji *_server)
  → argument texts = list(...) w wywołaniu widgetu, którego pierwszym argumentem jest "ID".

Użycie: move_scene_texts.py <katalog modules wykładu>
Funkcje pomocnicze widgetów (scene_widget, frames_widget, …) muszą przyjmować `texts`.
"""
import pathlib
import re
import sys


def match_paren(t, i):
    """Indeks nawiasu zamykającego dla '(' na pozycji i (z pominięciem napisów i komentarzy)."""
    depth = 0
    k = i
    quote = None
    while k < len(t):
        c = t[k]
        if quote:
            if c == "\\":
                k += 1
            elif c == quote:
                quote = None
        elif c in "\"'`":
            quote = c
        elif c == "#":
            k = t.index("\n", k)
            continue
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return k
        k += 1
    raise ValueError("brak nawiasu zamykającego")


def main(mod_dir):
    files = sorted(pathlib.Path(mod_dir).glob("*.R"))
    texts = {}
    sources = {f: f.read_text(encoding="utf-8") for f in files}
    # 1. wytnij wywołania scene_texts() i zapamiętaj listy opisów
    for f in files:
        t = sources[f]
        while True:
            # scene_texts() albo jej odpowiednik w wykładzie (np. exp_texts())
            m = re.search(r'^[ \t]*(?:scene|exp)_texts\(input, output, "([^"]+)",\s*', t, re.M)
            if not m:
                break
            open_i = t.index("(", m.start())
            close_i = match_paren(t, open_i)
            arg = t[m.end():close_i].strip()
            assert arg.startswith("list("), (f, m.group(1), arg[:40])
            texts[m.group(1)] = arg
            end = close_i + 1
            if t[end:end + 1] == "\n":
                end += 1
            t = t[:m.start()] + t[end:]
        sources[f] = t
    # 2. dopisz texts = list(...) do wywołania widgetu z tym id
    for wid, arg in texts.items():
        placed = False
        for f in files:
            t = sources[f]
            m = re.search(r'\b[A-Za-z_][A-Za-z0-9_.]*\(\s*"%s"' % re.escape(wid), t)
            if not m:
                continue
            open_i = t.index("(", m.start())
            close_i = match_paren(t, open_i)
            line_start = t.rfind("\n", 0, m.start()) + 1
            indent = re.match(r"[ \t]*", t[line_start:]).group(0) + "  "
            body = "\n".join((indent + l.strip()) if l.strip() else l for l in arg.split("\n"))
            before = t[:close_i].rstrip()
            t = before + ",\n" + indent + "texts = " + body.lstrip() + "\n" + indent[:-2] + t[close_i:]
            sources[f] = t
            placed = True
            break
        if not placed:
            raise SystemExit("Nie znaleziono widgetu dla %s" % wid)
    # 3. funkcje pomocnicze: scene_widget() przyjmuje texts, scene_texts() znika
    helper = re.compile(r"\) \{\n  lc_step_widget\(id,\n    title = title,\n    steps = steps,\n(?!    texts = texts,)")
    for f in files:
        t = sources[f]
        # także przy ponownym uruchomieniu, gdy opisy są już w wywołaniach widgetów
        if texts or any("texts = list(" in s for s in sources.values()):
            t = helper.sub(",\n                         texts = NULL) {\n  lc_step_widget(id,\n    title = title,\n    steps = steps,\n    texts = texts,\n", t)
        t = re.sub(r"(# Teksty kroków[^\n]*\n)?scene_texts <- function\(input, output, id, texts\) \{\n.*?\n\}\n", "", t, flags=re.S)
        sources[f] = t
    for f in files:
        f.write_text(sources[f], encoding="utf-8")
    print("przeniesiono opisy:", ", ".join(texts) or "brak")


if __name__ == "__main__":
    main(sys.argv[1])
