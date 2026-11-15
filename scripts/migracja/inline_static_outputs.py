#!/usr/bin/env python3
"""Przenosi treść stałych wyjść serwera do dokumentu.

    output$ID <- renderUI(WYRAŻENIE)        (w funkcji *_server; WYRAŻENIE nie czyta input$…)
    exercise_solution_server(output, "ID", function() WYRAŻENIE)   (drugi obsługiwany wzorzec)
    … uiOutput("ID") …                      (w opisie rozdziału)
  →
    ID_ui <- function() WYRAŻENIE           (przed instrukcją najwyższego poziomu, która zawiera uiOutput)
    … ID_ui() …

Użycie: inline_static_outputs.py <katalog modules wykładu> <ID> [<ID> …]
Funkcje pomocnicze zdefiniowane wewnątrz *_server, z których korzysta WYRAŻENIE,
trzeba przenieść ręcznie na poziom modułu (budowanie zgłosi brakującą nazwę).
"""
import pathlib
import re
import sys


def match_paren(t, i):
    """Indeks nawiasu zamykającego dla '(' na pozycji i (z pominięciem napisów i komentarzy)."""
    depth, k, quote = 0, i, None
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
        elif c in "([{":
            depth += 1
        elif c in ")]}":
            depth -= 1
            if depth == 0:
                return k
        k += 1
    raise ValueError("brak nawiasu zamykającego")


def dedent(text, n):
    return "\n".join(line[n:] if line[:n].strip() == "" else line for line in text.split("\n"))


def main():
    modules = pathlib.Path(sys.argv[1])
    ids = sys.argv[2:]
    files = {p: p.read_text(encoding="utf-8") for p in sorted(modules.glob("*.R"))}
    for out_id in ids:
        pattern = re.compile(r"^([ \t]*)output\$" + re.escape(out_id) + r" <- renderUI\(", re.M)
        source = next(((p, m) for p, t in files.items() for m in [pattern.search(t)] if m), None)
        if source is None:
            # Drugi wzorzec: exercise_solution_server(output, "ID", function() { … }).
            pattern = re.compile(r"^([ \t]*)exercise_solution_server\(output, \"" + re.escape(out_id) + r"\", function\(\) ", re.M)
            source = next(((p, m) for p, t in files.items() for m in [pattern.search(t)] if m), None)
        if source is None:
            sys.exit(f"{out_id}: nie znaleziono output${out_id} <- renderUI(")
        p, m = source
        t = files[p]
        # Nawias wywołania renderUI( albo exercise_solution_server( — treść to reszta argumentów.
        close = match_paren(t, t.index("(", m.start() + len(m.group(1))))
        expr = t[m.end():close].strip()
        if re.search(r"\binput\$", expr):
            sys.exit(f"{out_id}: wyrażenie czyta input$…, nie jest stałe")
        end = close + 1
        while end < len(t) and t[end] in " \t":
            end += 1
        if end < len(t) and t[end] == "\n":
            end += 1
        if t[end:end + 1] == "\n":       # pusty wiersz po usuniętej instrukcji
            end += 1
        files[p] = t[:m.start()] + t[end:]
        body = dedent(expr, len(m.group(1)))
        definition = f"# Treść stała (dawniej output${out_id} na serwerze): składana przy budowaniu strony.\n{out_id}_ui <- function() {body}\n\n"

        call = f'uiOutput("{out_id}")'
        target = next((q for q, s in files.items() if call in s), None)
        if target is None:
            sys.exit(f"{out_id}: nie znaleziono {call}")
        s = files[target]
        at = s.index(call)
        # Początek instrukcji najwyższego poziomu zawierającej wywołanie.
        starts = [x.start() for x in re.finditer(r"^[A-Za-z_.][A-Za-z0-9_.]* <- ", s[:at], re.M)]
        top = starts[-1]
        # Komentarze bezpośrednio nad instrukcją zostają przy niej.
        lines = s[:top].split("\n")
        k = len(lines) - 1
        while k > 0 and lines[k - 1].startswith("#"):
            k -= 1
        top = len("\n".join(lines[:k])) + (1 if k > 0 else 0)
        s = s[:top] + definition + s[top:]
        files[target] = s.replace(call, f"{out_id}_ui()")
        print(f"{out_id}: {p.name} → {target.name}")
    for p, t in files.items():
        p.write_text(t, encoding="utf-8")


if __name__ == "__main__":
    main()
