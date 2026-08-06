#!/usr/bin/env python3
"""Kompiluje katalog wezly/ do graf.json i waliduje przeciw ontologia.yaml.
Użycie: python3 kompiluj.py [katalog_bazy]   (domyślnie: katalog skryptu)
Zależność: pyyaml
"""
import json, re, sys
from pathlib import Path
import yaml

baza = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).parent
ont = yaml.safe_load((baza / "ontologia.yaml").read_text(encoding="utf-8"))
bledy, wezly, krawedzie = [], {}, []

def dziedzina_ok(spec, typ_zrodla, typ_celu):
    zr, cel = [s.strip() for s in spec.split("->")]
    return typ_zrodla in zr.split("|") and typ_celu in cel.split("|")

for plik in sorted((baza / "wezly").glob("*.md")):
    tekst = plik.read_text(encoding="utf-8")
    m = re.match(r"^---\n(.*?)\n---\n(.*)$", tekst, re.S)
    if not m:
        bledy.append(f"{plik.name}: brak frontmatter"); continue
    fm = yaml.safe_load(m.group(1))
    naglowek = re.search(r"^# (.+)$", m.group(2), re.M)
    nid = fm.get("id")
    if not nid: bledy.append(f"{plik.name}: brak id"); continue
    if nid in wezly: bledy.append(f"{plik.name}: zduplikowane id '{nid}'"); continue
    if fm.get("typ") not in ont["typy_wezlow"]:
        bledy.append(f"{nid}: nieznany typ '{fm.get('typ')}'")
    if fm.get("status") not in ont["statusy"]:
        bledy.append(f"{nid}: nieznany status '{fm.get('status')}'")
    wezly[nid] = {"id": nid, "typ": fm.get("typ"),
                  "label": naglowek.group(1) if naglowek else nid,
                  "status": fm.get("status"), "powstanie": fm.get("powstanie"),
                  "tagi": fm.get("tagi", []), "plik": plik.name,
                  "_relacje": fm.get("relacje") or []}

for w in wezly.values():
    for r in w.pop("_relacje"):
        typ, cel = r.get("typ"), r.get("cel")
        if typ not in ont["typy_relacji"]:
            bledy.append(f"{w['id']}: nieznana relacja '{typ}'"); continue
        if cel not in wezly:
            bledy.append(f"{w['id']}: relacja {typ} -> nieistniejący cel '{cel}'"); continue
        if not dziedzina_ok(ont["typy_relacji"][typ]["dziedzina"], w["typ"], wezly[cel]["typ"]):
            bledy.append(f"{w['id']} -{typ}-> {cel}: naruszenie dziedziny "
                         f"({w['typ']} -> {wezly[cel]['typ']})")
        krawedzie.append({"source": w["id"], "target": cel, "typ": typ,
                          "nota": r.get("nota")})

for typ, spec in ont["typy_relacji"].items():          # acykliczność
    if not spec.get("acykliczna"): continue
    nast = {}
    for k in krawedzie:
        if k["typ"] == typ: nast.setdefault(k["source"], []).append(k["target"])
    stan = {}
    def dfs(v, sciezka):
        stan[v] = 1
        for u in nast.get(v, []):
            if stan.get(u) == 1:
                bledy.append(f"cykl w '{typ}': {' -> '.join(sciezka + [u])}")
            elif stan.get(u) is None:
                dfs(u, sciezka + [u])
        stan[v] = 2
    for v in list(nast):
        if stan.get(v) is None: dfs(v, [v])

wyj = {"meta": {"ontologia": "ontologia.yaml", "wezlow": len(wezly),
                "krawedzi": len(krawedzie)},
       "nodes": list(wezly.values()), "edges": krawedzie}
(baza / "graf.json").write_text(json.dumps(wyj, ensure_ascii=False, indent=2),
                                encoding="utf-8")
print(f"Węzłów: {len(wezly)}, krawędzi: {len(krawedzie)} -> graf.json")
statusy = {}
for w in wezly.values(): statusy[w["status"]] = statusy.get(w["status"], 0) + 1
print("Statusy:", ", ".join(f"{k}: {v}" for k, v in sorted(statusy.items())))
if bledy:
    print(f"\nBŁĘDY ({len(bledy)}):"); [print(" -", b) for b in bledy]; sys.exit(1)
print("Walidacja: OK")
