from pathlib import Path
import re
current_path = Path(__file__).resolve().parent

l = (current_path.parent / "_chapters").glob("*.md")

pattern = re.compile(r"\*(.*?)\*:\s*(.*)")

all_defs = []
current_terms = set()


for file in l:
    flag = False
    for l in file.read_text().split("\n"):
        if not flag and l.strip() != "<div class=\"glossary\" markdown=\"1\">":
            continue
        
        flag = True
        
        matches = pattern.match(l)
        if matches is None:
            continue
        term, definiton = matches.groups()
        
        if term not in current_terms:
            current_terms.add(term)
            all_defs.append((term, definiton))
        
str_rep = []
for t, d in all_defs:
    str_rep.append(f"- term: {t}")
    str_rep.append(f"  definition: {d}")
    
outpath = current_path.parent / "_data" / "glossary.yml"
outpath.write_text("\n".join(str_rep))