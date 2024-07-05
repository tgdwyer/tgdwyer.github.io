from pathlib import Path
import re
from collections import OrderedDict

current_path = Path(__file__).resolve().parent

l = (current_path.parent / "_chapters").glob("*.md")


all_defs = []
current_terms = {}

chapter_dict = OrderedDict()
data = (current_path.parent / "_data" / "chapters.yml").read_text()

pattern = re.compile(r"- title:\s*(?P<title>.+)\s+url:\s*/(?P<url>[^/]+)/")

# Find all matches in the data
matches = pattern.finditer(data)

for match in matches:
    title = match.group("title")
    url = match.group("url")
    chapter_dict[title] = url

_chapters = list(chapter_dict.keys())
pattern = re.compile(r"\*(.*?)\*:\s*(.*)")

for file in l:
    flag = False
    title = None
    for l in file.read_text().split("\n"):

        if "title: " in l:
            title = l.split(":")[1].strip()[1:-1]

        if not flag and l.strip() != '## Glossary':
            continue

        assert title is not None
        flag = True

        matches = pattern.match(l)
        if matches is None:
            continue
        term, definition = matches.groups()

        if term not in current_terms:
            current_terms[term] = (title, len(all_defs))
            all_defs.append((term, definition, title))
        else:
            if _chapters.index(title) < _chapters.index(current_terms[term][0]):
                all_defs[current_terms[term][1]] = (term, definition, title)
                current_terms[term] = (title, current_terms[term][1])


str_rep = []
for t, d, first in all_defs:
    str_rep.append(f"- term: {t}")
    str_rep.append(f"  definition: {d}")
    str_rep.append(f"  first_appeared: {chapter_dict[first]}")

outpath = current_path.parent / "_data" / "glossary.yml"
outpath.write_text("\n".join(str_rep))
