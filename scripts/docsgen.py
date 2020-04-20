import re
from typing import List, Tuple

test_and_docs = [
          ('test/primitive.egi', 'docs/manual/primitive-functions.rst')
        , ('test/lib/core/assoc.egi', 'docs/manual/lib/core/assoc.rst')
        , ('test/lib/core/base.egi', 'docs/manual/lib/core/base.rst')
        , ('test/lib/core/collection.egi', 'docs/manual/lib/core/collection.rst')
        , ('test/lib/core/maybe.egi', 'docs/manual/lib/core/maybe.rst')
        , ('test/lib/core/number.egi', 'docs/manual/lib/core/number.rst')
        , ('test/lib/core/order.egi', 'docs/manual/lib/core/order.rst')
        , ('test/lib/core/string.egi', 'docs/manual/lib/core/string.rst')
        ]


def template(func: str, examples: List[str]):
    header = func + "\n   ::\n\n"
    programs = ""

    for target, expect in examples:
        target = target.replace('\\', '\\\\')
        expect = expect.replace('\\', '\\\\')

        targets = target.split('\n')

        if len(targets) > 1:
            code = ' ' * 6 + targets[0] + '\n'
            code += "".join([' ' * 6 + t + '\n' for t in targets[1:]])
            code += "      ---> {}\n".format(expect)

        else:
            code = "      {} ---> {}\n".format(target, expect)

            # Description comes in 2 lines if it's too long
            if len(code) > 60:
                code  = "      {}\n".format(target)
                code += "      ---> {}\n".format(expect)

        programs += code

    return header + programs


def gen_manual(d):
    ret = ""
    for k, vs in d.items():
        ret += template(k, vs) + "\n"
    return ret


def parse_string(s: str) -> Tuple[str, str]:
    result = re.match(r'(".*")(.*)', s, flags=re.DOTALL)
    assert result
    return result.group(1), result.group(2)


def parse_parenthesized_atom(s: str):
    n_paren = 0
    n_comma = 0
    in_string = False
    j = len(s) - 1

    for i, c in enumerate(s):
        if in_string:
            if c == '"':
                in_string = False
        else:
            if c == '"' and not in_string:
                in_string = True
            if c == '(' or c == '[':
                n_paren += 1
            if c == ')' or c == ']':
                n_paren -= 1
            if c == ',' and n_paren == 1:
                n_comma += 1

        if n_paren == 0:
            j = i
            break

    return (s[:j + 1], n_comma, s[j + 1:].strip())


def parse_atom(s: str):
    s = s.strip()

    if s == "":
        return ""

    if s[0] == '(':
        atom, n, rest = parse_parenthesized_atom(s)
        if n == 0:
            return atom[1:-1], rest
        return atom, rest

    if s[0] == '[':
        atom, _, rest = parse_parenthesized_atom(s)
        return atom, rest

    if s[0] == '"':
        return parse_string(s)

    result = re.match(r'(\S+)(.*)', s, flags=re.DOTALL)
    assert result
    return result.group(1), result.group(2)


def parse_assert(s: str):
    result = re.match(r'^assertEqual\s+\"(\S+)\"\s+(.+)$', s, flags=re.DOTALL)

    if result is None:
        return None

    try:
        label = result.group(1)
        s = result.group(2)
        target, s = parse_atom(s)
        expect, s = parse_atom(s)
        assert expect != ""
        return label, target, expect
    except Exception:
        return None


def main():
    for (testfile, docfile) in test_and_docs:
        refs = {}

        with open(testfile, 'r') as f:
            prev_lines = []

            for line in f:
                if line == "":
                    continue

                result = parse_assert("".join(prev_lines) + line)

                if result:
                    prev_lines = []
                    label, target, expect = result

                    if label not in refs.keys():
                        refs[label] = [(target, expect)]
                    else:
                        refs[label].append((target, expect))

                else:
                    if line[:11] == 'assertEqual':
                        prev_lines = [line]
                        continue

                    if len(prev_lines) > 0 and line[0:2] == "  ":
                        prev_lines.append(line)
                        continue

                    prev_lines = []


        with open(docfile, 'r') as f:
            doc = f.read()

        regexp = "\.\. BEGIN docsgen.*?\.\. END docsgen"
        new_manual = ".. BEGIN docsgen\n\n" + gen_manual(refs) + ".. END docsgen"
        new_doc = re.sub(regexp, new_manual, doc, flags = re.DOTALL | re.MULTILINE)

        with open(docfile, 'w') as f:
            f.write(new_doc)


if __name__ == "__main__":
    main()
