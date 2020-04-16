#!/bin/sh python3

import re


test_and_docs = [
        ('test/primitive.egi', 'docs/manual/primitive-functions.rst')
        ]


def template(func, examples):
    header = func + "\n   ::\n\n"
    return header + "".join(["      > " + code + "\n" for code in examples])


def gen_manual(d):
    ret = ""
    for k, vs in d.items():
        ret += template(k, vs) + "\n"
    return ret


def main():
    for (testfile, docfile) in test_and_docs:
        refs = {}

        with open(testfile, 'r') as f:
            for line in f:
                if line == "":
                    continue

                result = re.match(r'^assertEqual\s"(.+)"\s\((.+)\)\s(.+)', line)
                if result:
                    label = result.group(1)
                    target = result.group(2).strip()
                    expect = result.group(3).strip()

                    if expect[0] == '(' and expect[-1] == ')':
                        expect = expect[1:-1]

                    if label not in refs.keys():
                        refs[label] = ["{} --> {}".format(target, expect)]
                    else:
                        refs[label].append("{} --> {}".format(target, expect))

        with open(docfile, 'r') as f:
            doc = f.read()

        regexp = "\.\. BEGIN docsgen.*?\.\. END docsgen"
        new_manual = ".. BEGIN docsgen\n\n" + gen_manual(refs) + ".. END docsgen"
        new_doc = re.sub(regexp, new_manual, doc, flags = re.DOTALL | re.MULTILINE)

        with open(docfile, 'w') as f:
            f.write(new_doc)


if __name__ == "__main__":
    main()
