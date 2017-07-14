#!/usr/bin/env python3

from pathlib import *
import os
import csv
import subprocess


NAMES = ["arith", "untyped", "fulluntyped", "tyarith",
         "simplebool", "fullsimple", "bot", "fullerror",
         "rcdsubbot", "fullsub", "fullref", "equirec",
         "fullequirec", "fullisorec", "recon", "fullrecon",
         "fullpoly", "fullomega"]

NEW_ADDED = {
    'arith': ['bool', 'nat'],
    'untyped': ['varapp'],
    'fulluntyped': ['record', 'floatstring', 'let'],
    'tyarith': ['typedbool', 'typednat'],
    'simplebool': ['typed'],
    'fullsimple': ['extension', 'variant', 'typedrecord', 'unit'],
    'bot': ['top', 'bottom'],
    'fullerror': [],
    'rcdsubbot': [],
    'fullsub': [],
    'fullref': ['ref'],
    'equirec': ['rectype'],
    'fullequirec': [],
    'fullisorec': [],
    'recon': [],
    'fullrecon': [],
    'fullpoly': ['pack'],
    'fullomega': []
}

for k in NEW_ADDED.keys():
    assert(k in NAMES)


def sloc_components():
    ret = dict()
    root = Path('../tapl/src/main/scala/tapl/component/')
    for c in [x for x in root.iterdir() if x.is_dir()]:
        # print(c.name)
        result = subprocess.run(
            ["cloc", "--csv", "--quiet", c],
            stdout=subprocess.PIPE)
        result.check_returncode()
        reader = csv.DictReader(result.stdout.decode().strip().splitlines())
        rows = list(reader)
        assert(len(rows) == 1)
        for r in rows:
            # print(c.name, r['code'])
            ret[c.name] = int(r['code'])
    return ret


def sloc_languages():
    demo = 0
    ret = dict()
    root = Path('../tapl/src/main/scala/tapl/language/')
    for c in [x for x in root.iterdir() if x.is_dir()]:
        # print(c.name)
        result = subprocess.run(
            ["cloc", "--by-file", "--csv", "--quiet", c],
            stdout=subprocess.PIPE)
        result.check_returncode()
        reader = csv.DictReader(result.stdout.decode().strip().splitlines())
        s = 0
        for row in reader:
            file_name = Path(row['filename']).name
            if file_name == 'Test.scala':
                demo += int(row['code'])
                continue
            assert(file_name in [x + '.scala' for x in ['Alg', 'Print', 'Parse', 'Eval', 'Typer']])
            s += int(row['code'])
        # print(c.name, s)
        ret[c.name] = s
    # print('demo = {}'.format(demo))
    return ret


def sloc_comparison():
    ret = dict()
    root = Path('../comparison/src/main/scala/comp/tapl/')
    for c in [x for x in root.iterdir() if x.is_dir()]:
        result = subprocess.run(
            ["cloc", "--by-file", "--csv", "--quiet", c],
            stdout=subprocess.PIPE)
        result.check_returncode()
        reader = csv.DictReader(result.stdout.decode().strip().splitlines())
        s = 0
        for row in reader:
            file_name = Path(row['filename']).name
            if file_name == 'Demo.scala':
                continue
            assert(file_name in [x + '.scala' for x in ['Term', 'Parser', 'Evaluator']])
            s += int(row['code'])
        # print(c.name, s)
        ret[c.name] = s
    return ret


components = sloc_components()
languages = sloc_languages()
comparison = sloc_comparison()

assert(len(languages) == len(comparison) == len(NAMES))

tot_s = 0
tot_s2 = sum(comparison.values())

for name in NAMES:
    s = languages[name]
    for c in NEW_ADDED[name]:
        s += components[c]
    tot_s += s
    s2 = comparison[name]
    print('{} & {} & {} & {} & {:+.1%}'.format(','.join(NEW_ADDED[name]), name, s, s2, (s - s2) / s2))
print('& {} & {} & {} & {:+.1%}'.format('total', tot_s, tot_s2, (tot_s - tot_s2) / tot_s2))
