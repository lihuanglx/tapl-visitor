NAMES = ["arith", "untyped", "fulluntyped", "tyarith",
         "simplebool", "fullsimple", "bot", "fullerror",
         "rcdsubbot", "fullsub", "fullref", "equirec",
         "fullequirec", "fullisorec", "recon", "fullrecon",
         "fullpoly", "fullomega"]


def pattern(name):
    r = '''
    test("{0}") {{
      compare("{0}", visitor.{0}.Test.benchmarkParsing, comp.parsing.{0}.Parser.input)
    }}'''.format(name)
    return r


for name in NAMES:
    print(pattern(name))
