#!/usr/bin/env python3

from random import randrange, choice
import string
from copy import copy

BOOL_TERM, NAT_TERM, TYPE = 0, 1, 2
SORT = [BOOL_TERM, NAT_TERM, TYPE]

NAMES = ["Arith", "Untyped", "FullUntyped", "TyArith", "SimpleBool",
         "FullSimple", "Bot", "FullRef", "FullError", "RcdSubBot", "FullSub",
         "FullEquiRec", "FullIsoRec", "EquiRec", "Recon", "FullRecon",
         "FullPoly", "FullOmega"]


class NonTerminal:
    def __init__(self, template):
        self.template = template

    def gen(self, depth, g):
        temp = copy(self.template)
        for i in range(0, len(temp)):
            if temp[i] in SORT:
                d = randrange(max(0, depth - 2), depth)
                temp[i] = g.gen(temp[i], d, g)
        return '(' + " ".join(temp) + ')'


class Generator:
    def __init__(self, terminals, non_terminals):
        self.terminals = terminals
        self.non_terminals = non_terminals

    def __add__(self, other):
        terminals = copy(self.terminals)
        non_terminals = copy(self.non_terminals)
        for i in SORT:
            terminals[i] += other.terminals[i]
            non_terminals[i] += other.non_terminals[i]
        return Generator(terminals, non_terminals)

    def gen(self, typ, depth, g):
        if depth < 1 or len(self.non_terminals[typ]) == 0:
            return choice(self.terminals[typ])
        else:
            nt = choice(self.non_terminals[typ])
            return nt.gen(depth, g)


class EGenerator(Generator):
    def __init__(self, bool_ts, bool_nts, nat_ts, nat_nts):
        terminals = [bool_ts, nat_ts, []]
        non_terminals = [
            [NonTerminal(x) for x in bool_nts],
            [NonTerminal(x) for x in nat_nts],
            []
        ]
        super().__init__(terminals, non_terminals)


def gBool():
    bool_ts = ['true', 'false']
    bool_nts = [['if', BOOL_TERM, 'then', BOOL_TERM, 'else', BOOL_TERM]]
    nat_ts = []
    nat_nts = [['if', BOOL_TERM, 'then', NAT_TERM, 'else', NAT_TERM]]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gNat():
    bool_ts = []
    bool_nts = [['iszero', NAT_TERM]]
    nat_ts = ['0', '1']
    nat_nts = [
        ['succ', NAT_TERM],
        ['pred', NAT_TERM]
    ]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gArith():
    return gBool() + gNat()


def gUntyped():
    # TODO
    bool_ts = []
    bool_nts = [
        ['((\\x.\\y.x)', BOOL_TERM, ')', BOOL_TERM],
        ['((\\x.\\y.y)', BOOL_TERM, ')', BOOL_TERM],
        ['(', '\\f.((f', BOOL_TERM, ')', BOOL_TERM, ')', ')', '(\\a.\\b.a)'],
        ['(', '\\f.((f', BOOL_TERM, ')', BOOL_TERM, ')', ')', '(\\a.\\b.b)']
    ]
    nat_ts = []
    nat_nts = [
        ['((\\x.\\y.x)', NAT_TERM, ')', BOOL_TERM],
        ['((\\x.\\y.y)', BOOL_TERM, ')', NAT_TERM],
        ['(', '\\f.((f', NAT_TERM, ')', NAT_TERM, ')', ')', '(\\a.\\b.a)'],
        ['(', '\\f.((f', NAT_TERM, ')', NAT_TERM, ')', ')', '(\\a.\\b.b)']
    ]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gRecord():
    bool_ts = []
    bool_nts = [
        ['{a =', BOOL_TERM, ', b =', NAT_TERM, '}.a'],
        ['{a =', BOOL_TERM, ', b =', BOOL_TERM, '}.b'],
        ['{a =', NAT_TERM, ', b =', BOOL_TERM, '}.b'],
        ['{a =', NAT_TERM, ', b =', BOOL_TERM, ', c = ', NAT_TERM, '}.b']
    ]
    nat_ts = []
    nat_nts = [
        ['{a =', BOOL_TERM, ', b =', NAT_TERM, '}.b'],
        ['{a =', NAT_TERM, ', b =', BOOL_TERM, '}.a'],
        ['{a =', NAT_TERM, ', b =', NAT_TERM, '}.b'],
        ['{a =', NAT_TERM, ', b =', BOOL_TERM, ', c =', NAT_TERM, '}.c']
    ]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gLet():
    bool_ts = []
    bool_nts = [
        ['let u =', NAT_TERM, 'in', BOOL_TERM],
        ['let u =', BOOL_TERM, 'in', BOOL_TERM],
        ['let u =', BOOL_TERM, 'in', 'u'],
    ]
    nat_ts = []
    nat_nts = [
        ['let u = ', NAT_TERM, 'in', NAT_TERM],
        ['let u = ', BOOL_TERM, 'in', NAT_TERM],
        ['let u = ', NAT_TERM, 'in', 'u'],
    ]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gFullUntyped():
    return gArith() + gUntyped() + gRecord() + gLet()


class ETGenerator(Generator):
    def __init__(self, bool_ts, bool_nts, nat_ts, nat_nts, ty_ts):
        terminals = [bool_ts, nat_ts, ty_ts]
        non_terminals = [
            [NonTerminal(x) for x in bool_nts],
            [NonTerminal(x) for x in nat_nts],
            []
        ]
        super().__init__(terminals, non_terminals)


def gTypedBool():
    tmp = ETGenerator([], [], [], [], ['Bool'])
    return tmp + gBool()


def gTypedNat():
    tmp = ETGenerator([], [], [], [], ['Nat'])
    return tmp + gNat()


def gTyArith():
    return gTypedBool() + gTypedNat()


def gTyped():
    bool_ts = []
    bool_nts = [
        ['((\\x:', TYPE, '.\\y:', TYPE, '.x)', BOOL_TERM, ')', BOOL_TERM],
        ['((\\x:', TYPE, '.\\y:', TYPE, '.y)', BOOL_TERM, ')', BOOL_TERM],
        ['(', '\\f:', TYPE, '.((f', BOOL_TERM, ')', BOOL_TERM, ')', ')', '(\\a:', TYPE, '.\\b:', TYPE, '.a)'],
        ['(', '\\f:', TYPE, '.((f', BOOL_TERM, ')', BOOL_TERM, ')', ')', '(\\a:', TYPE, '.\\b:', TYPE, '.b)']
    ]
    nat_ts = []
    nat_nts = [
        ['((\\x:', TYPE, '.\\y:', TYPE, '.x)', NAT_TERM, ')', BOOL_TERM],
        ['((\\x:', TYPE, '.\\y:', TYPE, '.y)', BOOL_TERM, ')', NAT_TERM],
        ['(', '\\f:', TYPE, '.((f', NAT_TERM, ')', NAT_TERM, ')', ')', '(\\a:', TYPE, '.\\b:', TYPE, '.a)'],
        ['(', '\\f:', TYPE, '.((f', NAT_TERM, ')', NAT_TERM, ')', ')', '(\\a:', TYPE, '.\\b:', TYPE, '.b)']
    ]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gSimpleBool():
    return gTyped() + gTypedBool()


def gTypedRecord():
    # type_nts = [['{', IDENT, ':', TYPE, ',', IDENT, ':', TYPE, '}']]
    # tmp = ETGenerator([], [], [], type_nts)
    return gRecord()


def gVariant():
    bool_ts = []
    bool_nts = [
        ['case', '<r =', BOOL_TERM, '>', 'as', TYPE, 'of',
         '<', 'l', '=', 'x', '>', '=>', 'x', '|',
         '<', 'r', '=', 'y', '>', '=>', 'y']
    ]
    nat_ts = []
    nat_nts = [
        ['case', '<l =', NAT_TERM, '>', 'as', TYPE, 'of',
         '<', 'l', '=', 'x', '>', '=>', 'x', '|',
         '<', 'r', '=', 'y', '>', '=>', 'y']
    ]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gExtension():
    bool_ts = []
    bool_nts = [['fix', '(\\x:', TYPE, '.', BOOL_TERM, ')']]
    nat_ts = []
    nat_nts = [['fix', '(\\x:', TYPE, '.', NAT_TERM, ')']]
    return ETGenerator(bool_ts, bool_nts, nat_ts, nat_nts, ['Unit'])


def gSimple():
    return gTyArith() + gTyped() + gLet() + gTypedRecord() + gExtension()


def gFullSimple():
    return gSimple() + gVariant()


def gTopBot():
    return ETGenerator([], [], [], [], ['Top', 'Bot'])


def gBot():
    return gTyped() + gTopBot()


def gRef():
    bool_ts = []
    bool_nts = [
        ['(\\r:', TYPE, '. if !r then', BOOL_TERM, 'else {r :=', BOOL_TERM, '; ', BOOL_TERM, '}) (ref ', BOOL_TERM, ')']
    ]
    nat_ts = []
    nat_nts = [
        ['(\\r:', TYPE, '. if iszero(!r) then', NAT_TERM, 'else {r :=', BOOL_TERM, '; ', NAT_TERM, '}) (ref ', NAT_TERM, ')']
    ]
    return ETGenerator(bool_ts, bool_nts, nat_ts, nat_nts, ['Ref Bool', 'Ref Nat'])


def gFullRef():
    return gFullSimple() + gTopBot() + gRef()


def gError():
    bool_ts = ['error']
    bool_nts = [['try', BOOL_TERM, 'with', BOOL_TERM]]
    nat_ts = ['error']
    nat_nts = [['try', NAT_TERM, 'with', NAT_TERM]]
    return EGenerator(bool_ts, bool_nts, nat_ts, nat_nts)


def gFullError():
    return gBot() + gTypedBool() + gError()


def gRcdSubBot():
    return gBot() + gTypedRecord()


def gFullSub():
    return gSimple()


def gRecType():
    return ETGenerator([], [], [], [], ['Rec X. X'])


def gFullEquiRec():
    return gFullSimple() + gRecType()


def gFold():
    bool_nts = [
        ['unfold', '[', TYPE, ']', '(fold [', TYPE, ']', BOOL_TERM, ')'],
        ['(\\x:', TYPE, '.', 'unfold [', TYPE, '] x)', '(fold [', TYPE, ']', BOOL_TERM, ')']
    ]
    nat_nts = [
        ['unfold', '[', TYPE, ']', '(fold [', TYPE, ']', NAT_TERM, ')'],
        ['(\\x:', TYPE, '.', 'unfold [', TYPE, '] x)', '(fold [', TYPE, ']', NAT_TERM, ')']
    ]
    return EGenerator([], bool_nts, [], nat_nts)


def gFullIsoRec():
    return gFullEquiRec() + gFold()


def gEquiRec():
    return gTyped() + gRecType()


def gRecon():
    return gTyped() + gTyArith()


def gFullRecon():
    return gRecon() + gLet()


def gPack():
    bool_nts = [
        ['let', '{X, x} =', '{', '*', TYPE, ',', BOOL_TERM, '}', 'as', TYPE, 'in', BOOL_TERM],
        ['let', '{X, x} =', '{', '*', TYPE, ',', BOOL_TERM, '}', 'as', TYPE, 'in (if x then', BOOL_TERM, 'else', BOOL_TERM, ')'],
    ]
    nat_nts = [
        ['let', '{X, x} =', '{', '*', TYPE, ',', NAT_TERM, '}', 'as', TYPE, 'in', NAT_TERM],
        ['let', '{X, x} =', '{', '*', TYPE, ',', NAT_TERM, '}', 'as', TYPE, 'in (if iszero(x) then', NAT_TERM, 'else', NAT_TERM, ')'],
    ]
    return EGenerator([], bool_nts, [], nat_nts)


def gPoly():
    bool_nts = [
        ['(\\X.', BOOL_TERM, ') [', TYPE, ']']
    ]
    nat_nts = [
        ['(\\X.', NAT_TERM, ') [', TYPE, ']']
    ]
    return EGenerator([], bool_nts, [], nat_nts)


def gFullPoly():
    return gSimple() + gPoly() + gPack()


def gOmega():
    bool_nts = [
        ['(\\X: Star', '.', BOOL_TERM, ') [', TYPE, ']']
    ]
    nat_nts = [
        ['(\\X: Star', '.', NAT_TERM, ') [', TYPE, ']']
    ]
    return EGenerator([], bool_nts, [], nat_nts)


def gFullOmega():
    return gSimple() + gRef() + gPack() + gOmega()


def gen_by_id(index, times, depth):
    gens = [
        gArith(),
        gUntyped(),
        gFullUntyped(),
        gTyArith(),
        gSimpleBool(),
        gFullSimple(),
        gBot(),
        gFullRef(),
        gFullError(),
        gRcdSubBot(),
        gFullSub(),
        gFullEquiRec(),
        gFullIsoRec(),
        gEquiRec(),
        gRecon(),
        gFullRecon(),
        gFullPoly(),
        gFullOmega()
    ]
    assert(len(gens) == len(NAMES))
    ret = []
    g = gens[index]
    for i in range(0, times):
        # TODO
        t = g.gen(BOOL_TERM, depth, g)
        ret.append(t)
    return ret


def main():
    NUM_CASES = 100
    MAX_DEPTH = 6
    for i in range(len(NAMES)):
        if i == 1 or i == 6 or i == 9 or i == 13:
            continue
        name = NAMES[i].lower()
        print(name)
        cs = gen_by_id(i, NUM_CASES, MAX_DEPTH)
        cases = [x + '\n' for x in cs]
        # print(cases)
        with open('{}.txt'.format(name), 'w') as f:
            f.writelines(cases)


if __name__ == '__main__':
    main()
