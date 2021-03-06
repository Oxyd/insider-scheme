#!/usr/bin/env python3

from collections import namedtuple
from enum import IntEnum
import random
import argparse
from pathlib import Path
import re

Character = namedtuple(
    'Character',
    ['code_point', 'name', 'general_category', 'canonical_combining_class', 'bidi_class',
     'decomposition', 'decimal_value', 'digit_value', 'numeric_value',
     'bidi_mirrored', 'unicode_1_name', 'iso_comment',
     'simple_uppercase', 'simple_lowercase', 'simple_titlecase']
)

UnicodeDatabase = namedtuple(
    'UnicodeDatabase',
    ['code_points', 'property_list', 'derived_core_properties', 'case_folding', 'special_casing']
)

def parse_character(s):
    c = Character._make(s.split(';'))
    return (int(c.code_point, 16), c)


def is_start_of_range(c):
    return c.name.endswith(', First>')


def make_range(start, end, c):
    return [(i, c._replace(code_point='{0:X}'.format(i))) for i in range(start, end + 1)]


def read_characters(data_path):
    result = {}

    with open(data_path, 'r') as f:
        while True:
            line = f.readline()
            if not line:
                break

            (codepoint, c) = parse_character(line)

            if not is_start_of_range(c):
                result[codepoint] = c
            else:
                (end_codepoint, _) = parse_character(f.readline())

                for cp, c in make_range(codepoint, end_codepoint, c):
                    result[cp] = c

    return result


single_character_property_re = re.compile(r'([0-9A-F]+) *; ([a-zA-Z_]+) #.*')
range_character_property_re = re.compile(r'([0-9A-F]+)\.\.([0-9A-F]+) *; ([a-zA-Z_]+) #.*')

def read_property_list(path):
    result = {}

    def add_property(cp, prop):
        if cp not in result:
            result[cp] = []
        result[cp].append(prop)


    with open(path, 'r') as f:
        for line in f:
            single_match = single_character_property_re.match(line)
            if single_match:
                add_property(int(single_match.group(1), 16), single_match.group(2))
            else:
                range_match = range_character_property_re.match(line)
                if range_match:
                    begin = int(range_match.group(1), 16)
                    end = int(range_match.group(2), 16)
                    prop = range_match.group(3)
                    for cp in range(begin, end + 1):
                        add_property(cp, prop)

    return result


SpecialCasing = namedtuple(
    'SpecialCasing',
    ['code_point', 'upper', 'conditions']
)


def parse_single_code_point(s):
    return int(s, 16)


def parse_code_point_list(s):
    return [parse_single_code_point(cp) for cp in s.split()]


def filter_comments(elements):
    if elements[-1].strip().startswith('#'):
        return elements[: len(elements) - 1]
    else:
        return elements


def parse_special_casing_line(line):
    elements = filter_comments(line.split(';'))

    if len(elements) == 4:
        conditions = []
    else:
        assert len(elements) == 5
        conditions = elements[4].strip()

    return SpecialCasing(parse_single_code_point(elements[0]),
                         parse_code_point_list(elements[3]),
                         conditions)


def read_special_casing(path):
    result = {}

    with open(path, 'r') as f:
        for line in f:
            if len(line.strip()) == 0 or line.startswith('#'):
                continue

            casing = parse_special_casing_line(line)
            if len(casing.conditions) == 0:
                result[casing.code_point] = casing

    return result



class CharacterCaseFolding:
    def __init__(self):
        self.common = None
        self.full = []


case_folding_re = re.compile(r'([0-9A-F]+); ([CFST]); ([0-9A-F ]+);')

def read_case_folding(path):
    result = {}

    def add_common(cp, common):
        result.setdefault(cp, CharacterCaseFolding()).common = common

    def add_full(cp, full):
        result.setdefault(cp, CharacterCaseFolding()).full = full

    with open(path, 'r') as f:
        for line in f:
            m = case_folding_re.match(line)
            if m:
                cp = parse_single_code_point(m.group(1))

                if m.group(2) == 'C':
                    add_common(cp, parse_single_code_point(m.group(3)))

                elif m.group(2) == 'F':
                    add_full(cp, parse_code_point_list(m.group(3)))

    return result


class HashFunction:
    int_max = 2**64
    p = 18015766095129967273

    def __init__(self, a, b, n):
        self.a = a
        self.b = b % HashFunction.p
        self.n = n

    def __call__(self, x):
        def add(x, y):
            return (x + y) % HashFunction.int_max

        def mul(x, y):
            return (x * y) % HashFunction.int_max

        return (add(mul(self.a, x), self.b) % self.p) % self.n


def make_random_hash_fn(n):
    return HashFunction(random.randint(1, HashFunction.p - 1),
                        random.randint(0, HashFunction.p - 1),
                        n)


class Graph:
    def __init__(self, n_vertices):
        self.num_vertices = n_vertices
        self.num_edges = 0
        self.edges = {i: set() for i in range(n_vertices)}


    def add_edge(self, u, v, edge_label):
        self.num_edges += 1
        self.edges[u].add((v, edge_label))
        self.edges[v].add((u, edge_label))


    def has_edge(self, u, v):
        for w, _ in self.edges[u]:
            if w == v:
                return True
        return False


def build_graph(data, n_vertices):
    assert n_vertices >= len(data) + 1

    f1 = make_random_hash_fn(n_vertices)
    f2 = make_random_hash_fn(n_vertices)

    g = Graph(n_vertices)
    for (i, x) in enumerate(data):
        u = f1(x)
        v = f2(x)

        if g.has_edge(u, v):
            return None

        g.add_edge(u, v, i)

    return (f1, f2, g)


def assign_g(graph, g, start):
    assert g[start] == -1

    g[start] = 0
    closed = set()
    stack = [(start, None)]

    while len(stack) > 0:
        (v, parent) = stack.pop()

        assert parent is None or g[parent] != -1
        assert g[v] != -1

        closed.add(v)

        for (u, i) in graph.edges[v]:
            if u != parent:
                if u in closed:
                    return False

                g[u] = (i - g[v]) % (graph.num_edges + 1)
                stack.append((u, v))

    return True


def build_g(graph):
    g = [-1] * graph.num_vertices

    for start in range(0, graph.num_vertices):
        if g[start] == -1:
            success = assign_g(graph, g, start)

            if not success:
                return None

    return g


class PerfectHashFunction:
    def __init__(self, f1, f2, g, num_elems):
        self.f1 = f1
        self.f2 = f2
        self.g = g
        self.num_elems = num_elems


    def __call__(self, x):
        return (self.g[self.f1(x)] + self.g[self.f2(x)]) % (self.num_elems + 1)


def build_perfect_hash(data):
    n = len(data) + 1
    attempts = 0
    max_attempts = 16

    def increase_n():
        nonlocal attempts
        nonlocal n

        attempts += 1

        if attempts >= max_attempts:
            n = max(n + 1, int(n * 1.05))
            attempts = 0


    while True:
        if attempts == 0:
            print('Graph size =', n)

        r = build_graph(data, n)
        if not r:
            increase_n()
            continue

        (f1, f2, graph) = r
        g = build_g(graph)

        if not g:
            increase_n()
            continue

        return PerfectHashFunction(f1, f2, g, len(data))


class CodePointAttribute(IntEnum):
    Numeric = 0
    LowerCase = 1
    UpperCase = 2
    Alphabetic = 3
    WhiteSpace = 4
    CasedLetter = 5
    CaseIgnorable = 6


def format_attribute(c):
    return {CodePointAttribute.Numeric: 'code_point_attribute::numeric',
            CodePointAttribute.LowerCase: 'code_point_attribute::lower_case',
            CodePointAttribute.UpperCase: 'code_point_attribute::upper_case',
            CodePointAttribute.Alphabetic: 'code_point_attribute::alphabetic',
            CodePointAttribute.WhiteSpace: 'code_point_attribute::white_space',
            CodePointAttribute.CasedLetter: 'code_point_attribute::cased_letter',
            CodePointAttribute.CaseIgnorable: 'code_point_attribute::case_ignorable'}[c]


def format_attributes(c):
    return ' | '.join(format_attribute(a) for a in sorted(c))


CodePointProperties = namedtuple(
    'CodePointProperties',
    ['code_point', 'attributes', 'numeric_value',
     'simple_uppercase', 'simple_lowercase', 'simple_case_folding', 'complex_uppercase', 'complex_case_folding']
)


def format_maybe_none(x):
    if x is None:
        return 0
    else:
        return x


def format_code_point_list(l):
    return ''.join('\\x{:x}'.format(c) for c in l)


def format_properties(prop):
    return ('{'
            + '{}, {}, {}, {}, {}, {}, U"{}", U"{}"'.format(prop.code_point,
                                                            format_attributes(prop.attributes),
                                                            prop.numeric_value,
                                                            format_maybe_none(prop.simple_uppercase),
                                                            format_maybe_none(prop.simple_lowercase),
                                                            prop.simple_case_folding,
                                                            format_code_point_list(prop.complex_uppercase),
                                                            format_code_point_list(prop.complex_case_folding))
            + '}')


def is_numeric(c):
    return (c.decimal_value != ''
            and c.digit_value != ''
            and c.numeric_value != '')


def is_white_space(cp, property_list):
    return cp in property_list and 'White_Space' in property_list[cp]


def is_alphabetic(cp, derived_core_properties):
    return cp in derived_core_properties and 'Alphabetic' in derived_core_properties[cp]


def make_alphabetic_properties(code_point, character, db):
    attributes = {CodePointAttribute.Alphabetic}
    derived_properties = db.derived_core_properties[code_point]
    if 'Lowercase' in derived_properties:
        attributes.add(CodePointAttribute.LowerCase)
    if 'Uppercase' in derived_properties:
        attributes.add(CodePointAttribute.UpperCase)

    if 'Lowercase' in derived_properties or 'Uppercase' in derived_properties or character.general_category == 'Lt':
        attributes.add(CodePointAttribute.CasedLetter)

    upcase_cp = int(character.simple_uppercase, 16) if character.simple_uppercase != '' else code_point
    downcase_cp = int(character.simple_lowercase, 16) if character.simple_lowercase != '' else code_point

    simple_case_folding_cp = code_point
    complex_case_folding = [code_point]
    if code_point in db.case_folding:
        if db.case_folding[code_point].common is not None:
            simple_case_folding_cp = db.case_folding[code_point].common

        if len(db.case_folding[code_point].full) > 0:
            complex_case_folding = db.case_folding[code_point].full
        else:
            complex_case_folding = [simple_case_folding_cp]

    complex_uppercase = [upcase_cp]

    if code_point in db.special_casing:
        sc = db.special_casing[code_point]
        complex_uppercase = sc.upper

    return CodePointProperties(code_point,
                               attributes,
                               0, # numeric_value
                               upcase_cp,
                               downcase_cp,
                               simple_case_folding_cp,
                               complex_uppercase,
                               complex_case_folding)


def build_properties(db):
    properties = []

    for cp, c in db.code_points.items():
        assert c.simple_uppercase == '' or is_alphabetic(cp, db.derived_core_properties)
        assert c.simple_lowercase == '' or is_alphabetic(cp, db.derived_core_properties)
        assert c.general_category != 'Lt' or is_alphabetic(cp, db.derived_core_properties)
        assert cp not in db.case_folding or is_alphabetic(cp, db.derived_core_properties)
        assert cp not in db.special_casing or is_alphabetic(cp, db.derived_core_properties)

        props = None
        if is_numeric(c):
            props = CodePointProperties(cp,
                                        {CodePointAttribute.Numeric},
                                        int(c.decimal_value),
                                        None,
                                        None,
                                        cp,
                                        [cp],
                                        [cp])

        elif is_white_space(cp, db.property_list):
            props = CodePointProperties(cp,
                                        {CodePointAttribute.WhiteSpace},
                                        0,
                                        None,
                                        None,
                                        cp,
                                        [cp],
                                        [cp])

        elif is_alphabetic(cp, db.derived_core_properties):
            props = make_alphabetic_properties(cp, c, db)

        if 'Case_Ignorable' in db.derived_core_properties.get(cp, []):
            if props is None:
                props = CodePointProperties(cp, {CodePointAttribute.CaseIgnorable}, 0, None, None, cp, [cp], [cp])
            else:
                props.attributes.add(CodePointAttribute.CaseIgnorable)

        if props is not None:
            properties.append(props)

    return properties


def list_codepoints(props):
    return [p.code_point for p in props]


def output_universal_hash_function(f, name, out):
    print('constexpr hash_function ' + name + '{' + '{}ull, {}ull, {}'.format(f.a, f.b, f.n) + '};', file=out)


def output_g(g, out):
    print('constexpr std::array<std::uint32_t, {}> g{{{{'.format(len(g)), file=out)
    print(',\n'.join('  ' + str(value) for value in g), file=out)
    print('}};', file=out)


def output_perfect_hash_function(h, out):
    print('constexpr std::uint64_t p = {}ull;'.format(HashFunction.p), file=out)
    output_universal_hash_function(h.f1, 'f1', out)
    output_universal_hash_function(h.f2, 'f2', out)
    output_g(h.g, out)


def output_code_point_table(props, out):
    print('std::array<code_point_properties, {}> const code_points{{{{'.format(len(props)), file=out)
    print(',\n'.join('  ' + format_properties(p) for p in props), file=out)
    print('}};', file=out)


def output_generated_file_header(out):
    print('// Do not edit. This is a generated file. See scripts/unicode.py.', file=out)


def read_database(dir_path):
    unicode_data_path = dir_path / 'UnicodeData.txt'
    prop_list_path = dir_path / 'PropList.txt'
    derived_core_properties_path = dir_path / 'DerivedCoreProperties.txt'
    case_folding_path = dir_path / 'CaseFolding.txt'
    special_casing_path = dir_path / 'SpecialCasing.txt'

    print('Reading {}...'.format(unicode_data_path))
    code_points = read_characters(unicode_data_path)

    print('Reading {}...'.format(prop_list_path))
    prop_list = read_property_list(prop_list_path)

    print('Reading {}...'.format(derived_core_properties_path))
    derived_core_properties = read_property_list(derived_core_properties_path)

    print('Reading {}...'.format(case_folding_path))
    case_folding = read_case_folding(case_folding_path)

    print('Reading {}...'.format(special_casing_path))
    special_casing = read_special_casing(special_casing_path)

    return UnicodeDatabase(code_points, prop_list, derived_core_properties, case_folding, special_casing)


def main():
    random.seed(a=0)

    parser = argparse.ArgumentParser(description='Build code_point_properties_table.inc')
    parser.add_argument('data_dir', type=str, help='path to directory with Unicode database files')
    parser.add_argument('src', type=str, help='path to the src directory')

    args = parser.parse_args()

    db = read_database(Path(args.data_dir))

    print('Analysing properties...')
    props = build_properties(db)

    print('Building perfect hash function...')
    h = build_perfect_hash(list_codepoints(props))

    print('Writing result...')

    with open(Path(args.src) / 'code_point_properties_forward.inc', 'w') as out:
        output_generated_file_header(out)
        print('extern std::array<code_point_properties, {}> const code_points;'.format(len(props)), file=out)

    with open(Path(args.src) / 'code_point_properties_table.inc', 'w') as out:
        output_generated_file_header(out)
        output_code_point_table(props, out)
        output_perfect_hash_function(h, out)

if __name__ == '__main__':
    main()
