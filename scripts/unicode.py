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
    ['code_points', 'property_list', 'derived_core_properties', 'case_folding']
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


CharacterCaseFolding = namedtuple(
    'CharacterCaseFolding',
    ['common']
)

case_folding_re = re.compile(r'([0-9A-F]+); ([CFST]); ([0-9A-F ]+);')

def read_case_folding(path):
    result = {}

    with open(path, 'r') as f:
        for line in f:
            m = case_folding_re.match(line)
            if m:
                if m.group(2) == 'C':
                    cp = int(m.group(1), 16)
                    common = int(m.group(3), 16)
                    result[cp] = CharacterCaseFolding(common)

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


class CodePointCategory(IntEnum):
    Numeric = 0
    LowerCase = 1
    UpperCase = 2
    Alphabetic = 3
    WhiteSpace = 4


def format_category(c):
    return {CodePointCategory.Numeric: 'code_point_category::numeric',
            CodePointCategory.LowerCase: 'code_point_category::lower_case',
            CodePointCategory.UpperCase: 'code_point_category::upper_case',
            CodePointCategory.Alphabetic: 'code_point_category::alphabetic',
            CodePointCategory.WhiteSpace: 'code_point_category::white_space'}[c]


def format_categories(c):
    return ' | '.join(format_category(cat) for cat in sorted(c))


CodePointProperties = namedtuple(
    'CodePointProperties',
    ['code_point', 'categories', 'numeric_value',
     'simple_uppercase', 'simple_lowercase', 'simple_case_folding']
)


def format_maybe_none(x):
    if x is None:
        return 0
    else:
        return x


def format_properties(prop):
    return ('{'
            + '{}, {}, {}, {}, {}, {}'.format(prop.code_point,
                                              format_categories(prop.categories),
                                              prop.numeric_value,
                                              format_maybe_none(prop.simple_uppercase),
                                              format_maybe_none(prop.simple_lowercase),
                                              prop.simple_case_folding)
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
    categories = {CodePointCategory.Alphabetic}
    derived_properties = db.derived_core_properties[code_point]
    if 'Lowercase' in derived_properties:
        categories.add(CodePointCategory.LowerCase)
    if 'Uppercase' in derived_properties:
        categories.add(CodePointCategory.UpperCase)

    upcase_cp = int(character.simple_uppercase, 16) if character.simple_uppercase != '' else None
    downcase_cp = int(character.simple_lowercase, 16) if character.simple_lowercase != '' else None

    case_folding_cp = code_point
    if code_point in db.case_folding:
        case_folding_cp = db.case_folding[code_point].common

    return CodePointProperties(code_point, categories, 0, upcase_cp, downcase_cp, case_folding_cp)


def build_properties(db):
    properties = []

    for cp, c in db.code_points.items():
        assert c.simple_uppercase == '' or is_alphabetic(cp, db.derived_core_properties)
        assert c.simple_lowercase == '' or is_alphabetic(cp, db.derived_core_properties)

        if is_numeric(c):
            properties.append(CodePointProperties(cp,
                                                  {CodePointCategory.Numeric},
                                                  int(c.decimal_value),
                                                  None,
                                                  None,
                                                  cp))

        elif is_white_space(cp, db.property_list):
            properties.append(CodePointProperties(cp,
                                                  {CodePointCategory.WhiteSpace},
                                                  0,
                                                  None,
                                                  None,
                                                  cp))

        elif is_alphabetic(cp, db.derived_core_properties):
            properties.append(make_alphabetic_properties(cp, c, db))

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
    print('constexpr std::array<code_point_properties, {}> code_points{{{{'.format(len(props)), file=out)
    print(',\n'.join('  ' + format_properties(p) for p in props), file=out)
    print('}};', file=out)


def output_generated_file_header(out):
    print('// Do not edit. This is a generated file. See scripts/unicode.py.', file=out)


def read_database(dir_path):
    unicode_data_path = dir_path / 'UnicodeData.txt'
    prop_list_path = dir_path / 'PropList.txt'
    derived_core_properties_path = dir_path / 'DerivedCoreProperties.txt'
    case_folding_path = dir_path / 'CaseFolding.txt'

    print('Reading {}...'.format(unicode_data_path))
    code_points = read_characters(unicode_data_path)

    print('Reading {}...'.format(prop_list_path))
    prop_list = read_property_list(prop_list_path)

    print('Reading {}...'.format(derived_core_properties_path))
    derived_core_properties = read_property_list(derived_core_properties_path)

    print('Reading {}...'.format(case_folding_path))
    case_folding = read_case_folding(case_folding_path)

    return UnicodeDatabase(code_points, prop_list, derived_core_properties, case_folding)


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
