#!/usr/bin/env python3

from collections import namedtuple
from enum import Enum
import random
import argparse

Character = namedtuple(
    'Character',
    ['code_point', 'name', 'general_category', 'canonical_combining_class', 'bidi_class',
     'decomposition', 'decimal_value', 'digit_value', 'numeric_value',
     'bidi_mirrored', 'unicode_1_name', 'iso_comment',
     'simple_uppercase', 'simple_lowercase', 'simple_titlecase']
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


class HashFunction:
    int_max = 2**64

    def __init__(self, a, b, c, d, n):
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.n = n

    def __call__(self, x):
        def add(x, y):
            return (x + y) % HashFunction.int_max

        def mul(x, y):
            return (x * y) % HashFunction.int_max

        return add(mul(x, add(mul(x, add(mul(x, self.a), self.b)), self.c)), self.d) % self.n


def make_random_hash_fn(n):
    return HashFunction(random.randrange(HashFunction.int_max),
                        random.randrange(HashFunction.int_max),
                        random.randrange(HashFunction.int_max),
                        random.randrange(HashFunction.int_max),
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


class CodePointCategory(Enum):
    Numeric = 0


def format_category(c):
    return {CodePointCategory.Numeric: 'code_point_category::numeric'}[c]


CodePointProperties = namedtuple(
    'CodePointProperties',
    ['code_point', 'category']
)


def format_properties(prop):
    return '{' + '{}, {}'.format(prop.code_point, format_category(prop.category)) + '}'


def numeric(c):
    return (c.decimal_value != ''
            and c.digit_value != ''
            and c.numeric_value != '')


def build_properties(codepoints):
    properties = []

    for cp, c in codepoints.items():
        if numeric(c):
            properties.append(CodePointProperties(cp, CodePointCategory.Numeric))

    return properties


def main():
    parser = argparse.ArgumentParser(description='Build code_point_properties_table.inc')
    parser.add_argument('data', type=str, help='path to UnicodeData.txt')
    parser.add_argument('output', type=str, help='output file path')

    args = parser.parse_args()

    chars = read_characters(args.data)
    props = build_properties(chars)

    with open(args.output, 'w') as out:
        print('std::array<code_point_properties, {}> code_points{{{{'.format(len(props)), file=out)
        print(',\n'.join('  ' + format_properties(p) for p in props), file=out)
        print('}};', file=out)


if __name__ == '__main__':
    main()
