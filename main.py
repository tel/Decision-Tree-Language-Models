from __future__ import division

from collections import defaultdict
import numpy as np


def getData(path = './'):
    with open(path + 'textA.txt', 'r') as fh:
        train = fh.read()
    with open(path + 'textB.txt', 'r') as fh:
        test  = fh.read()

    return (train, test)

def bigramCounts(str):
    counts = defaultdict(lambda: 0)
    for bg in zip(str, str[1:]):
        counts[bg] += 1
    return counts

def bigramFreq(str):
    cts = bigramCounts(str)
    n = len(str)
    out = defaultdict(lambda: 0)
    out.update((k, v/n) for k, v in cts.iteritems())
    return out

def allBigrams():
    alphabet = "abcdefghijklmnopqrstuvwxyz "
    return [(a, b) for a in alphabet for b in alphabet]

class Tree(object):
    def __init__(leaf = 

def agglomerate(base_freq, atoms):
    atoms_id = dict(enumerate(atoms))
    clusters = dict((idx, [a]) for idx, a in atoms_id.iteritems())
    trees = [Tree(id) for id, cluster in clusters.iteritems]
