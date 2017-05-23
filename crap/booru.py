import argparse
from xapian import *

# Global constants
dbPath = "/booru/"
xapianPath = dbPath + "xapian/"
imagesPath = dbPath + "images/"

# Value slots
siteIDSlot   = 0
scoreSlot    = 1
fileNameSlot = 2
fileURLSlot  = 3
sourceSlot   = 4

termPrefixes = [
    ('B', ["booru", "site"]),
    ('I', ["id"]),
    ('U', ["uploader", "creator"]),
    ('R', ["rating"]),
    ('E', ["extension", "ext"]),
    ('X', ["state"]),
    ('F', ["file", "hash"]),
    ('',  ["tag"])
]

rangeValues = [
    (siteIDSlot, ["id", "new", "newest"]),
    (scoreSlot,  ["score", "best"]),
]

strValues = [
    (fileURLSlot,  ["url"]),
    (sourceSlot,   ["source", "src"]),
]

# Helpers
def addRange(qp, r, v, f):
    qp.add_valuerangeprocessor(r(v, f + ':'))

def getVal(doc, slot):
    return int(sortable_unserialise(doc.get_value(slot)))

def showTerm(t):
    t = t.decode()

    for p, ns in termPrefixes:
        if t[0] == p:
            t = ns[0]+':'+t[1:]
            break

    return t

def search(query=None, page=0, limit=10, sort=None, sortdesc=True,
           unfinished=False, **ignored):
    db = Database(xapianPath)

    qp = QueryParser()
    qp.set_database(db)
    qp.set_default_op(Query.OP_AND)
    qp.set_stemming_strategy(QueryParser.STEM_NONE)

    for p, fs in termPrefixes:
        [ qp.add_boolean_prefix(f, p) for f in fs ]
    for v, fs in rangeValues:
        [ addRange(qp, NumberValueRangeProcessor, v, f) for f in fs ]
    for v, fs in strValues:
        [ addRange(qp, StringValueRangeProcessor, v, f) for f in fs ]

    if query:
        flags = (QueryParser.FLAG_DEFAULT | QueryParser.FLAG_BOOLEAN_ANY_CASE |
                 QueryParser.FLAG_PURE_NOT)
        q = qp.parse_query(query, flags)
    else:
        q = Query.MatchAll

    en = Enquire(db)
    en.set_query(q)

    if sort is not None and sort != 'none':
        for v, fs in rangeValues:
            [ en.set_sort_by_value(v, sortdesc) for f in fs if sort == f ]

    return en.get_mset(page*limit, limit)


# Commands
def listFiles(mset, args):
    for match in mset:
        doc = match.document
        fileName = doc.get_value(fileNameSlot).decode()
        print(imagesPath + fileName)

def showInfo(mset, args):
    for match in mset:
        doc = match.document

        print('-- Document', doc.get_docid(), '--')
        for v, fs in strValues:
            print(fs[0]+':'+doc.get_value(v).decode())
        for v, fs in rangeValues:
            print(fs[0]+':'+str(getVal(doc, v)))

        for t in doc.termlist():
            print(showTerm(t.term))

def showTags(mset, args):
    tags = set()

    for match in mset:
        tags = tags.union(set([ t.term for t in match.document.termlist()]))

    tags = list(tags)
    tags.sort()

    for t in tags:
        print('{0:10} {1}'.format(mset.get_termfreq(t), showTerm(t)))

def count(mset, args):
    print(mset.get_matches_estimated())

commands = {
    'list': listFiles,
    'info': showInfo,
    'show': showInfo,
    'tags': showTags,
    'count': count,
}


def main():
    # Parse the options
    opt = argparse.ArgumentParser(description='Query a hsbooru database.')
    opt.add_argument('-s', '--sort', default='new', metavar='VAL',
                     choices=['none'] + sum([x[1] for x in rangeValues], []),
                     help='value to sort results by')

    opt.add_argument('-a', '--asc', action='store_false', dest='sortdesc',
                     help='sort ascending (lowest first)')

    opt.add_argument('-l', '--limit', type=int, default=1000,
                     help='how many results to return')

    opt.add_argument('-p', '--page', type=int, default=0,
                     help='which page of the results to return')

    opt.add_argument('action', metavar='ACTION', choices=list(commands.keys()),
                     help='what to do with the results')

    opt.add_argument('query', nargs='*', metavar='TAG',
                     help='tag expression to search for')

    args = opt.parse_args()
    args.query = ' '.join(args.query)

    mset = search(**vars(args))
    commands[args.action](mset, args)

main()
