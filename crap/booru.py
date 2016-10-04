from flask import Flask
app = Flask(__name__)

# Filthy work-around for braindead breakage
import sys
sys.path.append('/usr/lib/python3.5/site-packages/xapian')
from xapian import *

# Global constants
dbPath = "/z/booru/"
xapianPath = dbPath + "xapian/"
imagesPath = dbPath + "images/"

# Value slots
siteIDSlot   = 0
scoreSlot    = 1
fileNameSlot = 2
sourceSlot   = 3

unprocessedTag = 'Xtodo'

termPrefixes = [
    ('B', ["booru", "site"]),
    ('U', ["uploader", "creator"]),
    ('R', ["rating"]),
]

rangeValues = [
    (siteIDSlot, ["id"]),
    (scoreSlot,  ["score"]),
]

strValues = [
    (fileNameSlot, ["file", "hash"]),
    (sourceSlot,   ["source", "src"]),
]

# Helpers
def addRange(qp, v, f):
    qp.add_valuerangeprocessor(NumberValueRangeProcessor(v, f + ':'))

def getVal(doc, slot):
    return int(sortable_unserialise(doc.get_value(slot)))

def search(query=None, page=0, limit=10, sort=['score', 'id'], sortdesc=True,
           unfinished=False):
    db = Database(xapianPath)

    qp = QueryParser()
    qp.set_database(db)
    qp.set_default_op(Query.OP_AND)
    for p, fs in termPrefixes:
        [ qp.add_boolean_prefix(f, p) for f in fs ]
    for v, fs in rangeValues:
        [ addRange(qp, v, f) for f in fs ]

    if query:
        flags = (QueryParser.FLAG_DEFAULT | QueryParser.FLAG_BOOLEAN_ANY_CASE |
                 QueryParser.FLAG_PURE_NOT)
        q = qp.parse_query(query, flags)
    else:
        q = Query.MatchAll

    if not unfinished:
        q = Query(Query.OP_AND_NOT, q, Query(unprocessedTag))

    en = Enquire(db)
    en.set_query(q)

    if len(sort) > 0:
        km = MultiValueKeyMaker()
        for s in sort:
            for v, fs in rangeValues:
                [ km.add_value(v) for f in fs if s == f ]
        en.set_sort_by_key(km, sortdesc)

    return en.get_mset(page*limit, limit)

def listFiles(mset):
    for match in mset:
        doc = match.document
        fileName = doc.get_value(fileNameSlot).decode()
        fileURL  = doc.get_data().decode()

        finished = unprocessedTag in (t.term for t in doc.termlist())
        if finished:
            print(imagesPath + fileName)
        else:
            print(fileURL)

# For testing
listFiles(search(query=' '.join(sys.argv[1:]), limit=100, unfinished=True))

#    res = en.get_mset(page*limit, limit)
#    print("Results: approx.", res.get_matches_estimated())
#
#    for match in res:
#        doc = match.document
#
#        id    = getVal(doc, siteIDSlot)
#        score = getVal(doc, scoreSlot)
#        file  = doc.get_value(fileNameSlot)
#
#        print(id, score, doc.get_docid())
#
#    db.close()
