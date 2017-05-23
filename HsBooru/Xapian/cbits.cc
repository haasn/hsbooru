#include <xapian.h>

extern "C" {

// Document-related functions
Xapian::Document *doc_new()
{
    return new Xapian::Document();
}

void doc_delete(Xapian::Document *doc)
{
    delete doc;
}

void doc_add_val_str(Xapian::Document *doc, unsigned int valueno,
                     const char *val, size_t len)
{
    doc->add_value((Xapian::valueno)valueno, std::string(val, len));
}

void doc_add_val_dbl(Xapian::Document *doc, unsigned int valueno, double val)
{
    doc->add_value((Xapian::valueno)valueno, Xapian::sortable_serialise(val));
}

void doc_add_term(Xapian::Document *doc, const char *tag, size_t len)
{
    doc->add_term(std::string(tag, len), 1);
}

// Database-related functions
Xapian::WritableDatabase * db_open(const char *path, const char **error)
{
    Xapian::WritableDatabase *db = NULL;
    try {
        db = new Xapian::WritableDatabase(std::string(path));
        return db;
    } catch (const Xapian::Error &e) {
        *error = e.get_msg().c_str();
        delete db;
        return NULL;
    }
}

void db_delete(Xapian::WritableDatabase *db)
{
    delete db;
}

void db_commit(Xapian::WritableDatabase *db)
{
    db->commit();
}

void db_add_doc(Xapian::WritableDatabase *db, Xapian::Document *doc)
{
    db->add_document(*doc);
}

}
