#include <xapian.h>
#include <string.h>

extern "C" {

// Constants
int db_create_or_open()   { return Xapian::DB_CREATE_OR_OPEN;   }
int db_backend_inmemory() { return Xapian::DB_BACKEND_INMEMORY; }

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
Xapian::WritableDatabase * db_open(const char *path, int flags,
                                   const char **error)
{
    Xapian::WritableDatabase *db = NULL;
    try {
        db = new Xapian::WritableDatabase(std::string(path), flags);
        return db;
    } catch (const Xapian::Error &e) {
        *error = strdup(e.get_description().c_str());
        delete db;
        return NULL;
    }
}

void db_delete(Xapian::WritableDatabase *db)
{
    delete db;
}

unsigned int db_add_doc(Xapian::WritableDatabase *db, Xapian::Document *doc,
                        const char **error)
{
    try {
        return db->add_document(*doc);
    } catch (const Xapian::Error &e) {
        *error = strdup(e.get_description().c_str());
        return 0;
    }
}

unsigned int db_add_synonym(Xapian::WritableDatabase *db,
                            const char *tag1, size_t len1,
                            const char *tag2, size_t len2,
                            const char **error)
{
    try {
        db->add_synonym(std::string(tag1, len1), std::string(tag2, len2));
        return 1;
    } catch (const Xapian::Error &e) {
        *error = strdup(e.get_description().c_str());
        return 0;
    }
}

unsigned int db_tx_begin(Xapian::WritableDatabase *db, const char **error)
{
    try {
        db->begin_transaction();
        return 1;
    } catch (const Xapian::Error &e) {
        *error = strdup(e.get_description().c_str());
        return 0;
    }
}

unsigned int db_tx_commit(Xapian::WritableDatabase *db, const char **error)
{
    try {
        db->commit_transaction();
        return 1;
    } catch (const Xapian::Error &e) {
        *error = strdup(e.get_description().c_str());
        return 0;
    }
}

}
