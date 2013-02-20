#ifndef _CPP_DS_H
#define _CPP_DS_H

#include "kvset.h"

enum gi_type {
    SKIPLIST,
    TESTMAP,
    STLSET,
    STLMAP,
    STLUNORDERED_SET,
    BTREESET,
    BTREESET4,
    NULL_STORAGE,
    ERROR_NO_TYPE
};

enum gi_type get_gi_subtype(Eterm e);

struct db_table_generic_interface;
KVSet* gi_create(struct db_table_generic_interface* tbl);

/* prototypes for the construction functions used in gi_create implementation */
#include "skiplist.h"
KVSet* new_cppset_default(void);
KVSet* create_stlset(void);
KVSet* create_stlmap(void);
KVSet* create_stlunordered_set(void);
KVSet* create_btreeset(void);
KVSet* create_btreeset4(void);
KVSet* create_null(void);

/* commonly used by C-implemented datastructures */
int compare(Eterm * element, Eterm * key);
void * generic_interface_malloc(size_t size);
void generic_interface_free(void *);


#endif /* _CPP_DS_H */
