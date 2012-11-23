
#ifndef _DB_SUBTABLE_HASH_H
#define _DB_SUBTABLE_HASH_H

#include "erl_db_hash.h"


#define KJELL_DEBUG

#ifdef KJELL_DEBUG
 #define D if(1) 
#else
 #define D if(0) 
#endif

void db_initialize_subtable_hash(void);

typedef struct db_table_subtable_hash {
    DbTableCommon common;
    DbTableHash* subtables;
} DbTableSubtableHash;


#endif


