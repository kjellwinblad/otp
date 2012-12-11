
#ifndef _DB_SUBTABLE_HASH_H
#define _DB_SUBTABLE_HASH_H

#include "kvset.h"


#define KJELL_DEBUG 1

#ifdef KJELL_DEBUG
 #define D if(1) 
#else
 #define D if(0) 
#endif

void db_initialize_generic_interface(void);

typedef struct db_table_generic_interface {
    DbTableCommon common;
    KVSet* kvset;
} DbTableGenericInterface;


#endif


