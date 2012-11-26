
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#define ERTS_WANT_DB_INTERNAL__
#include "erl_db.h"
#include "bif.h"
#include "big.h"
#include "export.h"
#include "erl_binary.h"


//Public interface
int db_create_subtable_hash(Process *p, DbTable* tb);

int db_first_subtable_hash(Process* p, 
                           DbTable* tb, /* [in out] */ 
                           Eterm* ret   /* [out] */);
int db_next_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */
                          Eterm key,   /* [in] */
                          Eterm* ret /* [out] */);
int db_last_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */
                          Eterm* ret   /* [out] */);
int db_prev_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */
                          Eterm key, 
                          Eterm* ret);
int db_put_subtable_hash(DbTable* tb, /* [in out] */ 
                         Eterm obj,
                         int key_clash_fail); /* DB_ERROR_BADKEY if key exists */ 
int db_get_subtable_hash(Process* p, 
                         DbTable* tb, /* [in out] */ 
                         Eterm key, 
                         Eterm* ret);
int db_get_element_subtable_hash(Process* p, 
                                 DbTable* tb, /* [in out] */ 
                                 Eterm key, 
                                 int index, 
                                 Eterm* ret);
int db_member_subtable_hash(DbTable* tb, /* [in out] */ 
                            Eterm key, 
                            Eterm* ret);
int db_erase_subtable_hash(DbTable* tb,  /* [in out] */ 
                           Eterm key, 
                           Eterm* ret);
int db_erase_object_subtable_hash(DbTable* tb, /* [in out] */ 
                                  Eterm obj,
                                  Eterm* ret);
int db_slot_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */ 
                          Eterm slot, 
                          Eterm* ret);
int db_select_chunk_subtable_hash(Process* p, 
                                  DbTable* tb, /* [in out] */ 
                                  Eterm pattern,
                                  Sint chunk_size,
                                  int reverse,
                                  Eterm* ret);
int db_select_subtable_hash(Process* p, 
                            DbTable* tb, /* [in out] */ 
                            Eterm pattern,
                            int reverse,
                            Eterm* ret);
int db_select_delete_subtable_hash(Process* p, 
                                   DbTable* tb, /* [in out] */ 
                                   Eterm pattern,
                                   Eterm* ret);
int db_select_continue_subtable_hash(Process* p, 
                                     DbTable* tb, /* [in out] */ 
                                     Eterm continuation,
                                     Eterm* ret);
int db_select_delete_continue_subtable_hash(Process* p, 
                                            DbTable* tb, /* [in out] */ 
                                            Eterm continuation,
                                            Eterm* ret);
int db_select_count_subtable_hash(Process* p, 
                                  DbTable* tb, /* [in out] */ 
                                  Eterm pattern, 
                                  Eterm* ret);
int db_select_count_continue_subtable_hash(Process* p, 
                                           DbTable* tb, /* [in out] */ 
                                           Eterm continuation, 
                                           Eterm* ret);

int db_delete_all_objects_subtable_hash(Process* p,
                                        DbTable* db /* [in out] */ );

int db_free_table_subtable_hash(DbTable* db /* [in out] */ );
int db_free_table_continue_subtable_hash(DbTable* db); /* [in out] */  
    
void db_print_subtable_hash(int to, 
                            void* to_arg, 
                            int show, 
                            DbTable* tb /* [in out] */ );

void db_foreach_offheap_subtable_hash(DbTable* db,  /* [in out] */ 
                                      void (*func)(ErlOffHeap *, void *),
                                      void *arg);
void db_check_table_subtable_hash(DbTable* tb);

/* Lookup a dbterm for updating. Return false if not found.
 */
int db_lookup_dbterm_subtable_hash(DbTable*, Eterm key, 
                                   DbUpdateHandle* handle); /* [out] */

/* Must be called for each db_lookup_dbterm that returned true,
** even if dbterm was not updated.
*/
void db_finalize_dbterm_subtable_hash(DbUpdateHandle* handle);


DbTableMethod db_hash;
/*
** External interface 
*/
DbTableMethod db_subtable_hash =
        {
        db_create_subtable_hash,
        db_first_subtable_hash,
        db_next_subtable_hash,
        db_first_subtable_hash,
        db_next_subtable_hash,
        db_put_subtable_hash,
        db_get_subtable_hash,
        db_get_element_subtable_hash,
        db_member_subtable_hash,
        db_erase_subtable_hash,
        db_erase_object_subtable_hash,
        db_slot_subtable_hash,
        db_select_chunk_subtable_hash,
        db_select_subtable_hash,
        db_select_delete_subtable_hash,
        db_select_continue_subtable_hash,
        db_select_delete_continue_subtable_hash,
        db_select_count_subtable_hash,
        db_select_count_continue_subtable_hash,
        db_delete_all_objects_subtable_hash,
        db_free_table_subtable_hash,
        db_free_table_continue_subtable_hash,
        db_print_subtable_hash,
        db_foreach_offheap_subtable_hash,
#ifdef HARDDEBUG
        db_check_table_subtable_hash,
#else
        NULL,
#endif
        db_lookup_dbterm_subtable_hash,
        db_finalize_dbterm_subtable_hash
        };

static Uint no_subtables;

Uint calculate_no_subtables()
{
    Uint next;
    int is_prime;
    int i;
    if(erts_no_schedulers <= 3) {
        return erts_no_schedulers;
    } else {
        next = erts_no_schedulers - 1;
        do {
            next = next + 1;
            is_prime = 1;
            for(i = 2; i < next; i++){
                if((next % i) == 0){
                    is_prime = 0;
                }
            }
        } while( ! is_prime);
        return next;
    }
}

void db_initialize_subtable_hash(void){
    no_subtables = calculate_no_subtables();
}

int db_create_subtable_hash(Process *p, DbTable *tbl)
{
    int i;
    D printf("CALLING db_create_subtable_hash %d\n", no_subtables);
    DbTableSubtableHash *tb = &tbl->subtable_hash;
    tb->subtables = (DbTableHash*) malloc(sizeof(DbTableHash)*no_subtables);
    tb->no_deleted_subtables = 0;
#ifdef ERTS_SMP
    tb->common.type = tb->common.type | DB_FINE_LOCKED;
#endif    
    for(i=0; i < no_subtables; i++){
        tb->subtables[i].common = tb->common;
        tb->subtables[i].common.meth = &db_hash;
#ifdef ERTS_SMP
        tb->subtables[i].common.is_thread_safe = 0;
#endif
        db_hash.db_create(p, &(tb->subtables[i]));
    }

    D printf("CREATE TABLE READY %d\n");
    return DB_ERROR_NONE;
}

int db_first_subtable_hash(Process* p, 
                           DbTable* tb, /* [in out] */ 
                           Eterm* ret   /* [out] */)
{
    D printf("CALLING db_first_subtable_hash \n");
    return DB_ERROR_NONE;
}

int db_next_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */
                          Eterm key,   /* [in] */
                          Eterm* ret /* [out] */)
{
    D printf("CALLING db_next_subtable_hash \n");
    return DB_ERROR_NONE;
}


int db_last_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */
                          Eterm* ret   /* [out] */)
{
    D printf("CALLING db_last_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_prev_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */
                          Eterm key, 
                          Eterm* ret)
{
    D printf("CALLING db_prev_subtable_hash \n");
    return DB_ERROR_NONE;
}
inline DbTableHash * get_subtable_from_key(DbTable* tb_param, Eterm key)
{
    DbTableSubtableHash *tb = &tb_param->subtable_hash;
    HashValue hval = MAKE_HASH(key);
    DbTableHash *subtable = &(tb->subtables[hval % no_subtables]);
    return subtable;
}
inline DbTableHash * get_subtable_from_object(DbTable* tb_param, Eterm obj)
{
    DbTableSubtableHash *tb = &tb_param->subtable_hash;
    Eterm key = GETKEY(tb, tuple_val(obj));
    HashValue hval = MAKE_HASH(key);
    DbTableHash *subtable = &(tb->subtables[hval % no_subtables]);
    return subtable;
}
int db_put_subtable_hash(DbTable* tb_param, /* [in out] */ 
                         Eterm obj,
                         int key_clash_fail)
{
    D printf("CALLING db_put_subtable_hash \n");
    DbTableHash *subtable = get_subtable_from_object(tb_param, obj);
    return db_hash.db_put(subtable, obj, key_clash_fail);
}
int db_get_subtable_hash(Process* p, 
                         DbTable* tb_param, 
                         Eterm key, 
                         Eterm* ret)
{
    D printf("CALLING db_get_subtable_hash \n");
    DbTableHash *subtable = get_subtable_from_key(tb_param, key);
    D printf("FOUND \n");
    return db_hash.db_get(p, subtable, key, ret);
}
int db_get_element_subtable_hash(Process* p, 
                                 DbTable* tb, /* [in out] */ 
                                 Eterm key, 
                                 int index, 
                                 Eterm* ret)
{
    D printf("CALLING db_get_element_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_member_subtable_hash(DbTable* tb, /* [in out] */ 
                            Eterm key, 
                            Eterm* ret)
{
    D printf("CALLING db_member_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_erase_subtable_hash(DbTable* tb,  /* [in out] */ 
                           Eterm key, 
                           Eterm* ret)
{
    D printf("CALLING db_erase_subtable_hash \n");
    DbTableHash *subtable = get_subtable_from_key(tb, key);;
    return db_hash.db_erase(subtable, key, ret);
}
int db_erase_object_subtable_hash(DbTable* tb, /* [in out] */ 
                                  Eterm obj,
                                  Eterm* ret)
{
    D printf("CALLING db_erase_object_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_slot_subtable_hash(Process* p, 
                          DbTable* tb, /* [in out] */ 
                          Eterm slot, 
                          Eterm* ret)
{
    D printf("CALLING db_slot_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_select_chunk_subtable_hash(Process* p, 
                                  DbTable* tb, /* [in out] */ 
                                  Eterm pattern,
                                  Sint chunk_size,
                                  int reverse,
                                  Eterm* ret)
{
    D printf("CALLING db_select_chunk_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_select_subtable_hash(Process* p, 
                            DbTable* tb, /* [in out] */ 
                            Eterm pattern,
                            int reverse,
                            Eterm* ret)
{
    D printf("CALLING db_select_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_select_delete_subtable_hash(Process* p, 
                                   DbTable* tb, /* [in out] */ 
                                   Eterm pattern,
                                   Eterm* ret)
{
    D printf("CALLING db_select_delete_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_select_continue_subtable_hash(Process* p, 
                                     DbTable* tb, /* [in out] */ 
                                     Eterm continuation,
                                     Eterm* ret)
{
    D printf("CALLING db_select_continue_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_select_delete_continue_subtable_hash(Process* p, 
                                            DbTable* tb, /* [in out] */ 
                                            Eterm continuation,
                                            Eterm* ret)
{
    D printf("CALLING db_select_delete_continue_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_select_count_subtable_hash(Process* p, 
                                  DbTable* tb, /* [in out] */ 
                                  Eterm pattern, 
                                  Eterm* ret)
{
    D printf("CALLING db_select_count_subtable_hash \n");
    return DB_ERROR_NONE;
}
int db_select_count_continue_subtable_hash(Process* p, 
                                           DbTable* tb, /* [in out] */ 
                                           Eterm continuation, 
                                           Eterm* ret)
{
    D printf("CALLING db_select_count_continue_subtable_hash \n");
    return DB_ERROR_NONE;
}

int db_delete_all_objects_subtable_hash(Process* p,
                                        DbTable* db /* [in out] */ )
{
    D printf("CALLING db_delete_all_objects_subtable_hash \n");
    return DB_ERROR_NONE;
}


int db_free_table_subtable_hash(DbTable* tbl /* [in out] */ )
{
    while (!db_free_table_continue_subtable_hash(tbl))
	;
    return 0;
}

int db_free_table_continue_subtable_hash(DbTable* db)
{
    D printf("CALLING db_free_table_continue_subtable_hash \n");
    DbTableSubtableHash *tb = &db->subtable_hash;
    int no_deleted_subtables = tb->no_deleted_subtables;
    int stop_freeing_table;
    if(no_deleted_subtables == no_subtables){
        free(tb->subtables);
        return 1;   
    } else {
        DbTableHash *subtable = &(tb->subtables[no_deleted_subtables]);
        stop_freeing_table = db_hash.db_free_table_continue(subtable);
        if(stop_freeing_table) {
            tb->no_deleted_subtables = no_deleted_subtables + 1;
            return 0;
        } else {
            return 0;
        }
    }
}

    
void db_print_subtable_hash(int to, 
                            void* to_arg, 
                            int show, 
                            DbTable* tb /* [in out] */ )
{
        D printf("CALLING db_print_subtable_hash \n");
}

void db_foreach_offheap_subtable_hash(DbTable* db,  /* [in out] */ 
                                      void (*func)(ErlOffHeap *, void *),
                                      void *arg)
{
    D printf("CALLING db_foreach_offheap_subtable_hash \n");

}

void db_check_table_subtable_hash(DbTable* tb)
{
    D printf("CALLING db_check_table_subtable_hash \n");

}

/* Lookup a dbterm for updating. Return false if not found.
 */
int db_lookup_dbterm_subtable_hash(DbTable* db, Eterm key, 
                                   DbUpdateHandle* handle)
{
    D printf("CALLING db_lookup_dbterm_subtable_hash \n");
    return DB_ERROR_NONE;
}

/* Must be called for each db_lookup_dbterm that returned true,
** even if dbterm was not updated.
*/
void db_finalize_dbterm_subtable_hash(DbUpdateHandle* handle)
{
    D printf("CALLING db_finalize_dbterm_subtable_hash \n");

}
