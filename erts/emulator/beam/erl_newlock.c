#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_newlock.h"

#define SUCCESSOR(INDEX) ((INDEX+1)%MAX_QUEUE_LENGTH)

void queue_init(queue_handle* q){
    int i;
    erts_atomic32_set_nob(&q->size, 0);
    q->head  =0;
    q->tail = 0;
    q->entries = malloc(MAX_QUEUE_LENGTH * sizeof(QueueEntry));
    /* // initializing the buffer is not required
    for(i = 0; i < MAX_QUEUE_LENGTH; i++) {
	q->entries[i] = NULL;
    }
    */
}
/*
void queue_init_padded(padded_queue_handle* q){
    q->qh.entries = malloc(MAX_QUEUE_LENGTH * sizeof(padded_queue_handle));
}
*/

/* return 1 on queue full */
int queue_push(queue_handle* q, void* entry, erts_atomic32_t* cnt) {
    erts_aint32_t ticket = erts_atomic_inc_read_mb(cnt);
    if(ticket > MAX_QUEUE_LENGTH) return 0;
    q->entries[ q->tail ].value = entry;
    q->entries[ q->tail ].ticket = ticket;
    q->tail = SUCCESSOR( q->tail );
    erts_atomic32_inc_mb( &q->size );
    return 1;
}

void* queue_pop(queue_handle* q, erts_atomic32_t* cnt) {
    erts_aint32_t cntval = erts_atomic32_read_mb(cnt);
    QueueEntry entry;
    
    entry = q->entries[ q->head ];
    if(entry.ticket != cntval) {
	return NULL;
    } else {
	erts_atomic32_inc_mb(cnt);
    }
    /* // clearing the buffer is not required
    q->entries[ q->head] = NULL;
    */
    q->head = SUCCESSOR( q->head );
    erts_atomic32_dec_mb( &q->size );

    return (entry.value);
}

void acquire_newlock(erts_atomic_t* L, newlock_node* I) {
    newlock_node* pred;
    erts_atomic_set_nob(&I->next, (erts_aint_t) NULL);
    pred = (newlock_node*)erts_atomic_xchg_mb(L, (erts_aint_t) I);
    if(pred != NULL) {
	erts_atomic32_set_mb(&I->locked, 1);
	erts_atomic_set_mb(&pred->next, (erts_aint_t) I);
	while(erts_atomic32_read_mb(&I->locked)); /* spin */
    }
}

int try_newlock(erts_atomic_t* L, newlock_node* I) {
    newlock_node* pred;
    erts_atomic_set_nob(&I->next, (erts_aint_t) NULL);
    pred = (newlock_node*)erts_atomic_cmpxchg_mb(L, (erts_aint_t) I, (erts_aint_t) NULL);
    return pred == NULL;
}

int is_free_newlock(erts_atomic_t* L) {
    return erts_atomic_read_mb(L) == (erts_aint_t) NULL;
}

void release_newlock(erts_atomic_t* L, newlock_node* I) {
    newlock_node* next = (newlock_node*)erts_atomic_read_mb(&I->next);
    if(next == NULL) {
	if(erts_atomic_cmpxchg_mb(L, (erts_aint_t) NULL, (erts_aint_t) I) == (erts_aint_t) I) {
	    return;
	}
	do {
	    next = (newlock_node*) erts_atomic_read_mb(&I->next);
	} while(next == NULL); /* spin */
    }
    erts_atomic32_set_mb(&next->locked, 0);
}
