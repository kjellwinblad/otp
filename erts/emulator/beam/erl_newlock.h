#ifndef ERL_NEWLOCK_H
#define ERL_NEWLOCK_H

#include<stdint.h>

/* values preliminary */
#define MAX_QUEUE_LENGTH 3276
#define MAX_PASSES 256
#define NUM_OF_QUEUES 5

struct queue_entry {
    uint32_t ticket;
    void* value;
};

typedef void* QueueEntry;
/* might also be: */
/* typedef struct queue_entry QueueEntry; */

typedef struct {
/*    uint16_t size; // needs to be atomic */
/*    uint16_t capacity; // potentially needs to be atomic */
/*    erts_atomic32_t capacity; */
    erts_atomic32_t head;
    QueueEntry* entries;
} queue_handle;

typedef union {
    queue_handle qh;
    byte _cache_line_padding[64];
} padded_queue_handle;

/* TODO: this function should be deprecated for it is misleading */
static ERTS_INLINE int queue_is_empty(queue_handle* q) {
    return (erts_atomic32_read_mb(&q->head) == -1);
}


static ERTS_INLINE int queue_is_full(queue_handle* q) {
    return (erts_atomic32_read_mb(&q->head) == MAX_QUEUE_LENGTH-1);
}


static ERTS_INLINE uint32_t queue_size(queue_handle* q) {
    return erts_atomic32_read_mb(&q->head) + 1;
}

void queue_init(queue_handle* q);
void queue_reset(queue_handle* q);
int queue_push(queue_handle* q, void* entry);
void* queue_pop(queue_handle* q, unsigned int idx);

typedef struct newlock_locknode {
    erts_atomic32_t locked;
    erts_atomic_t next;
    queue_handle queues[NUM_OF_QUEUES];
} newlock_node;

void acquire_newlock(erts_atomic_t* L, newlock_node* I);
int try_newlock(erts_atomic_t* L, newlock_node* I);
int is_free_newlock(erts_atomic_t* L);
void release_newlock(erts_atomic_t* L, newlock_node* I);

#endif // ERL_NEWLOCK_H
