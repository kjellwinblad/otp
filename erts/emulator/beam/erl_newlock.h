#ifndef ERL_NEWLOCK_H
#define ERL_NEWLOCK_H

#include<stdint.h>

/* values preliminary */
#define MAX_QUEUE_LENGTH 16384
#define MAX_PASSES 256

struct queue_entry {
    uint32_t ticket;
    void* value;
};

typedef struct queue_entry QueueEntry;
/* might also be: */
/* typedef void* QueueEntry; */

typedef struct {
/*    uint16_t size; // needs to be atomic */
/*    uint16_t capacity; // potentially needs to be atomic */
    erts_atomic32_t size;
/*    erts_atomic32_t capacity; */
    uint16_t head;
    uint16_t tail;
    QueueEntry* entries;
} queue_handle;

typedef union {
    queue_handle qh;
    byte _cache_line_padding[64];
} padded_queue_handle;

static ERTS_INLINE int queue_is_empty(queue_handle* q) {
    return (erts_atomic32_read_mb(&q->size) == 0);
}


static ERTS_INLINE int queue_is_full(queue_handle* q, erts_atomic32_t* cnt) {
    return ((erts_atomic32_read_mb(cnt) == MAX_QUEUE_LENGTH) || (erts_atomic32_read_mb(&q->size) == MAX_QUEUE_LENGTH));
}


static ERTS_INLINE uint16_t queue_size(queue_handle* q) {
    return erts_atomic32_read_mb(&q->size);
}

void queue_init(queue_handle* q);
int queue_push(queue_handle* q, void* entry, erts_atomic32_t* cnt);
void* queue_pop(queue_handle* q, erts_atomic32_t* cnt);

typedef struct newlock_locknode {
    erts_atomic32_t locked;
    erts_atomic_t next;
    erts_atomic32_t counter;
    erts_atomic32_t returns;
    queue_handle** queues;
} newlock_node;

void acquire_newlock(erts_atomic_t* L, newlock_node* I);
int try_newlock(erts_atomic_t* L, newlock_node* I);
int is_free_newlock(erts_atomic_t* L);
void release_newlock(erts_atomic_t* L, newlock_node* I);

#endif // ERL_NEWLOCK_H
