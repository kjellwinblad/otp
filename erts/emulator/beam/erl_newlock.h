#ifndef ERL_NEWLOCK_H
#define ERL_NEWLOCK_H

#include<stdint.h>

/* values preliminary */
#define MAX_QUEUE_LENGTH 16
#define MAX_PASSES 256

typedef struct {
/*    uint16_t size; // needs to be atomic */
/*    uint16_t capacity; // potentially needs to be atomic */
    erts_atomic32_t size;
/*    erts_atomic32_t capacity; */
    uint16_t head;
    uint16_t tail;
    void** entries;
} queue_handle;

typedef union {
    queue_handle qh;
    byte _cache_line_padding[64];
} padded_queue_handle;

static ERTS_INLINE int queue_is_empty(queue_handle* q) {
    return (erts_atomic32_read_mb(&q->size) == 0);
}


static ERTS_INLINE int queue_is_full(queue_handle* q) {
    return (erts_atomic32_read_mb(&q->size) == MAX_QUEUE_LENGTH);
}


static ERTS_INLINE uint16_t queue_size(queue_handle* q) {
    return erts_atomic32_read_mb(&q->size);
}

void queue_init(queue_handle* q);
void queue_push(queue_handle* q, void* entry);
void* queue_pop(queue_handle* q);

typedef struct newlock_locknode {
    erts_atomic32_t locked;
    uint32_t pass_counter;
    erts_atomic_t next;
    queue_handle* queues;
} newlock_node;

void acquire_newlock(erts_atomic_t* L, newlock_node* I);
void release_newlock(erts_atomic_t* L, newlock_node* I);

#endif // ERL_NEWLOCK_H
