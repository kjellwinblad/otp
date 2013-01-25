#ifndef STL_HPP
#define STL_HPP

typedef long Eterm;
typedef struct db_term {
    void* first_oh;
    unsigned int size;
#ifdef DEBUG_CLONE
    Eterm* debug_clone;
#endif
    Eterm tpl[1];
} DbTerm;

/* commonly used by C-implemented datastructures */
extern "C" int compare(Eterm * element, Eterm * key);
extern "C" void * generic_interface_malloc(size_t size);
extern "C" void generic_interface_free(void *);

template <typename T> using ETSAllocator = KVallocator<T, generic_interface_malloc, generic_interface_free>;
#endif // STL_HPP
