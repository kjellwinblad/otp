#ifndef BTREESET4_HPP
#define BTREESET4_HPP

/**@file
 * @brief btree set as a backend for ETS
 */

#include "btree_set.h"

template<typename KeyType = long*, class StdFuns = standard_functions<KeyType>, typename ValueType = KeyType>
class btreeset4 {
	class Cmp {
		public: bool operator()(ValueType k1, ValueType k2) const {
			return StdFuns::compare(k1, k2);
		}
	};

	btree::btree_set<ValueType, Cmp, typename StdFuns::template Alloc<ValueType>, 128> m;
	public:
		btreeset4() : m() {}
		ValueType put(ValueType key)     {
			auto p = m.insert(key);
			if(!p.second) {
				auto r = *p.first;
				m.erase(p.first);
				m.insert(key);
				return r;
			}
			return nullptr;
		}
		bool put_new(ValueType key)    {
			bool r = m.count(key);
			if(!r) m.insert(key);
			return !r;
		}
		ValueType remove(KeyType key)  {
			auto it = m.find(key);
			if(it != m.end()) {
				auto old = *it;
				m.erase(it);
				return old;
			}
			return nullptr;
		}
		ValueType lookup(KeyType key) {
			auto it = m.find(key);
			if(it == m.end()) {
				return nullptr;
			}
			else {
				return *it;
			}
		}
		bool member(KeyType key) {
			return m.count(key);
		}
		ValueType first() {
			if(m.begin() != m.end())
				return *m.begin();
			else
				return nullptr;
		}
		ValueType last() {
			if(m.rbegin() != m.rend())
				return *m.rbegin();
			else
				return nullptr;
		}
		ValueType next(KeyType key) {
			auto it = m.lower_bound(key);
			if(it != m.end()) {
				it = std::next(it);
				if(it != m.end()) {
					return *it;
				}
			}
			return nullptr;
		}
		ValueType previous(KeyType key){
			auto it = m.lower_bound(key);
			if(it != m.end()) {
				it = std::prev(it);
				if(it != m.end()) {
					return *it;
				}
			}
			return nullptr;
		}
};


#endif // BTREESET4_HPP
