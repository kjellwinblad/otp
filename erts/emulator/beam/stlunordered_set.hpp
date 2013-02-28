#ifndef STLUNORDERED_SET_HPP
#define STLUNORDERED_SET_HPP

/**@file
 * @brief STL std::unordered_set as a backend for ETS
 */

#include<unordered_set>

template<typename KeyType = long*, typename Hash = std::hash<KeyType>, class StdFuns = standard_functions<KeyType>, typename ValueType = KeyType>
class stlunordered_set {
	class Cmp {
		public: bool operator()(ValueType k1, ValueType k2) const {
			return StdFuns::compare(k1, k2) == StdFuns::compare(k2, k1);
		}
	};

	std::unordered_set<ValueType, Hash, Cmp, typename StdFuns::template Alloc<ValueType>> m;
	public:
		stlunordered_set() : m() {}
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
			if(m.begin() != m.end())
				return *m.begin();
			else
				return nullptr;
		}
		ValueType next(KeyType key) {
			/*auto it = m.lower_bound(key);
			if(it != m.end()) {
				it = std::next(it);
				if(it != m.end()) {
					return *it;
				}
			}*/
			return nullptr;
		}
		ValueType previous(KeyType key){
			/*auto it = m.lower_bound(key);
			if(it != m.end()) {
				it = std::prev(it);
				if(it != m.end()) {
					return *it;
				}
			}*/
			return nullptr;
		}
};


#endif // STLUNORDERED_SET_HPP
