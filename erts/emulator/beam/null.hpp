#ifndef NULL_HPP
#define NULL_HPP

/**@file
 * @brief a null backend for ETS. Does not store data.
 */


template<typename KeyType = long*, class StdFuns = standard_functions<KeyType>, typename ValueType = KeyType>
class null_storage {
	public:
		null_storage() {}
		ValueType put(ValueType key)     {
			return key;
		}
		bool put_new(ValueType key)    {
			return false;
		}
		ValueType remove(KeyType key)  {
			return nullptr;
		}
		ValueType lookup(KeyType key) {
			return nullptr;
		}
		bool member(KeyType key) {
			return false;
		}
		ValueType first() {
			return nullptr;
		}
		ValueType last() {
			return nullptr;
		}
		ValueType next(KeyType key) {
			return nullptr;
		}
		ValueType previous(KeyType key){
			return nullptr;
		}
};


#endif // NULL_HPP
