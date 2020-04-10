#include "Map.h"
#include "MapIterator.h"

Map::Map() {
	//TODO - Implementation
}

Map::~Map() {
	//TODO - Implementation
}

TValue Map::add(TKey c, TValue v){
	//TODO - Implementation
	return NULL_TVALUE;
}

TValue Map::search(TKey c) const{
	//TODO - Implementation
	return NULL_TVALUE;
}

TValue Map::remove(TKey c){
	//TODO - Implementation
	return NULL_TVALUE;
}


int Map::size() const {
	//TODO - Implementation
	return 0;
}

bool Map::isEmpty() const{
	//TODO - Implementation
	return false;
}

MapIterator Map::iterator() const {
	return MapIterator(*this);
}



