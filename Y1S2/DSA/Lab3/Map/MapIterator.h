#pragma once
#include "Map.h"
class MapIterator
{
	//DO NOT CHANGE THIS PART
	friend class Map;
private:
	const Map& _map;
  Index _crr;

	MapIterator(const Map& m);
public:
	void first();
	void next();
	TElem getCurrent();
	bool valid() const;

  // Best-case: Th(1)
  // Worst-case: Th(k)
  // Average-case: O(k)
  void jumpBackward(int k);
};