#pragma once

#include "Set.h"

class SetIterator
{
	friend class Set;
private:
	const Set& set;
	int idx;

	SetIterator(const Set& s);

public:
	void first();
	void next();
	TElem getCurrent();
	bool valid() const;
};