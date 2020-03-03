#ifndef __UTILS_H__
#define __UTILS_H__

// Convention adopted from the Rust language
// Functions that return String also pass ownership to that string (i.e. caller must free the string)
// Functions that return Str do NOT pass ownership of the string (i.e. caller must NOT free it)
typedef const char* String;
typedef const char* const Str;

// Function that stops the program with an error message
// Similar to Rust's panic! macro.
void failWith(Str message);

#endif
