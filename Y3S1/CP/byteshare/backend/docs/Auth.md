# Auth API

## How auth is handled

The API uses *Basic HTTP Authentication* to authenticate logged in users.

It expects either both username and password, *or* a JWT in place of the username, in which case the password is ignored

## **/register**

Register a new user

**Data required:**

JSON with `username`, `password` fields

**Returns:**

* JSON with `token` (encoded JWT) and `duration` fields
* 400 if user already exists or other username/password is empty

## **/token**

Gets a new token for the logged in user

**Returns:**

* JSON with `token` (encoded JWT) and `duration` fields (same as **/register**)
