# Files API

## /files

Returns the logged in user's files

**Returns:**

* JSON with `files` field containing an array of files (each file is an object with `filename` and `contents` fields)

## /files/save

Create or update the contents of a file

**Data required:**

JSON with `filename` and `contents` fields

**Returns:**

* 200 if everything went well

## /files/rename

Rename a file

**Data required:**

JSON with `filename` field and `newFilename`

**Returns:**

* 200 if file was renamed successfully
* 400 if file does not exist

## /files/compile

Compile a file

**Data required:**

JSON with `filename` to be compiled

**Returns:**

* JSON with `result` field containing the compilation result

## /files/delete

Delete a file

**Data required:**

JSON with `filename` field

**Returns:**

* 200 if file was deleted successfully
* 400 if file does not exist
