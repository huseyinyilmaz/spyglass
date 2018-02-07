Spyglass autocomplete server (WIP)
==================================

::

   # Upload collection/version. if version is not provided just use next integer
   POST /api/v1/<name>/<version>/ [{keys: [string], object: <object>}]
   POST /api/v1/<name>/ [{keys: [string], content:string}]

   {success: true}
   # Search for given keyword
   GET /api/v1/<name>/?keyword=<keyword>
   [<object>]
   # Delete corpus
   DELETE /api/v1/<name>/latest/
