Spyglass autocomplete server (WIP)
==================================

|build|_

Spyglass is a autocomplete server that stores list of search terms and content and uses that data to provide an autocomplete backend.

::

   # Upload collection/version. if version is not provided just use next integer
   POST /<collection>/ [{term: string, object: string}]
   {success: true}
   # Search for given keyword
   GET /<collection>/?term=<keywords>
   [<object>]
   # Delete collection
   DELETE /<collection>/

.. |build| image:: https://travis-ci.org/huseyinyilmaz/spyglass.png
.. _build: https://travis-ci.org/huseyinyilmaz/spyglass
