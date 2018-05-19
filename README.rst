Spyglass autocomplete server
============================

|build|_

Spyglass is a autocomplete server that stores list of search terms and content and uses that data to provide an autocomplete backend. Spyglass is not a smart search engine. But it is really fast autocomplete engine. A small server should be able to handle around 15.000 req/sec.


How to deploy
-------------

Currently, there are 2 options to deploy a spyglass server

1) Build from source:

   First option is to build it from source. Spyglass uses haskell-stack build tool so after installing stack you can build it with "stack build" command. After building it you can install it by stack install.

2) Use Docker image:

   In addition to releases, we also provide docker images at https://hub.docker.com/r/huseyinyilmaz/spyglass/ . To use spyglass with docker first create a file named Dockerfile add following content to it:

::

   FROM huseyinyilmaz/spyglass:0.2.3
   # Add configuration to image.
   ADD config.yaml /etc/spyglass/config.yaml


Than create a config file on the same directory as default file and name id config.yaml. You can find the sample a sample configuration file here: https://github.com/huseyinyilmaz/spyglass/blob/master/configuration/config.yaml

At this point you should have a configuration to build your docker image. Run following command to build and run it:

::
   # Build your image:
   $ docker build . --tag=samplespyglass
   # Run your image
   $ docker run --rm  -ti -p 8080:8080 samplespyglass


How to populate server
----------------------

::

   # Upload collection/version. if version is not provided just use next integer
   POST /<collection>/ [{term: string, object: string}]
   {success: true}
   # Search for given keyword
   GET /<collection>/?term=<keywords>
   [<object>]
   # Delete collection
   DELETE /<collection>/

.. |build| image:: https://travis-ci.org/huseyinyilmaz/spyglass.png?branch=master
.. _build: https://travis-ci.org/huseyinyilmaz/spyglass
