Spyglass autocomplete server
============================

|build|_

Spyglass is a autocomplete server that stores list of search term - content pairs and uses that data to provide an autocomplete backend. Spyglass is not a smart search engine. But it is really fast autocomplete engine. A small server should be able to handle around 15.000 req/sec.

Spyglass also has ability to update its content using a remote rest API. So once server is set up, server will be update itself in desired intervals. If remote api is down, spyglass will still serve existing data and keep trying to update its data in the future.

How to deploy
-------------

Currently, there are 2 options to deploy a spyglass server

1) Build from source:

   First option is to build it from source. Spyglass uses haskell-stack build tool so after installing stack you can build it with "stack build" command. After building it you can install it by stack install.

2) Download binary from github:
   https://github.com/huseyinyilmaz/spyglass/releases

2) Use Docker image:
   In addition to releases, we also provide docker images at https://hub.docker.com/r/huseyinyilmaz/spyglass/ . To use spyglass with docker, first create a file named Dockerfile and, add following content into the file:

::

   FROM huseyinyilmaz/spyglass:0.2.3
   # Add configuration to image.
   ADD config.yaml /etc/spyglass/config.yaml


Than create a config file on the same directory as default file and name it config.yaml. You can find the sample a sample configuration file here: https://github.com/huseyinyilmaz/spyglass/blob/master/configuration/config.yaml

Lastly, run following command to build and run the image:

::
   # Build your image:
   $ docker build . --tag=samplespyglass
   # Run your image
   $ docker run --rm  -ti -p 8080:8080 samplespyglass


How to populate server
----------------------

There are 2 ways to populate server with data.

Populate with data push
-----------------------

In order to push data into the server first you need an authentication information on configuration. Ensure that you have a user info on your configuration like this:

::

   users:
   - username: admin
     password: password


After you have auth info, you can push in 2 different ways.

First way is you can push data directly into the server with a request like this. Spyglass uses basic auth to authenticate requests.

::

    curl -X POST localhost:8080/test \
         -d '{"values": [{"term": "test1", "value": "value1"}, {"term": "test2", "value": "value2"}]}' \
         -u admin:password


After running this, endpoint will be avilable at "localhost:8080/test". One downside of this method is your data will not be updated until you push a new version. So you need to invalidate the data yourself.

Second way of pushing data is to push an endpoint url so spyglass server can pull the data.

Lets say you have an endpoint at localhost:8181/data that returns following body to get requests:

::

    {"values": [{"term": "test1", "value": "value1"},
                {"term": "test2", "value": "value2"}]}

You can send following request to create a new endpoint:

::

    curl -X POST localhost:8080/test \
         -d '{"endpoint": "http://localhost:8181/data", "timeout": 300}' \
         -u admin:password

This request will create a new endpoint at localhost:8080/test and endpoint will be populated and refresed in every 5 minutes from localhost:8181/data.

Pull
----

Second and best way of populating the endpoint is by providing data url on the configuration so whenever server is started, it will started with a data. In other words, if server starts without a problem, endpoints are guaranteed to exist.

In order to create endpoints with configuration following should be in the configuration file:

::

   endpoints:
     - path: "test"
       url: "http://127.0.0.1:8181/data"
       timeout: 300


.. |build| image:: https://travis-ci.org/huseyinyilmaz/spyglass.png?branch=master
.. _build: https://travis-ci.org/huseyinyilmaz/spyglass
