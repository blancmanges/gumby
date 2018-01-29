=============================
 Mr Gumby, a stupid slackbot
=============================

.. image:: https://travis-ci.org/kgadek/gumby.svg?branch=travis
    :target: https://travis-ci.org/kgadek/gumby

.. contents:: Table of Contents
   :depth: 2
   :backlinks: entry


Building & running
==================

.. code:: bash

    stack docker pull       # Preparations. Required once

    stack build             # Build.
    stack image container   # Build docker image.

    # Running
    docker run -e SLACK_SECRET_API_TOKEN=$YOUR_SECRET_TOKEN --rm -it kgadek-gumby:latest


Commit guidelines
=================

The git-commit "format"::

    <type>:<scope>: <subject>

    <body>

The ``<type>`` SHOULD be present. The ``<scope>`` is OPTIONAL. The ``<body>`` is RECOMMENDED.

.. note::
    Here we use RFC2119 & RFC8174 lingo.

``<type>``
----------

- ``feature``
- ``fix``
- ``docs``
- ``fmt`` - reformatting the code
- ``refactor``
- ``build`` - related to build & CI/CD systems