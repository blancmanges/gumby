=============================
 Mr Gumby, a stupid slackbot
=============================

.. image:: https://travis-ci.org/kgadek/gumby.svg?branch=master
    :target: https://travis-ci.org/kgadek/gumby

.. contents:: Table of Contents
   :depth: 2
   :backlinks: entry



Building
========

The usual ``cargo build`` is fine. If you want docker image with gumby, run
``make image`` - this shall build fairly minimal Alpine-based container.


Commit guidelines
=================

The git-commit "format"::

    <type>:<scope>: <subject>

    <body>

The ``<type>`` SHOULD be present. The ``<scope>`` is OPTIONAL. The ``<body>`` is RECOMMENDED.

.. note::
    The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
    NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED",
    "MAY", and "OPTIONAL" in this document are to be interpreted as
    described in BCP 14 [RFC2119] [RFC8174] when, and only when, they
    appear in all capitals, as shown here.

``<type>``
----------

- ``feature``
- ``fix``
- ``docs``
- ``fmt`` - reformatting the code
- ``refactor``
- ``build`` - related to build & CI/CD systems
- ``meta`` - related to repository itself (e.g. .gitignore changes)