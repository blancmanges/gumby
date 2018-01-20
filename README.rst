[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fkgadek%2Fgumby.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fkgadek%2Fgumby?ref=badge_shield)

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


## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fkgadek%2Fgumby.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fkgadek%2Fgumby?ref=badge_large)