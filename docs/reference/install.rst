============
Installation
============

There are two ways to install Egison: installing with package manager or installing from Haskell Platform.
The former method is available for only Linux and Mac users, while the latter is available for every user.

+----------+-------------------------------+--------------------------------+
|          | Install with package manager  | Install from Haskell Platform  |
+----------+-------------------------------+--------------------------------+
| Linux    | O (:ref:`yum` or :ref:`dpkg`) | O                              |
+----------+-------------------------------+--------------------------------+
| MacOS    | O (:ref:`homebrew`)           | O                              |
+----------+-------------------------------+--------------------------------+
| Windows  | X                             | O                              |
+----------+-------------------------------+--------------------------------+


Install with Package Manager
============================

.. _yum:

``yum``
-------

::

   $ sudo yum install https://git.io/egison.x86_64.rpm https://git.io/egison-tutorial.x86_64.rpm
   $ wget https://git.io/egison.x86_64.deb https://git.io/egison-tutorial.x86_64.deb

.. _dpkg:

``dpkg``
--------

::

   $ sudo dpkg -i ./egison*.deb

.. _homebrew:

Homebrew
--------

::

   $ brew update
   $ brew tap egison/egison
   $ brew install egison egison-tutorial

Install from Haskell Platform
=============================

1. Install Haskell Platform
---------------------------

To install Egison, you need to install `Haskell Platform <https://www.haskell.org/platform/>`_.
This is because Egison is implemented in Haskell and distributed as a `Hackage <https://hackage.haskell.org/>`_ package.

If you use ``apt-get``, execute the following commands.

::

   $ sudo apt-get update
   $ sudo apt-get install haskell-platform libncurses5-dev


Otherwise, download an installer from `here <https://www.haskell.org/platform/>`_.

2. Install Egison via Hackage
-----------------------------

After you installed Haskell Platform, perform the following commands in the terminal.

::

   $ cabal update
   $ cabal install egison egison-tutorial
   ...
   Installing executable(s) in /home/xxx/.cabal/bin
   Registering egison-X.X.X...

When the installation is finished, there will be a message that tells the location of the installed binary.
Add the path to the ``$PATH`` vairable so that your shell can find the ``egison`` command.
For example, if you are using bash, run the following commands.

::

   $ echo "PATH=\$PATH:/home/xxx/.cabal/bin" >> ~/.bashrc
   $ source ~/.bashrc
