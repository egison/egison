==================================
Migration Guide for the New Syntax
==================================

Automated translation with ``egison-translate``
===============================================

We have a tool to help users convert old Egison programs into the new syntax.
``egison-translate`` automatically translates Egison programs in the old S-expression syntax into the new Haskell-like syntax.
It is not released but can be built from the Egison source as follows.

::

    $ git clone git@github.com:egison/egison.git
    $ cd egison
    $ stack init
    $ stack build
    $ stack exec -- egison-translate name-of-file-you-want-to-convert.segi


Changes in function names
=========================

Apart from the changes in the syntax itself, some of the library/built-in variable names have also been renamed to match the new syntax.

Naming convention
-----------------

The following describes the changes in the naming rule of variables.
The table shows the example of such changes and the detailed description is given under the table.
Some of these changes are supported in ``egison-translate`` but others are not, so the users might need to manually modify the program to complete the translation, at least for now.

+----+----------------+---------------+---------------------------------+
|    | Old name       | New name      | Handled by ``egison-translate`` |
+----+----------------+---------------+---------------------------------+
| 1  | ``take-while`` | ``takeWhile`` | Yes                             |
+----+----------------+---------------+---------------------------------+
| 2  | ``even?``      | ``isEven``    | No                              |
+----+----------------+---------------+---------------------------------+
| 3  | ``member?``    | ``member``    | No                              |
+----+----------------+---------------+---------------------------------+
| 4  | ``delete/m``   | ``deleteAs``  | Yes                             |
+----+----------------+---------------+---------------------------------+

1. Names connected with hyphens ``-`` are converted into camelCase.
2. Unary function names that ended with a question mark ``?`` is prefixed with ``is``.
3. Non-unary function names that ended with a question mark ``foo?`` now omits the last question mark.
4. Function names of the form ``foo/m`` are renamed as ``fooAs``. For instance, ``delete/m`` became ``deleteAs``.


Individual changes
------------------

Also, some of the Lisp-inspired names have been renamed into Haskell-like names as shown in the following table.
These changes are supported in ``egison-translate``.

+----------+----------+
| Old name | New name |
+----------+----------+
| ``car``  | ``head`` |
+----------+----------+
| ``cdr``  | ``tail`` |
+----------+----------+
| ``rac``  | ``last`` |
+----------+----------+
| ``rdc``  | ``init`` |
+----------+----------+
