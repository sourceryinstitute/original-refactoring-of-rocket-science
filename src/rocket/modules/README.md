Bread crumbs
============

Objective
---------
To start clarifying the flow of information by narrowing each
procedure's variable/parameter access to only those variables/parameters.
theprocedure uses.

* Feature: `only` clause
* Benefits:
  - Documentation: show what comes from where,
  - Avoid polluting the namespace,
  - Prevent certain types of errors:
    - compile-time: name clashes
    - runtime: mistaken use of a variable due to a typo
