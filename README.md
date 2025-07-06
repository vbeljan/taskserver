taskserver
=====

Test task for CreativeSoftware

Performing a POST request towards the root path (i. e. http://localhost:8080/) will accept a json tasks list as described in the task specification and will reply with the tasks sorted.
A POST request towards the /script path will instead provide the sorted tasks in form of a bash script in a json format as in the following example:
`{
    "script": "#/usr/bin/env bash;echo 'Hello World'"
    }`

Build
-----

    $ rebar3 compile

Test
-----

Uses katt to test various API responses. Add or modify an .apib file in /priv and use it in a test in the suite.
Run the tests with:

    $ rebar3 ct
