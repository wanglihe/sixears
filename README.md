# conftest
a sip conference tester written by erlang

# see develop
Now it is developing in branch "develop" and no any useful code merged to master,
so switch to the "develop" to find the newest code.

# dep
After compare some sip frameworks (yxa, nksip, processone sip), I choose processone sip
as sip codec, because it is small. But there is no document about how to use it,
I only can use esip codec. (Does you know how to use dialog, help me).

I use rebar and erlang-p1-sip on debian, install them, then you can run "rebar compile".

# ref
rfc 3725 (3pcc)

# run
Run "rebar compile" at top level to compile code, then "cd ebin" and run sixears.
The sixears need only one arg, that is the name of script(config file), example:

./sixears ../example/session\_create\_destroy.conf
