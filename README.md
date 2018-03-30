# [![Build Status](https://travis-ci.org/fenollp/quickcheck_test_web_service_statem.svg?branch=master)](https://travis-ci.org/fenollp/quickcheck_test_web_service_statem) [quickcheck_test_web_service_statem](https://github.com/fenollp/quickcheck_test_web_service_statem)
Use QuickCheck/PropEr to test a web service, with statem

Minimal implementation of https://github.com/ThomasArts/tickets
* With rebar3
* With PropEr
* With Quviqs Quickcheck (free limited version from Erlang Factory SF 2016, not included)

```
148 quickcheck_test_web_service_statem.git master (20.3) ∀ r as test shell
===> Verifying dependencies...
===> Compiling mylib
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
1> prop_misc:t1().
[-8,6,-17,-3,-9]
[-8,6,-17]
[6,-17]
[-17]
[]
ok
2> prop_misc:t2().
[9,-2,31,11,21]
[0,-2,31,11,21]
[0,0,31,11,21]
[0,0,0,11,21]
[0,0,0,0,21]
[0,0,0,0,0]
ok
3> prop_misc:t3().
[-8,3,-2,4]
[-8,3]
[3]
[]
ok
4>
```

```
1 quickcheck_test_web_service_statem.git master (20.3) ∀ r as test proper -m prop_misc
===> Verifying dependencies...
===> Compiling mylib
===> Testing prop_misc:prop_sets_union_is_commutative()
......!
Failed: After 7 test(s).
{{set,2,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[-1],[],[],[],[],[],[1],[],[],[],[]}}},{set,2,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[-33],[],[],[],[-5],[],[],[],[],[],[]}}}}

SetA = [-1,1]
SetB = [-33,-5]

Shrinking ....(4 time(s))
{{set,2,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[0],[],[],[],[],[-1],[],[],[],[],[],[],[],[],[],[]}}},{set,2,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[0,-16],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}}

SetA = [0,-1]
SetB = [-16,0]
===>
0/1 properties passed, 1 failed
===> Failed test cases:
  prop_misc:prop_sets_union_is_commutative() -> false
```

```
148 quickcheck_test_web_service_statem.git master (20.3) ∀ r as test proper -m prop_misc
===> Verifying dependencies...
===> Compiling mylib
===> Testing prop_misc:prop_sets_union_is_commutative()
....................!
Failed: After 21 test(s).
{{set,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[-28],[],[],[]}}},{set,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[4],[],[],[]}}}}

SetA = [-28]
SetB = [4]

Shrinking (0 time(s))
{{set,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[-28],[],[],[]}}},{set,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[4],[],[],[]}}}}
===>
0/1 properties passed, 1 failed
===> Failed test cases:
  prop_misc:prop_sets_union_is_commutative() -> false
```
