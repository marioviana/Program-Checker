# Formal Verification
Project from Formal Verification curricular unit.

The verifier should comprehend a parser and a VC-Gen and manage the proof of these conditions by interacting with an automatic proofing tool, in this case the Z3.

Sample code to demonstrate the syntax of our language we define here (Simple Language).

```C
pre
  a>c;
  b>c;

program a {
  int aux = 4;
  int b = t;
  aux = 10;
  print aux;
  while(a > 3){
    inv a > c;
    aux = 10;
    print t;
  }
  print t;
}

post
  a==5;
```
