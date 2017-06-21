# Formal Verification
Project from Formal Verification curricular unit.

The verifier should comprehend a parser and a VC-Gen and manage the proof of these conditions by interacting with an automatic proofing tool, in this case the Z3.

Sample code to demonstrate the syntax of our language we define here (Simple Language).

```C
pre
  a>c;
  b>c;

program a (int x; int y;){
  int aux = 4;
  int c = y;
  aux = 10;
  print aux;
  try {
    while(x > 3) {
      inv x > c;
      aux = 10;
      print y;
    }
  }
  catch {
    print x;
  }
  print y;
}

postn
  a==5;
poste
  false; 
```
