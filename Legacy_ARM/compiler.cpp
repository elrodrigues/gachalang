#include <iostream>
#include "AST.h"
#include "parser.h"

using namespace std;

int main() {
  cout << "Hello, world!" << endl;
  AST* expA = new Add(new Id("x"), new Id("y"));
  AST* expB = new Add(new Id("x"), new Id("y"));
  cout << expA->equals(expB) << endl;
  delete expA;
  delete expB;

  // Need to define vector first and then create call
  // format is $_"callee"
  vector<AST*> $_factorial = {new Subtract(new Id("n"), new Integer(1))};
  expA = new Call("factorial", $_factorial);
  delete expA;
}
