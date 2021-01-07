#ifndef AST_INCLUDED
#define AST_INCLUDED
#include <vector>
using namespace std;

enum class Nodes {
  INTEGER, ID, NOT, EQUAL, NOTEQUAL,
  ADD, SUB, MUL, DIV,
  CALL, RET, BLK, FDEF, VAR, ASGN,
  IF, WHL
};

class AST {
public:
  virtual bool equals(AST* other) const = 0;
  enum Nodes getType() const;
  virtual ~AST(){}
protected:
  enum Nodes type;
};

enum Nodes AST::getType() const {
  return type;
}
////////////////////////////////////
//////////// Integer ///////////////
////////////////////////////////////
struct Integer : public AST {
  int value;
  Integer(int val);

  bool equals(AST* other) const;
};

Integer::Integer(int val) {
  value = val;
  type = Nodes::INTEGER;
}

bool Integer::equals(AST* other) const {
  bool eq = false;
  switch(other->getType()) {
    case Nodes::INTEGER :
    {
      Integer* new_oth = static_cast<Integer*>(other);
      eq = (value == new_oth->value);
    }
    break;

    default :
    eq = false;
  }
  return eq;
}
////////////////////////////////////
/////////// Identifier /////////////
////////////////////////////////////
struct Id : public AST {
  string value;
  Id(string val);

  bool equals(AST* other) const;
};

Id::Id(string val) {
  value = val;
  type = Nodes::ID;
}

bool Id::equals(AST* other) const {
  if(other->getType() == Nodes::ID) {
    Id* new_oth = static_cast<Id*>(other);
    return (value.compare(new_oth->value) == 0);
  }
  else
    return false;
}
// OPERATOR NODES

////////////////////////////////////
////////////// NOT /////////////////
////////////////////////////////////
struct Not : public AST {
  AST* term;
  Not(AST* t);
  ~Not();

  bool equals(AST* other) const;
};

Not::Not(AST* t) {
  term = t;
  type = Nodes::NOT;
}

bool Not::equals(AST* other) const {
    if(other->getType() == Nodes::NOT) {
      Not* new_oth = static_cast<Not*>(other);
      return (term->equals(new_oth->term));
    }
    else
      return false;
}

Not::~Not() {
  delete term;
}

struct BinOp {
  AST* left;
  AST* right;
  ~BinOp(){
    delete left;
    delete right;
  }
};
////////////////////////////////////
//////////// Equal == //////////////
////////////////////////////////////
struct Equal : public AST {
  BinOp bop;
  Equal(AST* l, AST* r);

  bool equals(AST* other) const;
};

Equal::Equal(AST* l, AST* r) {
  bop.left = l;
  bop.right = r;
  type = Nodes::EQUAL;
}

bool Equal::equals(AST* other) const {
  if(other->getType() == Nodes::EQUAL) {
    Equal* new_oth = static_cast<Equal*>(other);
    return ((bop.left->equals(new_oth->bop.left)) &&
      (bop.right->equals(new_oth->bop.right)));
  }
  else
    return false;
}
////////////////////////////////////
/////////// NotEqual != ////////////
////////////////////////////////////
struct NotEqual : public AST {
  BinOp bop;
  NotEqual(AST* l, AST* r);

  bool equals(AST* other) const;
};

NotEqual::NotEqual(AST* l, AST* r) {
  bop.left = l;
  bop.right = r;
  type = Nodes::NOTEQUAL;
}

bool NotEqual::equals(AST* other) const {
  if(other->getType() == Nodes::NOTEQUAL) {
    NotEqual* new_oth = static_cast<NotEqual*>(other);
    return ((bop.left->equals(new_oth->bop.left)) &&
      (bop.right->equals(new_oth->bop.right)));
  }
  else
    return false;
}

////////////////////////////////////
///////////// Add + ////////////////
////////////////////////////////////
struct Add : public AST {
  BinOp bop;
  Add(AST* l, AST* r);

  bool equals(AST* other) const;
};

Add::Add(AST* l, AST* r) {
  bop.left = l;
  bop.right = r;
  type = Nodes::ADD;
}

bool Add::equals(AST* other) const {
  if(other->getType() == Nodes::ADD) {
    Add* new_oth = static_cast<Add*>(other);
    return ((bop.left->equals(new_oth->bop.left)) &&
      (bop.right->equals(new_oth->bop.right)));
  }
  else
    return false;
}
////////////////////////////////////
/////////// Subtract - /////////////
////////////////////////////////////
struct Subtract : public AST {
  BinOp bop;
  Subtract(AST* l, AST* r);

  bool equals(AST* other) const;
};

Subtract::Subtract(AST* l, AST* r) {
  bop.left = l;
  bop.right = r;
  type = Nodes::SUB;
}

bool Subtract::equals(AST* other) const {
  if(other->getType() == Nodes::SUB) {
    Subtract* new_oth = static_cast<Subtract*>(other);
    return ((bop.left->equals(new_oth->bop.left)) &&
      (bop.right->equals(new_oth->bop.right)));
  }
  else
    return false;
}
////////////////////////////////////
//////// Multiplication * //////////
////////////////////////////////////
struct Multiply : public AST {
  BinOp bop;
  Multiply(AST* l, AST* r);

  bool equals(AST* other) const;
};

Multiply::Multiply(AST* l, AST* r) {
  bop.left = l;
  bop.right = r;
  type = Nodes::MUL;
}

bool Multiply::equals(AST* other) const {
  if(other->getType() == Nodes::MUL) {
    Multiply* new_oth = static_cast<Multiply*>(other);
    return ((bop.left->equals(new_oth->bop.left)) &&
      (bop.right->equals(new_oth->bop.right)));
  }
  else
    return false;
}
////////////////////////////////////
/////////// Division / /////////////
////////////////////////////////////
struct Divide : public AST {
  BinOp bop;
  Divide(AST* l, AST* r);

  bool equals(AST* other) const;
};

Divide::Divide(AST* l, AST* r) {
  bop.left = l;
  bop.right = r;
  type = Nodes::DIV;
}

bool Divide::equals(AST* other) const {
  if(other->getType() == Nodes::MUL) {
    Multiply* new_oth = static_cast<Multiply*>(other);
    return ((bop.left->equals(new_oth->bop.left)) &&
      (bop.right->equals(new_oth->bop.right)));
  }
  else
    return false;
}
////////////////////////////////////
////////////// CALL ////////////////
////////////////////////////////////
struct Call : public AST {
  string callee;
  vector<AST*> args;
  Call(string c, vector<AST*>& ag);
  ~Call();

  bool equals(AST* other) const;
};

Call::Call(string c, vector<AST*>& ag) {
  callee = c;
  args = ag;
  type = Nodes::CALL;
}

bool Call::equals(AST* other) const {
  if(other->getType() == Nodes::CALL) {
    Call* new_oth = static_cast<Call*>(other);
    return (callee == new_oth->callee &&
      args == new_oth->args);
  }
  else
    return false;
}

Call::~Call() {
  size_t sz = args.size();
  for(size_t i = 0; i < sz; i++) {
    delete args[i];
  }
}

////////////////////////////////////
///////////// RETURN ///////////////
////////////////////////////////////
struct Return : public AST {
  AST* term;
  Return(AST* t);
  ~Return();

  bool equals(AST* other) const;
};

Return::Return(AST* t) {
  term = t;
  type = Nodes::RET;
}

Return::~Return(){
  delete term;
}

bool Return::equals(AST* other) const {
  if(other->getType() == Nodes::RET) {
    Return* new_oth = static_cast<Return*>(other);
    return term->equals(new_oth->term);
  }
  else
    return false;
}

////////////////////////////////////
////////////// BLOCK ///////////////
////////////////////////////////////
// Can collapse vector statements into one class
struct Block : public AST {
  vector<AST*> statements;
  Block(vector<AST*>& st);
  ~Block();

  bool equals(AST* other) const;
};

Block::Block(vector<AST*>& st) {
  statements = st;
  type = Nodes::BLK;
}

Block::~Block() {
  size_t sz = statements.size();
  for(size_t i = 0; i < sz; i++) {
    delete statements[i];
  }
}

bool Block::equals(AST* other) const {
  if(other->getType() == Nodes::BLK) {
    Block* new_oth = static_cast<Block*>(other);
    return (statements == new_oth->statements);
  }
  else
    return false;
}

////////////////////////////////////
/////////////// IF /////////////////
////////////////////////////////////
struct If : public AST {
  AST* conditional;
  AST* consequence;
  AST* alternative;

  If(AST* cond, AST* cons, AST* alt);
  ~If();
  bool equals(AST* other) const;
};

If::If(AST* cond, AST* cons, AST* alt) {
  conditional = cond;
  consequence = cons;
  alternative = alt;
  type = Nodes::IF;
}

If::~If() {
  delete conditional;
  delete consequence;
  delete alternative;
}

bool If::equals(AST* other) const {
  if(other->getType() == Nodes::IF) {
    If* new_oth = static_cast<If*>(other);
    return ((conditional == new_oth->conditional) &&
      (consequence == new_oth->consequence) &&
      (alternative == new_oth->alternative)
    );
  }
  else
    return false;
}

////////////////////////////////////
/////// FunctionDefinition /////////
////////////////////////////////////
struct FunctionDefinition : public AST {
  string name;
  vector<AST*> args;
  AST* body;

  FunctionDefinition(string n, vector<AST*>& ag, AST* bod);
  ~FunctionDefinition();
  bool equals(AST* other) const;
};

FunctionDefinition::FunctionDefinition(string n, vector<AST*>& ag, AST* bod) {
  name = n;
  args = ag;
  body = bod;
  type = Nodes::FDEF;
}

FunctionDefinition::~FunctionDefinition() {
  size_t sz = args.size();
  delete body;
  for(size_t i = 0; i < sz; i++)
    delete args[i];
}

bool FunctionDefinition::equals(AST* other) const {
  if(other->getType() == Nodes::FDEF) {
    FunctionDefinition* new_oth = static_cast<FunctionDefinition*>(other);
    return ((name == new_oth->name) &&
      (args == new_oth->args) &&
      (body->equals(new_oth->body)));
  }
  else
    return false;
}

////////////////////////////////////
/////// VAR (VARIABLE DEC) /////////
////////////////////////////////////
struct Var : public AST {
  string name;
  AST* value;

  Var(string n, AST* val);
  ~Var();
  bool equals(AST* other) const;
};

Var::Var(string n, AST* val) {
  name = n;
  value = val;
  type = Nodes::VAR;
}

bool Var::equals(AST* other) const {
  if(other->getType() == Nodes::VAR) {
    Var* new_oth = static_cast<Var*>(other);
    return ((name == new_oth->name) &&
      (value->equals(new_oth->value)));
  }
  else
    return false;
}

Var::~Var() {
  // デュラララ！！！
  delete value;
}

////////////////////////////////////
//////////// ASSIGN ////////////////
////////////////////////////////////
struct Assign : public AST {
  string name;
  AST* value;

  Assign(string n, AST* val);
  ~Assign();
  bool equals(AST* other) const;
};

Assign::Assign(string n, AST* val) {
  name = n;
  value = val;
  type = Nodes::ASGN;
}

bool Assign::equals(AST* other) const {
  if(other->getType() == Nodes::ASGN) {
    Assign* new_oth = static_cast<Assign*>(other);
    return ((name == new_oth->name) &&
      (value->equals(new_oth->value)));
  }
  else
    return false;
}

Assign::~Assign() {
  delete value;
}

////////////////////////////////////
///////////// WHILE ////////////////
////////////////////////////////////
struct While : public AST {
  AST* conditional;
  AST* body;

  While(AST* cond, AST* bod);
  ~While();
  bool equals(AST* other) const;
};

While::While(AST* cond, AST* bod) {
  conditional = cond;
  body = bod;
  type = Nodes::WHL;
}

While::~While() {
  delete conditional;
  delete body;
}

bool While::equals(AST* other) const {
  if(other->getType() == Nodes::WHL) {
    While* new_oth = static_cast<While*>(other);
    return ((conditional->equals(new_oth->conditional)) &&
      (body->equals(new_oth->body)));
  }
  else
    return false;
}
#endif
