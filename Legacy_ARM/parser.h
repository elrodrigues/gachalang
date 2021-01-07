#ifndef PARSER_INCLUDED
#define PARSER_INCLUDED
#include <regex>

using namespace std;

template <class T>
struct ParseResult;

struct Source {
  string str;
  string::iterator index;
  string::iterator end;

  Source(string st);
  ParseResult<string>* match(regex& regexp);
};

Source::Source(string st) {
  str = st;
  index = st.begin();
  end = st.end();
}

template <class T>
struct ParseResult {
  T value;
  Source* src;
  ParseResult(T& val, Source* s);
};

template <class T>
ParseResult<T>::ParseResult(T& val, Source* s) {
  value = val;
  src = s;
}

// Placed here to use ParseResult's constructor
// https://en.cppreference.com/w/cpp/regex
// http://www.cplusplus.com/reference/regex/ECMAScript/
// http://www.cplusplus.com/reference/regex/regex_constants/
// http://www.cplusplus.com/reference/regex/
// https://en.cppreference.com/w/cpp/regex/match_results
// https://en.cppreference.com/w/cpp/regex/match_results/position


////////////////////////////////////
////////// SOURCE MATCH ////////////
////////////////////////////////////
ParseResult<string>* Source::match(regex& regexp) {
  auto matches_beg = sregex_iterator(index, end, regexp); // bad idea
  auto matches_end = sregex_iterator();
  if(matches_beg != matches_end) {
    sregex_iterator i = matches_beg;
    smatch match = *i;
    string value = match.str();
    sregex_iterator nextIndex = ++i;
    if(nextIndex == matches_end) {
      return (new ParseResult<string>(value, new Source("")));
    }
    match = *nextIndex;
    string digestedStr = str.substr(match.position(0));
    return (new ParseResult<string>(value, new Source(digestedStr)));
  }
  else
    return nullptr;
}

// Parser Interface
template <class T>
class parser {
public:
  virtual ParseResult<T>* parse(Source& src) = 0;
};
#endif
