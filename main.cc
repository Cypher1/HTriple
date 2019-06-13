#include <iostream>
#include <fstream>
#include <sstream> //std::stringstream

#include "ast.h"

void runParser(std::string filename) {
  std::ifstream inFile;
  inFile.open(filename);

  std::stringstream strStream;
  strStream << inFile.rdbuf();
  std::string contents = strStream.str(); // Todo use the file+stream natively using memmap.

  Result<Tokens> toks = lex(filename, contents);

  std::cout << "Got " << toks.value.size() << "\n";

  for(const auto tok : toks.value) {
    std::cout << (int)tok.type << "@" << tok.loc.start << ":+" << tok.loc.length << "\n";
  }
  std::cout << "Errors:\n";
  for(const auto err : toks.errors) {
    std::cout << (int)err.type << "@" << err.loc.start << ":+" << err.loc.length << "\n";
    std::cout << err.msg << "\n";
  }
}

int main(int argc, char* argv[]) {
  for(int i=1; i<argc; ++i) {
    std::cout << i << ": " << argv[i] << "\n";
    runParser(argv[i]);
  }
  return 0;
}