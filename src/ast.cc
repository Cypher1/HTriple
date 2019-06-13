#include <iostream>
#include <vector>
#include <string>
#include "ast.h"

const std::string whiteSpace = " \t\n\r";
const std::string numberChar = "0123456789.";
const std::string operatorChar = "-+&#@<>[]^~∆%•|=÷×°$\\/*:?!,.;";
const std::string symbolChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";

const std::vector<std::pair<std::string, TokenType>> matchToken = {
  {"(", TokenType::OpenParen},
  {")", TokenType::CloseParen},
  {"{", TokenType::OpenBrace},
  {"}", TokenType::CloseBrace},
  {"'", TokenType::SingleQuote},
  {"\"", TokenType::DoubleQuote},
  {"`", TokenType::BackQuote},
  {"-|", TokenType::PreCond},
  {"|-", TokenType::PostCond},
  {".", TokenType::Dot},
  {"=", TokenType::Definition}
};

int matchesFrom(const std::string chars, const std::string content) {
  Offset length = 0;
  while(length < content.size()) {
    if(chars.find(content[length]) == std::string::npos) {
      break;
    }
    length++;
  }
  return length;
}

std::pair<TokenType, Offset> chooseTok(std::string content, Messages& msgs) {
  // TODO: use string views.
  for(const auto sym : matchToken) {
    const auto& tokS = sym.first;
    if(content.substr(0, tokS.size()) == tokS) {
      return {sym.second, tokS.size()};
    }
  }
  if(Offset length = matchesFrom(numberChar, content)) {
    return {TokenType::NumberLiteral, length};
  }
  if(Offset length = matchesFrom(symbolChar, content)) {
    return {TokenType::Symbol, length};
  }
  if(Offset length = matchesFrom(operatorChar, content)) {
    return {TokenType::Operator, length};
  }
  Message msg = {
    MessageType::Error,
    "Unexpected character '" + content.substr(0,1) + "'",
    {0, 0, "ha"}
  };
  msgs.push_back(msg);

  return {TokenType::Error, 1};
}

void consumeWhiteSpace(Position& loc, const std::string content) {
  for(;loc < content.size(); loc++) {
    char cur = content[loc];
    if(whiteSpace.find(cur) != std::string::npos) {
      continue;
    }
    if(cur == '/' && loc+1 < content.size() && content[loc+1] == '*') {
      loc++;
      for(loc++; loc < content.size(); loc++) {
        if(content[loc] == '/' && content[loc-1] == '*') break;
      }
      loc--;
    }
    if(cur == '/' && loc+1 < content.size() && content[loc+1] == '/') {
      loc++;
      for(loc++; loc < content.size(); loc++) {
        if(content[loc] == '\n') break;
      }
      loc--;
    }
    break;
  }
}

Result<Tokens> lex(std::string content, std::string filename) {
  Tokens toks;
  Messages msgs;

  Position loc = 0;
  consumeWhiteSpace(loc, content);
  while(loc < content.size()) {
    std::pair<TokenType, Offset> next = chooseTok(content.substr(loc), msgs);
    TokenType type = next.first;
    Offset length = next.second;
    Token tok = {type, {loc, length, filename}};
    toks.insert(toks.begin(), tok);
    loc += length;
    consumeWhiteSpace(loc, content);
  }
  return {toks, msgs};
}

std::vector<Tree<Token>> toAst(Tokens& toks, Messages& msgs) {
  std::vector<Tree<Token>> children;
  while(toks.size()) {
    // Matching brackets?
    Token curr = toks.back();
    toks.pop_back();
    Tree<Token> child = {curr, {}};
    children.push_back(child);
  }
  return children;
}

Result<Tree<Token>> ast(Result<Tokens> toks, const std::string filename) {
  auto msgs = toks.msgs;
  Tree<Token> root = {{TokenType::Symbol, {0, 0, filename}}, toAst(toks.value, msgs)};

  return {root, msgs};
}
