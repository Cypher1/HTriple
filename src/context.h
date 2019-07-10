#pragma once
#ifndef CONTEXT_H
#define CONTEXT_H

#include "context.h"
#include <string>

#include "util.h"

class Context {
  Messages &msgs;
  PassStep step;
  PassStep final;

public:
  const std::string &content;
  const std::string &filename;

  Context(Context &&ctx) = delete; // Disable move.
  Context(const Context &ctx) = delete; // Disable copy.

  Context(Messages &msgs, const std::string &content,
          const std::string &filename, PassStep step, PassStep final)
      : msgs{msgs}, step{step}, final{final}, content{content}, filename{filename} {}

  void startStep(PassStep start_step);
  PassStep getStep();
  void msg(Location loc, MessageType level, std::string msg_txt);

  bool done();

  Messages getMsgs() const;

  std::string getStringAt(const Location &loc) const;
};

#endif // #ifndef CONTEXT_H