#include <iostream>
#include <stdlib.h>
#include <string>
#include <sstream>

typedef struct {
  int32_t x;
  int32_t y;
} struct_x_i32_y_i32;

namespace std{
template <typename T>
string to_string(const T& t){
  stringstream out;
  out << t;
  return out.str();
}
string to_string(const struct_x_i32_y_i32& t){
  stringstream out;
  out << "{x=" << to_string(t.x) << ", y=" << to_string(t.y) << "}";
  return out.str();
}
string to_string(const bool& t){
  return t ? "true" : "false";
}
}

int main(int argc, char* argv[]) {
  const struct_x_i32_y_i32 examples_struct_construction_ob = {.x=3, .y=5};
  std::cout << ((std::to_string("ob=")+std::to_string(examples_struct_construction_ob))) << "\n";
  return 0;
}
