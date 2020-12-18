#include <iostream>
#include <string>
#include <sstream>
namespace std{
template <typename T>
string to_string(const T& t){
  stringstream out;
  out << t;
  return out.str();
}
string to_string(const bool& t){
  return t ? "true" : "false";
}
}
struct x_I32y_I32;
struct x_I32y_I32{int32_t x; int32_t y; x_I32y_I32(const int32_t x, const int32_t y): x{x}, y{y} {}};
std::ostream& operator<<(std::ostream& os, const struct x_I32y_I32& t) { os << "struct(x=" << t.x << ", y=" << t.y << ")"; return os; }

int main(int argc, char* argv[]) {
  const auto examples_struct_construction_ob = x_I32y_I32(3, 5);
  std::cout << ((std::to_string((std::to_string("ob=")+std::to_string(examples_struct_construction_ob)))+std::to_string("\n")));
  return 0;
}