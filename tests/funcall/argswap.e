int g(int a, int b){
  print(a);
  print(b);
  return (b - a);
}
int f(int a, int b){
  print(a);
  print(b);
  return g(b, a);
}
int main(int argc, char* argv[]){
  return f(atoi(argv[1]),
           atoi(argv[2]));
}
