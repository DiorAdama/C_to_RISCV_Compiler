int g(int a,int b){
  return (b - a);
}
int f(int a, int b){
  return g(b, a);
}
int main(int x, int y){
  return f(x, y);
}
